library(tidyverse)
dir.create("Plots", showWarnings = FALSE)

energy <- read.csv("Data/energydata_complete.csv")
energy$date <- as.POSIXct(energy$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
energy$NSM <- hour(energy$date) * 3600 + minute(energy$date) * 60 + second(energy$date)
energy$Day_of_week <- withr::with_locale(c(LC_TIME = "C"), factor(weekdays(energy$date)))
stopifnot(nrow(energy) == 19735, !anyNA(energy), !anyDuplicated(energy$date))

# Use the sampling algorithm from the authors' R version.
# Stratified sampling based on the level of Appliances (appliance energy consumption).
published_index <- withr::with_rng_version("3.3.0", {
  set.seed(1)
  caret::createDataPartition(energy$Appliances, p = 0.75, list = FALSE)})
published_index <- as.vector(published_index)

# Weekday indicators also encode weekend status. Use Monday as the reference.
model_data <- energy |>
  select(-c(date, rv1, rv2)) |>
  mutate(Day_of_week = fct_relevel(Day_of_week, "Monday"))
diagnostic_data <- model_data[published_index, ]
diagnostic_fit <- lm(Appliances ~ ., data = diagnostic_data)

test_data <- model_data[-published_index, ]
x_num <- diagnostic_data |> select(-c(Appliances, Day_of_week))
pca_fit <- prcomp(x_num, center = TRUE, scale. = TRUE)

Rmse_f <- function(y, yhat){sqrt(mean((y - yhat)^2))}
R2 <- function(y, yhat){1 - sum((y - yhat)^2) / sum((y - mean(y))^2)}
MAE <- function(y, yhat){mean(abs(y - yhat))}
MAPE <- function(y, yhat){mean(abs((y - yhat) / y))}

score <- function(actual, predicted) {
  c(RMSE = Rmse_f(actual, predicted), R2 = R2(actual, predicted),
    MAE = MAE(actual, predicted), MAPE = 100 * MAPE(actual, predicted))
}

baseline_test <- as_tibble_row(score(test_data$Appliances, predict(diagnostic_fit, test_data)))

pca_summary <- tibble(PC = seq_along(pca_fit$sdev),
                      variance = pca_fit$sdev^2) |>
  mutate(proportion = variance / sum(variance),
         cumulative = cumsum(proportion))
print(pca_summary, n = Inf)

# Show the five largest absolute loadings for each PC, keeping their signs.
pc_loadings <- as_tibble(pca_fit$rotation[, c("PC1", "PC2")], rownames = "Predictor") |>
  pivot_longer(c(PC1, PC2), names_to = "PC", values_to = "Loading") |>
  group_by(PC) |>
  slice_max(abs(Loading), n = 5, with_ties = FALSE) |>
  ungroup() |>
  arrange(PC, desc(abs(Loading)))
print(pc_loadings)

# 3 from screeplot, 5 from the table(>1 eigenvalue and 80% variance)
pcr_vars <- names(x_num)
pcr_data <- function(data, pca, k) {
  pcr_vars <- rownames(pca$rotation)
  pc <- predict(pca, newdata = data[, pcr_vars])
  bind_cols(select(data, -all_of(pcr_vars)),
            as_tibble(pc[, seq_len(k), drop = FALSE]))
}

# Fit the fixed PCR candidates using PCA from the full training split.
fit3 <- lm(Appliances ~ ., data = pcr_data(diagnostic_data, pca_fit, 3))
fit5 <- lm(Appliances ~ ., data = pcr_data(diagnostic_data, pca_fit, 5))
# Keep the numeric OLS terms and the PC terms for the VIF comparison.
ols_vif <- car::vif(diagnostic_fit)
pc_vif <- function(model) {
  values <- car::vif(model)
  print(values)
  tibble(Predictor = rownames(values), VIF = values[, "GVIF"]) |>
    filter(startsWith(Predictor, "PC"))
}

vif_plot_data <- bind_rows(
  "OLS" = tibble(Predictor = pcr_vars, VIF = ols_vif[pcr_vars, "GVIF"]),
  "PCR-3" = pc_vif(fit3),
  "PCR-5" = pc_vif(fit5),
  .id = "Model") |>
  mutate(Model = factor(Model, levels = c("OLS", "PCR-3", "PCR-5")))

vif_plot <- ggplot(vif_plot_data, aes(x = VIF, y = Predictor)) +
  geom_vline(xintercept = c(5, 10), linetype = "dashed", color = "grey65") +
  geom_point(size = 2.5, color = "steelblue4") +
  facet_wrap(~ Model, scales = "free_y", nrow = 1) +
  scale_x_log10(
    breaks = c(1, 5, 10, 50, 150),
    limits = c(0.95, NA)
  ) +
  labs(x = "Variance inflation factor (log scale)", y = NULL) +
  theme_minimal()
ggsave("Plots/03-vif-comparison.png", plot = vif_plot, width = 10, height = 7, dpi = 300, bg = "white")

# Evaluate both candidates on the published test split.
test_predictors <- test_data[, setdiff(names(diagnostic_data), "Appliances")]
pred3 <- predict(fit3, newdata = pcr_data(test_predictors, pca_fit, 3))
pred5 <- predict(fit5, newdata = pcr_data(test_predictors, pca_fit, 5))

pcr_comparison <- bind_rows("OLS" = baseline_test,
  "PCR-3" = as_tibble_row(score(test_data$Appliances, pred3)),
  "PCR-5" = as_tibble_row(score(test_data$Appliances, pred5)),
  .id = "Model")
# Compress only the outdoor temperature and humidity measurements.
outdoor_vars <- c("T6", "RH_6", "T_out", "RH_out", "Tdewpoint")
outdoor_pca <- prcomp(diagnostic_data[, outdoor_vars], center = TRUE, scale. = TRUE)
outdoor_summary <- tibble(PC = seq_along(outdoor_pca$sdev), variance = outdoor_pca$sdev^2) |>
  mutate(proportion = variance / sum(variance), cumulative = cumsum(proportion))
print(outdoor_summary)

p_outdoor_scree <- ggplot(outdoor_summary, aes(PC, variance)) +
  geom_line() +
  geom_point() +
  labs(title = "Scree plot of outdoor temperature and humidity", y = "Variance") +
  theme_minimal()
ggsave("Plots/03-outdoor-scree.png", plot = p_outdoor_scree, width = 8, height = 5, dpi = 300, bg = "white")

outdoor_train <- pcr_data(diagnostic_data, outdoor_pca, 2)
outdoor_test <- pcr_data(test_predictors, outdoor_pca, 2)
outdoor_fit <- lm(Appliances ~ ., data = outdoor_train)
outdoor_pred <- predict(outdoor_fit, newdata = outdoor_test)
outdoor_results <- as_tibble_row(score(test_data$Appliances, outdoor_pred)) |>
  mutate(Model = "Outdoor PCR-2", .before = 1)

# PCA on temperature and humidity in the eight indoor locations.
rooms <- c(1:5, 7:9)
indoor_vars <- as.vector(rbind(paste0("T", rooms), paste0("RH_", rooms)))
indoor_pca <- prcomp(diagnostic_data[indoor_vars], center = TRUE, scale. = TRUE)
indoor_summary <- tibble(PC = seq_along(indoor_pca$sdev), variance = indoor_pca$sdev^2) |>
  mutate(proportion = variance / sum(variance), cumulative = cumsum(proportion))
print(indoor_summary)
print(round(indoor_pca$rotation[, 1:2], 3))

p_indoor_scree <- ggplot(indoor_summary, aes(PC, variance)) +
  geom_line() +
  geom_point() +
  labs(title = "Scree plot of indoor temperature and humidity", y = "Variance") +
  theme_minimal()
ggsave("Plots/03-indoor-scree.png", plot = p_indoor_scree, width = 8, height = 5, dpi = 300, bg = "white")

indoor_train <- pcr_data(diagnostic_data, indoor_pca, 2)
indoor_test <- pcr_data(test_predictors, indoor_pca, 2)
indoor_fit <- lm(Appliances ~ ., data = indoor_train)
indoor_pred <- predict(indoor_fit, newdata = indoor_test)
indoor_result <- as_tibble_row(score(test_data$Appliances, indoor_pred)) |>
  mutate(Model = "Indoor PCR-2", .before = 1)
indoor_fit3 <- lm(Appliances ~ ., data = pcr_data(diagnostic_data, indoor_pca, 3))
indoor_pred3 <- predict(indoor_fit3, pcr_data(test_predictors, indoor_pca, 3))
indoor_result3 <- as_tibble_row(score(test_data$Appliances, indoor_pred3)) |>
  mutate(Model = "Indoor PCR-3", .before = 1)
print(bind_rows(pcr_comparison, indoor_result, indoor_result3, outdoor_results))
