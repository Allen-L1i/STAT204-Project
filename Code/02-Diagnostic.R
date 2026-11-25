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

p_residuals <- ggplot(diagnostic_data, aes(Appliances, resid(diagnostic_fit))) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0) +
  labs(x = "Appliance energy consumption", y = "Residuals") +
  theme_minimal()
ggsave("Plots/02-residuals-observed.png", plot = p_residuals, width = 8, height = 5, dpi = 300, bg = "white")


png("Plots/02-ols-diagnostics.png", width = 10, height = 8, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(diagnostic_fit, which = c(1, 2, 3, 5), ask = FALSE)
dev.off()

# MultiCollinearity Check - VIF & ScreePlot
rooms <- c(1:5, 7:9)
temperature <- paste0("T", rooms)
humidity <- paste0("RH_", rooms)
vif_values <- car::vif(diagnostic_fit)
indoor_vif <- vif_values[c(temperature, humidity), "GVIF"]
print(head(sort(indoor_vif, decreasing = TRUE), 5))

pair_summary <- function(vars) {
  correlations <- cor(diagnostic_data[vars])
  pairs <- correlations[upper.tri(correlations)]
  c(Min = min(pairs), Median = median(pairs), Max = max(pairs))
}
print(round(rbind(Temperature = pair_summary(temperature),
                  Humidity = pair_summary(humidity)), 2))

x_num <- diagnostic_data |> select(-c(Appliances, Day_of_week))
pca_fit <- prcomp(x_num, center = TRUE, scale. = TRUE)
p_scree <- ggplot(tibble(PC = seq_along(pca_fit$sdev), variance = pca_fit$sdev^2),
                  aes(PC, variance)) +
  geom_line() +
  geom_point() +
  labs(title = "Scree plot of standardized predictors", y = "Variance") +
  theme_minimal()
ggsave("Plots/02-predictor-scree.png", plot = p_scree, width = 8, height = 5, dpi = 300, bg = "white")

