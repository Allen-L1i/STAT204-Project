library(tidyverse)

energy <- read.csv("data/energydata_complete.csv")
energy$date <- strptime(as.character(energy$date), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
energy$date <- as.POSIXct(energy$date, tz = "UTC")

second_day <- function(x) {
  hour(x) * 3600 + minute(x) * 60 + second(x)
}
weekend_weekday <- function(x) {
  if (weekdays(x) %in% c("Saturday", "Sunday")) "Weekend" else "Weekday"
}
energy$NSM <- second_day(energy$date)
withr::with_locale(c(LC_TIME = "C"), {
  energy$WeekStatus <- as.factor(unlist(lapply(energy$date, weekend_weekday)))
  energy$Day_of_week <- as.factor(weekdays(energy$date))
})
stopifnot(nrow(energy) == 19735, !anyNA(energy), !anyDuplicated(energy$date))

# Use the sampling algorithm from the authors' R version.
# Stratified sampling based on the level of Appliances (appliance energy consumption).
published_index <- withr::with_rng_version("3.3.0", {
  set.seed(1)
  caret::createDataPartition(energy$Appliances, p = 0.75, list = FALSE)})
published_index <- as.vector(published_index)
train_data <- energy[published_index, ]
test_data <- energy[-published_index, ]
train_data2 <- subset(train_data, select = -c(rv1, rv2))
test_data2 <- subset(test_data, select = -c(rv1, rv2))

# The original dummies package is unavailable here (archived on CRAN). 
# Keep all levels of each factor.
indicator_matrix <- function(x) {
  result <- model.matrix(~ x - 1)
  colnames(result) <- levels(x)
  result
}
encode_calendar <- function(data) {
  data.frame(subset(data, select = -c(WeekStatus, Day_of_week)),
             indicator_matrix(data$WeekStatus), indicator_matrix(data$Day_of_week))
}
new_train_data <- encode_calendar(train_data2)
new_test_data <- encode_calendar(test_data2)
stopifnot(ncol(new_train_data) == 37, ncol(new_test_data) == 37)
training <- new_train_data[, 2:37]
testing <- new_test_data[, 2:37]

# Re-create the folds 3 times, following the authors' evaluation procedure.
fit_control <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 3,
                                   verboseIter = TRUE, returnResamp = "all")
# Fix the CV seed for this standalone reproduction of the LM.
set.seed(1)
model <- caret::train(Appliances ~ ., data = training, method = "lm",
                      trControl = fit_control, metric = "RMSE")
print(model)

Rmse_f <- function(y, yhat){sqrt(mean((y - yhat)^2))}
R2 <- function(y, yhat){1 - sum((y - yhat)^2) / sum((y - mean(y))^2)}
MAE <- function(y, yhat){mean(abs(y - yhat))}
MAPE <- function(y, yhat){mean(abs((y - yhat) / y))}

score <- function(actual, predicted) {
  c(RMSE = Rmse_f(actual, predicted), R2 = R2(actual, predicted),
    MAE = MAE(actual, predicted), MAPE = 100 * MAPE(actual, predicted))
}

baseline <- rbind(Training = score(training$Appliances, predict(model, training)),
                  Testing = score(testing$Appliances, predict(model, testing)))
published <- tribble(
  ~Split,     ~RMSE, ~R2,  ~MAE,  ~MAPE,
  "Training", 93.21, 0.18, 53.13, 61.32,
  "Testing",  93.18, 0.16, 51.97, 59.93
)
baseline_tbl <- as_tibble(baseline, rownames = "Split")
comparison <- baseline_tbl |>
  pivot_longer(-Split, names_to = "Metric", values_to = "Reproduced") |>
  left_join(pivot_longer(published, -Split, names_to = "Metric", values_to = "Published"),
    by = c("Split", "Metric")) |>
  select(Split, Metric, Published, Reproduced) |>
  mutate(Difference = Reproduced - Published)
print(comparison)

# Test the stability of the result
split_seeds <- 1:30

model_data <- encode_calendar(subset(energy, select = -c(rv1, rv2)))[, 2:37]
baseline_test <- as_tibble_row(baseline["Testing", ])
evaluate_split <- function(seed) {
  if (seed == 1) return(baseline_test)
  index <- withr::with_seed(seed, as.vector(caret::createDataPartition(
    energy$Appliances, p = 0.75, list = FALSE)), .rng_sample_kind = "Rounding")
  train <- model_data[index, ]
  test <- model_data[-index, ]
  fit <- lm(Appliances ~ ., data = train)
  as_tibble_row(score(test$Appliances, predict(fit, test)))
}

by_seed <- tibble(Seed = split_seeds) |>
  mutate(metrics = map(Seed, evaluate_split)) |>
  unnest(metrics)

scores <- by_seed |>
  pivot_longer(-Seed, names_to = "Metric", values_to = "value") |>
  group_by(Metric) |>
  summarise(Mean = mean(value), SD = sd(value),
            Min = min(value), Max = max(value), .groups = "drop")

stability <- baseline_test |>
  pivot_longer(everything(), names_to = "Metric", values_to = "Published_split") |>
  left_join(scores, by = "Metric")
print(by_seed, n = Inf)
print(stability)
