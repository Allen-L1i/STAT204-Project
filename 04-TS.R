library(tidyverse)
dir.create("plots", showWarnings = FALSE)

ts_energy <- read.csv("data/energydata_complete.csv")

ts_energy$date <- as.POSIXct(ts_energy$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
ts_energy <- ts_energy |> arrange(date)
stopifnot(nrow(ts_energy) == 19735, !anyNA(ts_energy), !anyDuplicated(ts_energy$date),
          all(diff(as.numeric(ts_energy$date)) == 10 * 60))

ts_energy$NSM <- hour(ts_energy$date) * 3600 + minute(ts_energy$date) * 60 + second(ts_energy$date)
withr::with_locale(c(LC_TIME = "C"), {
  ts_energy$Day_of_week <- factor(weekdays(ts_energy$date),
    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  ts_energy$WeekStatus <- factor(ifelse(
    ts_energy$Day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
})
ts_energy <- ts_energy |> 
  select(-c(rv1, rv2))

# Construct the lag
ts_energy <- ts_energy |> 
  mutate(Appliances_lag1 = lag(Appliances))

# Keep the paper's 75%/25% proportions, but split following time
split_index <- floor(0.75 * nrow(ts_energy))
ts_train_data <- ts_energy[seq_len(split_index), ] |> 
  slice(-1)
ts_test_data <- ts_energy[seq.int(split_index + 1L, nrow(ts_energy)), ]
stopifnot(!anyNA(ts_train_data), !anyNA(ts_test_data),
          max(ts_train_data$date) < min(ts_test_data$date),
          ts_test_data$Appliances_lag1[1] == tail(ts_train_data$Appliances, 1))

split_summary <- tibble(
  Split = c("Training", "Testing"),
  N = c(nrow(ts_train_data), nrow(ts_test_data)),
  Start = c(min(ts_train_data$date), min(ts_test_data$date)),
  End = c(max(ts_train_data$date), max(ts_test_data$date))
)
print(split_summary)

ts_training <- ts_train_data |> 
  select(-c(date, WeekStatus))
ts_testing <- ts_test_data |> 
  select(-c(date, WeekStatus))

# Refit on time order
baseline_fit <- lm(Appliances ~ . - Appliances_lag1, data = ts_training)
ar1_fit <- lm(Appliances ~ ., data = ts_training)

print(summary(baseline_fit))
print(summary(ar1_fit))

# At each test time the previous observation and current covariates are available.
predictions <- list(
  OLS = predict(baseline_fit, ts_testing),
  Persistence = ts_testing$Appliances_lag1,
  "OLS + lag 1" = predict(ar1_fit, ts_testing)
)
actual <- ts_testing$Appliances
test_comparison <- imap_dfr(predictions, function(predicted, model) {
  stopifnot(length(predicted) == length(actual), all(is.finite(predicted)))
  error <- actual - predicted
  tibble(Model = model, RMSE = sqrt(mean(error^2)), MAE = mean(abs(error)),
         R2 = 1 - sum(error^2) / sum((actual - mean(actual))^2))
})
print(test_comparison)

# Each lag is one 10-minute interval in the chronological training set.
png("plots/04-residual-acf.png", width = 10, height = 5, units = "in", res = 300)
par(mfrow = c(1, 2))
acf(resid(baseline_fit), lag.max = 36, main = "OLS", xlab = "Lag (10-minute intervals)",
    ylim = c(-0.2, 1))
acf(resid(ar1_fit), lag.max = 36, main = "OLS + lag 1", xlab = "Lag (10-minute intervals)",
    ylim = c(-0.2, 1))
dev.off()
