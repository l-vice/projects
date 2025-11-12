# LIBS --------------------------------------------------------------------

# LoadLibraries <- function() {
#  require(tidyverse)
#  print("Libs loaded: GTG")
# }

# LoadLibraries()

# DATA --------------------------------------------------------------------

final_dataset_timeseries <- read.csv(file = "Data/final_data_timeseries_test_orange_predictions.csv")

final_dataset_timeseries <- final_dataset_timeseries %>%
  mutate(
    published_at = as.POSIXct(x = published_at, format = "%m/%d/%Y",
                              tz = "UTC"),
    month = factor(month),
    age_bin = factor(age_bin),
    duration_bucket = factor(duration_bucket),
    category_id = factor(category_id)
  )

# DATA TABLE --------------------------------------------------------------

final_ts_long <- final_dataset_timeseries %>%
  dplyr::select(published_at,
                actual = log1p_like_view_ratio,
                mod_lm, mod_xgboost, mod_rf, mod_adb
  ) %>% 
  pivot_longer(
    cols = starts_with("mod_"),
    names_to = "model",
    values_to = "pred") %>%
  dplyr::mutate(
    model = dplyr::recode(
      model,
      "mod_lm" = "Linear Regression",
      "mod_rf" = "Random Forest",
      "mod_xgboost" = "XGBoost",
      "mod_adb" = "AdaBoost",
    ),
    model = factor(model, levels = c("Linear Regression", "Random Forest", "XGBoost", "AdaBoost")),
    resid = actual - pred
  )

# Performance Metrics -----------------------------------------------------

performance_metrics <- final_ts_long %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(
    RMSE = sqrt(mean((pred - actual)^2)),
    MAE = mean(abs(pred - actual)),
    R2 = 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2),
    MAPE = mean(abs((actual - pred) / actual)) * 100,
    SMAPE = mean(2 * abs(pred - actual) / (abs(actual) + abs(pred))) * 100,
    .groups = "drop"
  )

print(performance_metrics)

# Scatter plot of Predictive Accuracy  -----------------------------------

ggplot(final_ts_long, aes(x = actual, y = pred)) +
  geom_point(alpha = 0.6, size = 1.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.6) +
  geom_smooth(method = "lm", se = F, linewidth = 0.7) +
  facet_wrap(~ model) +
  labs(title = "Accuracy Scatter plot of Actual vs Predicted for like/view ratio",
       x = "Actual",
       y = "Predicted") +
  theme_minimal(base_size = 12)

# Scatter plot of Residuals ---------------------------------------------------

ggplot(final_ts_long, aes(x = pred, y = resid)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point(alpha = 0.6, size = 1.6) +
  stat_smooth(method = "loess", se = F, linewidth = 0.7, color = "blue") +
  labs(
    title = "Scatter plot of Residuals vs Predicted for like/view ratio",
    x = "Predicted",
    y = "Residuals",
  ) +
  facet_wrap(~ model, ncol = 2, scales = "free_x") +
  theme_minimal(base_size = 12)

# Time Series Plots -------------------------------------------------------------------

# Stationary Time-Series

ggplot(final_ts_long, aes(x = published_at)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 0.7) +
  geom_line(aes(y = pred, color = model), linewidth = 0.5, alpha = 0.9) +
  scale_color_manual(values = model_colors) +
  facet_wrap(~ model, ncol = 2, scales = "free_x") +
  labs(
    title = "Time Series of Predicted vs Actual for like/view ratio",
    x = "Published Date",
    y = "log(like_view_ratio + 1)",
    color = "Legend"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

# Smoothed/Non-Stationary Time-Series

final_ts_long_smoothed <- final_ts_long %>%
  group_by(model) %>%
  arrange(published_at) %>%
  mutate(
    actual_smooth = rollmean(actual, k = 7, fill = NA, align = "right"),
    pred_smooth = rollmean(pred, k = 7, fill = NA, align = "right")
  ) %>%
  ungroup()

model_colors <- c(
  "Actual" = "black",
  "Linear Regression" = "blue",
  "Random Forest" = "darkred",
  "XGBoost" = "steelblue",
  "AdaBoost" = "red"
)

ggplot(final_ts_long_smoothed, aes(x = published_at)) +
  geom_line(aes(y = actual_smooth, color = "Actual"), linewidth = 0.7) +
  geom_line(aes(y = pred_smooth, color = model), linewidth = 0.5, alpha = 0.9) +
  scale_color_manual(values = model_colors) +
  facet_wrap(~ model, ncol = 2, scales = "free_x") +
  labs(
    title = "Smoothed Time Series of Predicted vs Actual for like/view ratio",
    x = "Published Date", y = "log(like_view_ratio + 1)",
    color = "Legend"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )




