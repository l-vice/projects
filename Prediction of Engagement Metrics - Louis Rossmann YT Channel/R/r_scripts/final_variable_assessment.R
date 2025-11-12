
# Packages ----------------------------------------------------------------

LoadLibraries <- function() {
  require(MASS)
  require(forecast)
  require(forcats)
  require(tidyverse)
}

LoadLibraries()

# PROCEEDING WITH THE BEST MODEL ------------------------------------------

best_model_data <- read.csv("Data/rossman_vars_select_final.csv")

best_model_data <- best_model_data %>%
  mutate(
    month = factor(month),
    category_id = factor(category_id),
    age_bin = factor(age_bin),
    duration_bucket = factor(duration_bucket)
  )

best.model <- lm(like_view_ratio ~ comment_view_ratio + duration_bucket + 
                   category_id + age_bin +
                   duration_seconds + month +
                   view_count + comment_count, data = best_model_data
                 )

summary(best.model)$adj.r.squared # Not great but let's try and improve it.

# Best model summary:
# All vars: age_days, comment_view_ratio, duration_bucket, category_id, age_bin, duration_seconds, month, view_count, comment_count
# Numerical: age_days, comment_view_ratio, duration_seconds, view_count, comment_count
# Categorical: duration_bucket, age_bin, category_id, month

# VARIABLE ASSESSMENT ------------------------------------------------------

# We plot a qq plot for each of the variables to further assess

# Now we proceed to transform our numerical variables

# Lets inspect numerical variables first

best_model_numerical_vars <- 
  data.frame(
  like_view_ratio = best_model_data$like_view_ratio, # Y
  age_days = best_model_data$age_days, # X1
  comment_view_ratio = best_model_data$comment_view_ratio, # X2
  duration_seconds = best_model_data$duration_seconds, # X3
  view_count = best_model_data$view_count, # X4
  comment_count = best_model_data$comment_count # X5
)

par(mfrow = c(3, 2))

for (v in names(best_model_numerical_vars)) {
  qqnorm(best_model_numerical_vars[[v]], main = v)
  qqline(best_model_numerical_vars[[v]])
}

par(mfrow = c(1, 1))

kurtosis_test <- function(df) {
  numeric_cols = sapply(df, is.numeric)
  kurtosis_values = sapply(df[, numeric_cols, drop = F], moments::kurtosis)
  
  return(kurtosis_values)
}

kurtosis_test_numeric_vals_best_mdl <- kurtosis_test(best_model_numerical_vars)
print(kurtosis_test_numeric_vals_best_mdl) 

# like_view_ratio = 3.73, age_days = 2.47, comment_view_ratio = 10.1, duration_seconds = 25.51
# view_count = 1933.82, comment_count = 308.59

# Now lets try log transformations

best_model_numerical_vars_log_transformed <- 
  data.frame(
  log1p_like_view_ratio = log1p(best_model_data$like_view_ratio),
  log1p_comment_view_ratio = log1p(best_model_data$comment_view_ratio),
  log1p_duration_seconds = log1p(best_model_data$duration_seconds),
  log1p_view_count = log1p(best_model_data$view_count),
  log1p_comment_count = log1p(best_model_data$comment_count)
)

par(mfrow = c(3, 2))

for (v in names(best_model_numerical_vars_log_transformed)) {
  qqnorm(best_model_numerical_vars_log_transformed[[v]], main = v)
  qqline(best_model_numerical_vars_log_transformed[[v]])
}

par(mfrow = c(1, 1))

kurtosis_test_numeric_vals_best_mdl_log_transformed <- kurtosis_test(best_model_numerical_vars_log_transformed)
print(kurtosis_test_numeric_vals_best_mdl_log_transformed)

#  like_view_ratio = 3.48, comment_view_ratio = 9.67, durations_seconds = 4.42, view_count = 3.84
# comment_count = 3.30 - Again you should BC transform comment_view_ratio and duration seconds

# Now lets apply boxcox to get the min-max the appropriate transformation

# comment_view_ratio

boxcox_comment_view_ratio <- best_model_numerical_vars$comment_view_ratio + 1e-6

boxcox_mod_cvr <- lm(boxcox_comment_view_ratio ~ 1)
boxcox_cvr <- MASS::boxcox(boxcox_mod_cvr)
lambda.cvr <- boxcox_cvr$x[which.max(boxcox_cvr$y)]

bc_transformed_comment_view_ratio <- if(lambda.cvr == 0) {
  log(boxcox_comment_view_ratio)
} else {
  (boxcox_comment_view_ratio^lambda.cvr - 1) / lambda.cvr
}

# duration_seconds

boxcox_mod_ds <- lm(duration_seconds ~ 1, data = best_model_numerical_vars)
boxcox_ds <- MASS::boxcox(boxcox_mod_ds)
lambda.ds <- boxcox_ds$x[which.max(boxcox_ds$y)]

bc_transformed_duration_seconds <- if(lambda.ds == 0) {
  log(best_model_numerical_vars$duration_seconds)
} else {
  (best_model_numerical_vars$duration_seconds^lambda.ds - 1) / lambda.ds
}

best_model_numerical_vars_boxcox_transformed <- 
  data.frame(
    bc_comment_view_ratio = bc_transformed_comment_view_ratio,
    bc_duration_seconds = bc_transformed_duration_seconds
  )

# Let's test the results

par(mfrow = c(2, 1))

for (v in names(best_model_numerical_vars_boxcox_transformed)) {
  qqnorm(best_model_numerical_vars_boxcox_transformed[[v]], main = v)
  qqline(best_model_numerical_vars_boxcox_transformed[[v]])
}

par(mfrow = c(1, 1))

# Much better

kurtosis_test_numeric_vals_best_mdl_boxcox_transformed <- kurtosis_test(best_model_numerical_vars_boxcox_transformed)
print(kurtosis_test_numeric_vals_best_mdl_boxcox_transformed)

# Categorical variables -------------------------------------------------------

# Let's separate them

best_model_categorical <- 
  data.frame(
    month = best_model_data$month,
    category_id = best_model_data$category_id,
    age_bin = best_model_data$age_bin,
    duration_bucket = best_model_data$duration_bucket
  )

# Check frequency tables

best_model_categorical <- names(best_model_data)[sapply(best_model_data, is.factor)]



# You should add up all values that have less than 2% of observations into a separate category called other

# category_id

min_count_cat <- 65

best_model_data_transformed_categoricals <- best_model_data

best_model_data_transformed_categoricals <- best_model_data_transformed_categoricals %>%
  mutate(
    category_id = fct_lump_min(category_id, min = min_count_cat, other_level = "Other"),
    category_id = fct_drop(category_id)
  )

# age_bin

best_model_data_transformed_categoricals <- best_model_data_transformed_categoricals %>%
  dplyr::mutate(
    age_bin = fct_collapse(
      age_bin,
      `<=1m` = c("0-6d", "1-4w"),
      `1-3m` = "1-3m",
      `3-6m` = "3-6m",
      `6-12m` = "6-12m",
      `1-2y` = "1-2y",
      `2-3y` = "2-3y",
      `3y+` = "3y+"
    )
  )

unique(best_model_data_transformed_categoricals$age_bin)

test.mod <- lm(like_view_ratio ~ comment_view_ratio + age_days + 
                 duration_seconds + comment_count +
                 view_count + month + 
                 category_id + age_bin + duration_bucket, 
               data = best_model_data_transformed_categoricals)

summary(test.mod)$adj.r.squared
summary(best.model)$adj.r.squared
summary(test.mod)
anova(test.mod, best.model)

# SAVE THE DATASET AND LOAD IT BACK IN ORANGE ---------------------------------

best_model_data <- best_model_data %>%
  dplyr::mutate(
    bc_comment_view_ratio = best_model_numerical_vars_boxcox_transformed$bc_comment_view_ratio,
    bc_duration_seconds = best_model_numerical_vars_boxcox_transformed$bc_duration_seconds,
    category_id = best_model_data_transformed_categoricals$category_id,
    age_bin = best_model_data_transformed_categoricals$age_bin
  ) %>%
  dplyr::select(
    -comment_view_ratio, -duration_seconds
  )

unique(best_model_data$category_id)
unique(best_model_data$age_bin)

best_model_data <- read.csv("Data/rossman_vars_select_final.csv")

write.csv(x = best_model_data, file = "best_model_data_R.csv", row.names = F)

# LOAD THE DATASET FROM ORANGE --------------------------------------------

final_dataset <- read.csv(file = "Data/rossman_vars_transformed_cat.csv")

final_dataset <- final_dataset %>%
  mutate(
    month = factor(month),
    category_id = factor(category_id),
    age_bin = factor(age_bin),
    duration_bucket = factor(duration_bucket),
    age_days = as.integer(age_days)
  )

# Train-Test split
split <- initial_split(data = final_dataset, prop = 0.8)

# Train
final_dataset_train <- training(split)

write.csv(x = final_dataset_train, file = "final_dataset_train.csv", row.names = F)

# Test
final_dataset_test <- testing(split)

write.csv(x = final_dataset_test, file = "final_dataset_test.csv", row.names = F)

