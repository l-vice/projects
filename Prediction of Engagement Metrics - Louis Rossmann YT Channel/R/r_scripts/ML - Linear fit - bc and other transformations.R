
# PACKAGE -----------------------------------------------------------------

LoadLibraries <- function() {
  require(lmtest)
  require(tidyverse)
  require(readxl)
  require(lubridate)
  require(hms)
  require(car)
  require(DescTools)
  require(dplyr)
}

LoadLibraries()
performance_metrics
# DATASET INVESTIGATION --------------------------------------------------

df <- read_excel("Data/rossman_vars_transformed.xlsx")

df <- df %>%
  mutate(
    category_id = factor(category_id),
    duration_bucket = factor(duration_bucket),
    weekday = factor(weekday),
    weekend = as.integer(weekend),
    hour = as.integer(hour),
    month = factor(month),
    age_bin = factor(age_bin),
  )

df <- df %>%
  dplyr::select(
    -view_count, -like_count,
    -comment_count, -duration_seconds,
    -weekday, -weekend,
    -hour, -month,
    -age_bin, -like_view_ratio,
    -comment_view_ratio, -log1p_like_count,
    -log1p_comment_count, -log1p_age_days,
    -age_days_cubed, -comment_view_ratio_squared
  )

# ANALYSIS --------------------------------------------------------------------

# This is the optimal model
mod.opt <- lm(formula = log1p_like_view_ratio ~ ., data = df)

# Let's inspect the residuals
par(mfrow = c(2, 2))

plot(mod.opt)

# Ok its clear that variance increases with the higher values,
# the Q-Q plot indicates non-normal residuals with heavy tails, and the leverage plot,
# highlights a few influential points. The fit for low-to medium engagement levels
# is fine however high engagement videos are not modeled properly. Justifying the use
# of WLS to improve reliability.

# Weighted-Least-Squares Model --------------------------------------------

# Extract residuals from the base model, errors between predicted and actual values
res <- resid(mod.opt)

# We will skip using the inverse-squared residuals since we are dealing with vars that are already
# log transformed
# We will use the rank based method to distribute weights
res_rank <- rank(abs(res), ties.method = "first")

# Reverse rank so the largest residuals weights the least
weights_rank <- 1 - res_rank / max(res_rank)

# We rescale to avoid zeros
weights_scaled <- 0.05 + 0.95 * weights_rank

# Now we model
par(mfrow=c(1, 1))
mod.wls <- lm(log1p_like_view_ratio ~ ., data = df, weights = weights_scaled)

# We tested all different combination of interaction effects with our categorical variables
# with the aim of addressing the "camel"/leptokurtic shape in our Q-Q plot however,
# no combination was effective in normalizing the residuals nor did any combination
# significantly improve our model, there were minor improvements in R^2 but all that
# was offset by introduction of multicollinearity. For that reason, we decided to move on
# with the basic model without any interactive terms. 

# Resolving high-leverage observations

# Investigate top leverage points
cooks_thresh <- 4 / nrow(df)
influential_points <- which(cooks.distance(mod.wls) > cooks_thresh)
length(influential_points)

df.lm <- df[-influential_points, ]
weights_scaled.lm <- weights_scaled[-influential_points]

mod.wls.s <- lm(log1p_like_view_ratio ~ ., data = df.lm, weights = weights_scaled.lm)
summary(mod.wls.s)
plot(mod.wls.s)

df.lm <- df.lm[-1439, ]
weights_scaled.lm <- weights_scaled.lm[-1439]
hatvalues(mod.wls.s)[1439]
cooks.distance(mod.wls.s)[1439]
rstandard(mod.wls.s)[1439]

par(mfrow = c(2, 2))
plot(mod.wls.s)
summary(mod.wls.s)$adj.r.squared
summary(mod.wls)$adj.r.squared

str(df.lm)
shapiro.test(df.lm$log1p_like_view_ratio)
shapiro.test(df.lm$log1p_view_count)
shapiro.test(df.lm$log1p_comment_view_ratio)
shapiro.test(df.lm$log1p_duration_seconds)
shapiro.test(df.lm$age_days)
test_norm

# Further transformations - Leptokurtic -----------------------------------


# RETRANSFORMING DATA -----------------------------------------------------

# We must remove the observations we identified as high leverage,
# we decided that the trade-off of removing these variables is more favourable
# to our analysis objective 

high_leverage_observations <- c(
  193, 249, 425, 432, 519, 682, 703, 773, 800, 815,
  962, 1014, 1084, 1100, 1102, 1130, 1357, 1365, 1378, 1382,
  1428, 1439, 1462, 1480, 1487, 1510, 1663, 2746, 2790, 2798,
  2815, 2828, 2845, 2848, 2892, 2923, 3098
)

df_linear_regression <- read_excel("Data/rossman_vars_transformed.xlsx")

df_linear_regression <- df_linear_regression[-high_leverage_observations, ]

write.csv(x = df_linear_regression, file = "rossman_vars_no_leverage_transformed.csv", row.names = F)

df_linear_regression <- df_linear_regression %>%
  mutate(
    category_id = factor(category_id),
    duration_bucket = factor(duration_bucket),
    weekday = factor(weekday),
    weekend = as.integer(weekend),
    hour = as.integer(hour),
    month = factor(month),
    age_bin = factor(age_bin)
  )

# We extract non-transformed variables, to investigate the source for kurtosis

non_transformed_numeric_vars <- df_linear_regression %>%
  dplyr::select(comment_view_ratio, view_count, 
                duration_seconds, age_days, 
                like_view_ratio)

# We plot a qq plot for each of the variables to further assess

par(mfrow = c(3, 2))

for (v in names(non_transformed_numeric_vars)) {
  qqnorm(non_transformed_numeric_vars[[v]], main = v)
  qqline(non_transformed_numeric_vars[[v]])
}

par(mfrow = c(1, 1))

attach(non_transformed_numeric_vars)
moments::kurtosis(comment_view_ratio)
moments::kurtosis(view_count)
moments::kurtosis(duration_seconds)
moments::kurtosis(age_days)
moments::kurtosis(like_view_ratio)
detach(non_transformed_numeric_vars)

# We can safely assume that all of the variables are in need for certain
# transformations. We decide to plug in log1p and asssess further

log_transformed_vars <- data.frame(
  log1p_comment_view_ratio = log1p(non_transformed_numeric_vars$comment_view_ratio),
  log1p_view_count = log1p(non_transformed_numeric_vars$view_count),
  log1p_duration_seconds = log1p(non_transformed_numeric_vars$duration_seconds),
  log1p_like_view_ratio = log1p(non_transformed_numeric_vars$like_view_ratio)
)

par(mfrow = c(2, 2))

for (v in names(log_transformed_vars)) {
  qqnorm(log_transformed_vars[[v]], main = v)
  qqline(log_transformed_vars[[v]])
}

par(mfrow = c(1, 1))

attach(log_transformed_vars)
moments::kurtosis(log1p_comment_view_ratio)
moments::kurtosis(log1p_view_count)
moments::kurtosis(log1p_duration_seconds)
moments::kurtosis(log1p_like_view_ratio)
detach(log_transformed_vars)

# Ok in here we decide to accept some excess since we are dealing with RW data
# we go on to further investigate comment_view_ratio and duration_seconds, trying
# to see if there is a transformation that could serve us better

# Add 1e-6 just so its non-zero
bc_comment_view_ratio <- non_transformed_numeric_vars$comment_view_ratio + 1e-6

boxcox_commetn
mod <- lm()

bc <- boxcox(lm(bc_comment_view_ratio ~ 1, data = non_transformed_numeric_vars))
lambda <- bc$x[which.max(bc$y)]

bc_transformed_comment_view_ratio <- if (lambda == 0) {
  log(bc_comment_view_ratio)
} else {
  (bc_comment_view_ratio^lambda - 1) / lambda
}

transformations_comment_view_ratio <- data.frame(
  sqrt_comment_view_ratio = sqrt(non_transformed_numeric_vars$comment_view_ratio),
  bxcx_comment_view_ratio = bc_transformed_comment_view_ratio
)

par(mfrow = c(2, 1))

for (v in names(transformations_comment_view_ratio)) {
  qqnorm(transformations_comment_view_ratio[[v]], main = v)
  qqline(transformations_comment_view_ratio[[v]])
}

moments::kurtosis(transformations_comment_view_ratio$sqrt_comment_view_ratio)
moments::kurtosis(transformations_comment_view_ratio$bxcx_comment_view_ratio)

par(mfrow = c(1, 1))

# Ok these are the results we've wanted. We successfully "normalized" 
# the comment_view_ratio to an acceptable degree. We will take that value as final

# Range is from 6 till 13250, we don't have to add anything
bc.ds <- boxcox(lm(non_transformed_numeric_vars$duration_seconds ~ 1, data = non_transformed_numeric_vars))
lambda.ds <- bc.ds$x[which.max(bc.ds$y)]

bc_transformed_duration_seconds <- if (lambda.ds == 0) {
  log(non_transformed_numeric_vars$duration_seconds)
} else {
  (non_transformed_numeric_vars$duration_seconds^lambda.ds - 1) / lambda.ds
}

transformations_durations_seconds <- data.frame(
  sqrt_duration_seconds = sqrt(non_transformed_numeric_vars$duration_seconds),
  bxcx_duration_seconds = bc_transformed_duration_seconds
)

par(mfrow = c(1, 2))

for (v in names(transformations_durations_seconds)) {
  qqnorm(transformations_durations_seconds[[v]], main = v)
  qqline(transformations_durations_seconds[[v]])
}

moments::kurtosis(transformations_durations_seconds$sqrt_duration_seconds)
moments::kurtosis(transformations_durations_seconds$bxcx_duration_seconds)

par(mfrow = c(1, 1))

# Ok in here we decide to accept the 3.68 or the 0.68 excess and move on to modelling

# COMBINING DATA AND MODELING ---------------------------------------------

df_linear_model_lk_transformed <- data.frame(
  log1p_like_view_ratio = log_transformed_vars$log1p_like_view_ratio,
  age_days = non_transformed_numeric_vars$age_days,
  log1p_view_count = log_transformed_vars$log1p_view_count,
  bc_comment_view_ratio = transformations_comment_view_ratio$bxcx_comment_view_ratio,
  bc_duration_seconds = transformations_durations_seconds$bxcx_duration_seconds,
  category_id = df_linear_regression$category_id,
  duration_bucket = df_linear_regression$duration_bucket
  )

str(df_linear_model_lk_transformed)
write.csv(x = df_linear_model_lk_transformed, file = "rossman_vars_no_leverage_transformed_for_normality.csv", 
          row.names = F)

# Train-Test split
split_standard <- rsample::initial_split(df_linear_model_lk_transformed, prop = 0.8)

# train
df_linear_model_lk_transformed_train <- rsample::training(split_standard)

write.csv(x = df_linear_model_lk_transformed_train, file = "rossman_vars_no_leverage_transformed_for_normality_train_set.csv", 
          row.names = F)

# test
df_linear_model_lk_transformed_test <- rsample::testing(split_standard)

write.csv(x = df_linear_model_lk_transformed_test, file = "rossman_vars_no_leverage_transformed_for_normality_test_set.csv", 
          row.names = F)

# Simplified version
# In here we removed box-cox transformations since there was not much to be gained

df_linear_model_lk_transformed_simplified <- data.frame(
  log1p_like_view_ratio = log_transformed_vars$log1p_like_view_ratio,
  age_days = non_transformed_numeric_vars$age_days,
  log1p_view_count = log_transformed_vars$log1p_view_count,
  sqrt_comment_view_ratio = transformations_comment_view_ratio$sqrt_comment_view_ratio,
  log1p_duration_seconds = log_transformed_vars$log1p_duration_seconds,
  category_id = df_linear_regression$category_id,
  duration_bucket = df_linear_regression$duration_bucket
)

write.csv(x = df_linear_model_lk_transformed_simplified, file = "rossman_vars_no_leverage_transformed_for_normality_simplified.csv", 
          row.names = F)

# Train-Test split
split_simplified <- rsample::initial_split(df_linear_model_lk_transformed, prop = 0.8)

# train
df_linear_model_lk_transformed_simplified_train <- rsample::training(split_simplified)

write.csv(x = df_linear_model_lk_transformed_simplified_train, file = "rossman_vars_no_leverage_transformed_for_normality_simplified_train_set.csv", 
          row.names = F)

# test
df_linear_model_lk_transformed_simplified_test <- rsample::testing(split_simplified)

write.csv(x = df_linear_model_lk_transformed_simplified_test, file = "rossman_vars_no_leverage_transformed_for_normality_simplified_test_set.csv", 
          row.names = F)

# MODELING ----------------------------------------------------------------

mod.optim.bc <- lm(log1p_like_view_ratio ~ ., data = df_linear_model_lk_transformed)
summary(mod.wls.bc)

# Some basic diagnostics
par(mfrow = c(2, 2))
plot(mod.wls.bc)

# Now we fit WLS to try and improve the model even further
res.bc <- resid(mod.wls.bc)

# We now want to rank the residuals (higher residuals = lower weights)
rank_res_bc <- rank(abs(res.bc), ties.method = "first")

# We scale the ranks into weights from ranges of 0.05 till 1
weights_rank_bc <- 1 - rank_res_bc / max(rank_res_bc)
weights_scaled_bc <- 0.05 + 0.95 * weights_rank_bc

# Now we add to our dataset - we will use this later to export to Orange
# df_linear_model_lk_transformed$wls_weights <- weights_scaled_bc

# Now we have our scaled weights, lets remodel

mod.wls.bc <- lm(log1p_like_view_ratio ~ ., data = df_linear_model_lk_transformed,
                  weights = weights_scaled_bc)

summary(mod.wls.bc)

par(mfrow = c(2, 2))
plot(mod.wls.bc)

# High leverage points
cooks_tresh <- 4 / nrow(df_linear_model_lk_transformed)
influential_points.bc <- which(cooks.distance(mod.wls.bc) > cooks_tresh)
length(influential_points.bc)

# We will keep these observations anyways

# Final assessment
summary(mod.wls.bc)
summary(mod.wls.bc)$adj.r.squared
summary(mod.wls.bc)$coefficients

# Saving the dataset for work in Orange3
rossman_vars_orange_bc_wls <- df_linear_model_lk_transformed
rossman_vars_orange_bc_wls$weights <- weights_scaled_bc
str(rossman_vars_orange_bc_wls)

