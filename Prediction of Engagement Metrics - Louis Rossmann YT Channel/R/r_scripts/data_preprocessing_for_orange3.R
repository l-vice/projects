
# 08/03/2025 - Author: LBK 

# LIBS --------------------------------------------------------------------

LoadLibraries <- function() {
  require(readxl)
  require(tidyverse)
  require(lubridate)
  require(hms)
}

LoadLibraries()

# DATA --------------------------------------------------------------------

df <- read_excel("Data/rossman_variables_raw.xlsx")

df <- df %>%
  dplyr::select(
    -topic_categories, -viewers_peak,
    -actual_end, -actual_start,
    -location_lng, -location_lat,
    -location_desc, -recording_date,
    -made_for_kids, -license,
    -privacy_status, -favorite_count,
    -dimension, -duration_iso,
    -default_audio_lang, -default_lang,
    -tags, -description,
    -channel_title, -channel_id,
    -title, -video_id
  ) %>%
  mutate(
    duration_seconds = as.numeric((lubridate::hms(duration_hms))),
    duration_bucket = cut(
      duration_seconds,
      breaks = c(-Inf, 480, 1200, Inf),
      labels = c("<8m", "8-20m", ">20m"),
      right = T
    )
  ) %>%
  mutate(
    published_at = ymd_hms(published_at, tz = "UTC"),
    age_days = as.numeric(difftime(Sys.time(), published_at, units = "days")),
    weekday = wday(published_at, label = T, week_start = 1),
    weekend = as.integer(weekday %in% c("Sat", "Sun")),
    hour = hour(published_at),
    month = month(published_at, label = T)
  ) %>%
  mutate(
    age_bin = cut(
      age_days,
      breaks = c(-Inf, 6, 30, 90, 180, 365, 730, 1095, Inf),
      labels = c("0–6d", "1–4w", "1–3m", "3–6m", "6–12m", "1–2y", "2–3y", "3y+"),
      right = TRUE
    )
  ) %>%
  mutate(
    category_id = factor(category_id),
    hour = factor(hour),
    month = factor(month, ordered = F)
  ) %>%
  dplyr::select(-duration_hms, -published_at)

# Lets convert two level factors to binary variables for efficient application
# of ML models in Orange3.

bin_map <- c(
  caption = "true",
  definition = "hd",
  projection = "360",
  upload_status = "processed",
  licensed_content = TRUE
)

df <- df %>%
  mutate(
    across(all_of(names(bin_map)),
           ~ as.integer(.x == bin_map[cur_column()]),
           .names = "is_{.col}")
  ) %>%
  dplyr::select(-all_of(names(bin_map)))

# We check for variables with constant features
# <= 0.05 | => 0.95

df <- df %>%
  dplyr::select(-all_of(
    df %>%
      dplyr::select(starts_with("is_")) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "var", values_to = "mean") %>%
      filter(mean <= 0.05 | mean >= 0.95) %>%
      pull(var)
  ))

# The final step is to remove all videos that were previously live streams since
# they are not part of our objective.

df <- df %>%
  filter(was_livestream != TRUE) %>%
  dplyr::select(-was_livestream)

# Write .csv file
write.csv(x = df, file = "rossman_vars.csv", row.names = F)

# MODEL to select vars ----------------------------------------------------

# We fit a model an were left with variables that had varying ranges of predictive power.
# They all passed VIF.
#  Y = like_view_ratio, X = cateogry_id, duration_bucket, view_count, age_days, duration_seconds, comment_view_ratio

# Data rev ---------------------------------------------------------------

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

df <- df[-high_leverage_observations, ]

# This is only for our LM model

# We extract non-transformed variables, to investigate the source for kurtosis
df$comment_view_ratio <- df$comment_count / df$view_count
df$like_view_ratio <- df$like_count / df$view_count

non_transformed_numeric_vars <- df %>%
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
# transformations. We decide to plug in log1p and assess further

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

bc <- boxcox(lm(bc_comment_view_ratio ~ 1, data = non_transformed_numeric_vars))
lambda <- bc$x[which.max(bc$y)] # in between cube root and log-transform

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
lambda.ds <- bc.ds$x[which.max(bc.ds$y)] # fourth power (.25) and log transform

# Both variables indicate the need for substantial skew reduction

bc_transformed_duration_seconds <- if (lambda.ds == 0) {
  log(non_transformed_numeric_vars$duration_seconds)
} else {
  (non_transformed_numeric_vars$duration_seconds^lambda.ds - 1) / lambda.ds
}

transformations_durations_seconds <- data.frame(
  sqrt_duration_seconds = sqrt(non_transformed_numeric_vars$duration_seconds),
  bxcx_duration_seconds = bc_transformed_duration_seconds
)
summary(mod.wls.bc)
par(mfrow = c(1, 2))

for (v in names(transformations_durations_seconds)) {
  qqnorm(transformations_durations_seconds[[v]], main = v)
  qqline(transformations_durations_seconds[[v]])
}

moments::kurtosis(transformations_durations_seconds$sqrt_duration_seconds)
moments::kurtosis(transformations_durations_seconds$bxcx_duration_seconds)
par(mfrow = c(1, 1))