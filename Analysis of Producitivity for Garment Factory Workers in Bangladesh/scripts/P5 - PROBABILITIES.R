###########################################################################
# LIBS --------------------------------------------------------------------
###########################################################################
LoadLibraries <- function() {
  require(tidyverse) # general
  require(patchwork) # Combining plots of interactive effects
  require(corrplot) # Plot for correlations of numerical variables
  require(caret) # near-zero variance
  require(car) # vif, residualPlots
  require(class) # knn
  require(MASS) # lda, qda
  require(klaR) # regularized qda
  require(boot) # bootstrap methods
  require(glmnet) # ridge, lasso
  require(leaps) # regsubsets
  require(pls) # partial-least squares, pcr
  require(L0Learn) # subset selection for logit link
  print("Packages loaded")
}

LoadLibraries()
  
###########################################################################
# INTERPRETATION ----------------------------------------------------------
###########################################################################

# Define baseline
baseline <- data.frame(
  department = 0, no_of_style_change = 0,
  incentive = mean(df.dt1$incentive), no_of_workers = mean(df.dt1$no_of_workers), wip_sewing = mean(df.dt1$wip_sewing),
  team2 = 0, team6 = 0, team7 = 0, team8 = 0, team9 = 0, team10 = 0, team11 = 0,
  week_in_monthweek_3 = 0, 
  daySunday = 0, dayThursday = 0
)

# Define a function that will predict probabilities for a numeric variable

predict.effect_numeric <- function(x, seq_length = 50) {
  seq_vals <- seq(min(df.dt1[[x]]),
                  max(df.dt1[[x]]),
                  length.out = seq_length)
  scenarios <- baseline[rep(1, seq_length), ]
  scenarios[[x]] <- seq_vals
  
  pred <- predict(models$glm.fit.clear, newdata = scenarios, type = "response")
  
  res <- data.frame(
    variable = x,
    xval = seq_vals,
    prob = pred
  )
  rownames(res) <- NULL
  return(res)
}


# Function to predict probabilities for a categorical variables
predict.effect_factor <- function(x, levels = c(0, 1)) {
  scenarios <- baseline[rep(1, length(levels)), ]
  scenarios[[x]] <- levels
  
  pred <- predict(models$glm.fit.clear, newdata = scenarios, type = "response")
  
  res <- data.frame(
    variable = x,
    xval = levels,
    prob = pred
  )
  rownames(res) <- NULL
  return(res)
}

# Function to predict probabilities for an interaction effect

predict.effect_interaction <- function(x1, x2, vals1, vals2) {
  grid <- expand.grid(val1 = vals1, val2 = vals2)
  scenarios <- baseline[rep(1, nrow(grid)), ]
  scenarios[[x1]] <- grid$val1
  scenarios[[x2]] <- grid$val2
  pred <- predict(models$glm.fit.clear, newdata = scenarios, type = "response")
  data.frame(x1 = grid$val1, x2 = grid$val2, prob = pred, 
             interaction = paste(x1, "x", x2))
}

###########################################################################
# PREDICTIONS -------------------------------------------------------------
###########################################################################

###########################################################################
# BASELINE ----------------------------------------------------------------
###########################################################################

p_baseline <- predict(models$glm.fit.clear, newdata = baseline, type = "response");print(p_baseline)

###########################################################################
# NUMERIC -----------------------------------------------------------------
###########################################################################

# Incentive
p_incentive <- predict.effect_numeric(x = "incentive", seq_length = 100);print(p_incentive)

# wip_sewing 
p_wip <- predict.effect_numeric(x = "wip_sewing", seq_length = 100);print(p_wip)

numeric_effects <- rbind(p_incentive, p_wip)
class(numeric_effects);print(numeric_effects)

numeric_vars <- c("incentive", "wip_sewing")
effects_numeric <- do.call(rbind, lapply(numeric_vars, predict.effect_numeric))

###########################################################################
# FACTORS -----------------------------------------------------------------
###########################################################################

factor_vars <- c("department", "no_of_style_change", "team6", "team7",
                 "team8", "team9", "team10", "team11", "week_in_monthweek_3",
                 "daySunday", "dayThursday")

effects_factor <- do.call(rbind, lapply(factor_vars, predict.effect_factor));row.names(
  effects_factor) <- NULL;print(effects_factor)

###########################################################################
# INTERACTIONS ------------------------------------------------------------
###########################################################################

effects_interactions <- list()

# Department

# department * no_of_workers
effects_interactions[["department_no_of_workers"]] <- predict.effect_interaction(
  "department", "no_of_workers",
  vals1 = c(0, 1),
  vals2 = seq(min(df.dt1$no_of_workers), max(df.dt1$no_of_workers), length.out = 30)
)

# No of style change

# team8 * no_of_style_change
effects_interactions[["team8_no_of_style_change"]] <- predict.effect_interaction(
  "team8", "no_of_style_change",
  vals1 = c(0, 1),
  vals2 = seq(min(df.dt1$no_of_style_change), max(df.dt1$no_of_style_change), length.out = 30)
)

# team10 * no_of_style_change
effects_interactions[["team10_no_of_style_change"]] <- predict.effect_interaction(
  "team10", "no_of_style_change",
  vals1 = c(0, 1),
  vals2 = seq(min(df.dt1$no_of_style_change), max(df.dt1$no_of_style_change), length.out = 30)
)

# Incentive

# team7 * incentive

effects_interactions[["team7_incentive"]] <- predict.effect_interaction(
  "team7", "incentive",
  vals1 = c(0, 1),
  vals2 = seq(min(df.dt1$incentive), max(df.dt1$incentive), length.out = 30)
)

# team10 * incentive

effects_interactions[["team10_incentive"]] <- predict.effect_interaction(
  "team10", "incentive",
  vals1 = c(0, 1),
  vals2 = seq(min(df.dt1$incentive), max(df.dt1$incentive), length.out = 30)
)

# team2 * incentive

effects_interactions[["team2_incentive"]] <- predict.effect_interaction(
  "team2", "incentive",
  vals1 = c(0, 1),
  vals2 = seq(min(df.dt1$incentive), max(df.dt1$incentive), length.out = 30)
)

effects.interactions_df <- do.call(rbind, effects_interactions)

###########################################################################
# GRAPHS ------------------------------------------------------------------
###########################################################################

# Factors

effects_factor <- effects_factor %>%
  dplyr::mutate(
    group = dplyr::case_when(
      variable == "department"          & xval == 0 ~ "Sewing",
      variable == "department"          & xval == 1 ~ "Finishing",
      variable == "no_of_style_change"  & xval == 0 ~ "No Style Change",
      variable == "no_of_style_change"  & xval == 1 ~ "Style Change",
      variable == "team6"               & xval == 0 ~ "Not Team 6",
      variable == "team6"               & xval == 1 ~ "Team 6",
      variable == "team7"               & xval == 0 ~ "Not Team 7",
      variable == "team7"               & xval == 1 ~ "Team 7",
      variable == "team8"               & xval == 0 ~ "Not Team 8",
      variable == "team8"               & xval == 1 ~ "Team 8",
      variable == "team9"               & xval == 0 ~ "Not Team 9",
      variable == "team9"               & xval == 1 ~ "Team 9",
      variable == "team10"              & xval == 0 ~ "Not Team 10",
      variable == "team10"              & xval == 1 ~ "Team 10",
      variable == "team11"              & xval == 0 ~ "Not Team 11",
      variable == "team11"              & xval == 1 ~ "Team 11",
      variable == "week_in_monthweek_3" & xval == 0 ~ "Other Weeks",
      variable == "week_in_monthweek_3" & xval == 1 ~ "Week 3",
      variable == "daySunday"           & xval == 0 ~ "Not Sunday",
      variable == "daySunday"           & xval == 1 ~ "Sunday",
      variable == "dayThursday"         & xval == 0 ~ "Not Thursday",
      variable == "dayThursday"         & xval == 1 ~ "Thursday",
      TRUE ~ as.character(xval)
    )
  )

# Numeric threshold

threshold <- function(df, x) {
  df %>%
    filter(variable == x & prob >= 0.99) %>%
    slice(1) %>%
    pull(xval)
}

# Incentive
threshold_incentive <- threshold(effects_numeric, "incentive")

# wip_sewing
threshold_wip <- threshold(df = effects_numeric, x = "wip_sewing")

thresholds_df <- data.frame(
  variable = c("incentive", "wip_sewing"),
  xval     = c(threshold_incentive, threshold_wip),
  label    = c(paste0("≈", round(threshold_incentive), " incentive"),
               paste0("≈", round(threshold_wip), " WIP"))
)

# Numeric main effects

numeric_p_effects <- ggplot(effects_numeric, aes(x = xval, y = prob)) +
  geom_line(color = "black", size = 1) +
  facet_wrap(~ variable, scales = "free_x") +
  geom_vline(data = thresholds_df, aes(xintercept = xval), 
             color = "red", linetype = "dashed") +
  geom_text(data = thresholds_df, aes(x = xval, y = 0.92, label = label),
            color = "red", angle = 90, vjust = -0.5, hjust = 0, inherit.aes = FALSE) +
  labs(title = "Numeric Variable Effects",
       x = "Value", y = "Predicted Probability") +
  theme_minimal()

# Factor main effects

factor_p_effects <- ggplot(effects_factor, aes(x = group, y = prob, fill = factor(xval))) +
  geom_col(position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", prob)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3.5) +
  facet_wrap(~ variable, scales = "free_x") +
  scale_fill_manual(values = c("0" = "gray", "1" = "gray20")) +
  labs(title = "Probability of Success by Group Membership",
       x = "Group", y = "Predicted Probability") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 25, hjust = 1))

# Interactions 
interactions_p_effects <- ggplot(effects.interactions_df, aes(x = x2, y = prob, color = factor(x1))) +
  geom_line(size = 1) +
  facet_wrap(~ interaction, scales = "free") +
  scale_color_manual(values = c("0" = "gray", "1" = "gray20")) +
  labs(title = "Interaction Effects on Probability of Success",
       x = "Numeric Variable", y = "Predicted Probability",
       color = "Factor level") +
  theme_minimal()
 
p8 <- numeric_p_effects
p9 <- factor_p_effects
p10 <- interactions_p_effects

# Combined

effects_summary <-  (p9 | p10 / p8) +
  plot_annotation(
    title = "Probabilities of Success in a Controlled Environment",
    subtitle = "Incentives, Overtime, and Staffing Structure",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5)
    )
  );print(effects_summary)

# probability_plot <- effects_summary
# saveRDS(object = probability_plot, file = "probability_plot.rds")

probability_plots <- list(
  numeric_p_effects = p8,
  factor_p_effects = p9,
  interactions_p_effects = p10
)

interaction_probability_plots <- list(
  probability_plots = probability_plots,
  interaction_plots = interaction_plots
)

patchwork_plots <- list(
  dept_summary = dept_summary, # interactions
  team_summary, # interactions
  team_incentive_summary = team_incentive_summary, # interactions
  team_overtime_summary = team_overtime_summary, # interactions
  operational_summary = operational_summary, # interactions
  effects_summary # probability section
)

# saveRDS(object = patchwork_plots, file = "patchwork_plots.rds")
# saveRDS(object = interaction_probability_plots, file = "interaction_and_probability_plots.rds")
# saveRDS(object = effects_summary, file = "Objects/probability_plot.rds")

effects_summary
# NEXT --> BOOTSTRAP 