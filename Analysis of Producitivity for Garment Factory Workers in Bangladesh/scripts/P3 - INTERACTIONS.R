
# LIBS --------------------------------------------------------------------

LoadLibraries <- function() {
  require(tidyverse) # general
  require(patchwork) # Combining plots of interactive effects
  require(scales) # Graph styling
  require(corrplot) # Plot for correlations of numerical variables
  require(caret) # near-zero variance
  require(car) # vif, residualPlots
  require(class) # knn
  require(MASS) # lda, qda
  require(klaR) # regularized qda
  require(boot) # bootstrap methods
  require(glmnet) # ridge, lasso
  require(leaps) # regsubsets
  require(L0Learn) # subset selection for logit link
  print("Packages loaded")
}

LoadLibraries()

###########################################################################
# DISCLAIMER --------------------------------------------------------------
###########################################################################

# All plots are available as individual objects in the "Objects" folder.

###########################################################################
# TEAMS -------------------------------------------------------------------
###########################################################################

# Create team_id from dummy columns
team_cols <- paste0("team", 2:12)
# Default everyone to team 1
df.dt1.temp$team_id <- 1

# Assign team based on which dummy = 1
for (i in 2:12) {
  df.dt1.temp$team_id[df.dt1.temp[[paste0("team", i)]] == 1] <- i
}

table(rowSums(df.dt1.temp[, team_cols]))


# Baseline team mean
baseline.mean_team <- mean(df.dt1.temp$y[df.dt1.temp$team_id %in% c(1,2,3,4,5,12)], na.rm = TRUE)

# Per-team mean
team_perf <- aggregate(y ~ team_id, data = df.dt1.temp, FUN = mean, na.rm = TRUE)
print(team_perf)

# Add classification
team_perf$perf_class <- with(team_perf, ifelse(y >= 0.8, "Overperforming",
                                               ifelse(y >= 0.75 & y < 0.8, "Sufficient",
                                               "Underperforming")))

###########################################################################
# DAYS --------------------------------------------------------------------
###########################################################################

baseline.mean_days <- mean(df.dt1.temp$y)

# Let's combine all days
df.dt1.temp <- df.dt1.temp %>%
  mutate(day = case_when(
    daySunday == 1    ~ "Sunday",
    daySaturday == 1  ~ "Saturday",
    dayThursday == 1  ~ "Thursday",
    dayWednesday == 1 ~ "Wednesday",
    dayTuesday == 1   ~ "Tuesday",
    TRUE              ~ "Monday"   # baseline
  )) %>%
  mutate(day = factor(day,
                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Saturday", "Sunday")))

# let's classify mean performance per day
day.perf <- as.data.frame(df.dt1.temp %>%
  group_by(day) %>%
  summarise(y = mean(y)) %>%
  mutate(
    perf_class = case_when(
      y >= 0.80 ~ "Overperforming",
      y >= 0.75 ~ "Sufficient",
      TRUE ~ "Underperforming"
    )
  )
);print(day.perf)

print(day.perf)

###########################################################################
# WEEKS -------------------------------------------------------------------
###########################################################################

df.dt1.temp <- df.dt1.temp %>%
  dplyr::mutate(
    week_in_month = case_when(
      week_in_monthweek_2 == 1 ~ "Week 2",
      week_in_monthweek_3 == 1 ~ "Week 3",
      week_in_monthweek_4 == 1 ~ "Week 4",
      TRUE ~ "Week 1" # baseline
    )) %>%
  dplyr::mutate(week_in_month = factor(week_in_month,
                                       levels = c("Week 1", "Week 2", "Week 3", "Week 4")))

week.perf <- as.data.frame(df.dt1.temp %>%
  dplyr::group_by(week_in_month) %>%
  summarise(y = mean(y)) %>%
  mutate(
    perf_class = case_when(
      y >= 0.80 ~ "Overperforming",
      y >= 0.75 ~ "Sufficient",
      TRUE ~ "Underperforming"
    )
  )
);print(week.perf)

print(week.perf)

day.perf

###########################################################################
# DEPARTMENT --------------------------------------------------------------
###########################################################################
table(df.dt1.temp$department)

df.dt1.temp <- df.dt1.temp %>%
  dplyr::mutate(
    department_real = case_when(
      department == 1 ~ "Finishing",
      TRUE ~ "Sewing" # Baseline
    )) %>%
  dplyr::mutate(department_real = factor(department_real,
                                         levels = c("Sewing", "Finishing")))

dept.perf <- as.data.frame(df.dt1.temp %>%
  dplyr::group_by(department_real) %>%
  summarise(y = mean(y)) %>%
  dplyr::mutate(
    perf_class = case_when(
      y >= 0.80 ~ "Overperforming",
      y >= 0.75 ~ "Sufficient",
      TRUE ~ "Underperforming"
    )
  )
);print(dept.perf)


###########################################################################
# COMBINED ----------------------------------------------------------------
###########################################################################

# Team
team_perf_clean <- team_perf %>%
  rename(group = team_id) %>%
  mutate(group = as.character(group),
         group_type = "Team")

# Day
day_perf_clean <- day.perf %>%
  rename(group = day) %>%
  mutate(group = as.character(group),
         group_type = "Day")

# Week
week_perf_clean <- week.perf %>%
  rename(group = week_in_month) %>%
  mutate(group = as.character(group),
         group_type = "Week")

# Department
dept_perf_clean <- dept.perf %>%
  rename(group = department_real) %>%
  mutate(group = as.character(group),
         group_type = "Department")

# Combine all
perf_all <- bind_rows(team_perf_clean, day_perf_clean, 
                      week_perf_clean, dept_perf_clean);print(perf_all)

###########################################################################
# NUMERICAL ---------------------------------------------------------------
###########################################################################

make_numeric_perf <- function(data, var) {
  data %>%
    mutate(bin = ntile(.data[[var]], 4)) %>%
    group_by(bin) %>%
    summarise(y = mean(y)) %>%
    dplyr::mutate(
      perf_class = case_when(
        y >= 0.8 ~ "Overperforming",
        y >= 0.75 ~ "Sufficient",
        TRUE ~ "Underperforming"
      ),
      group_type = var,
      group = paste0(var, "_Q", bin)
    ) %>%
    dplyr::select(group_type, group, y, perf_class)
}


###########################################################################
targeted_perf <- df.dt1.temp %>%
  dplyr::group_by(targeted_productivity) %>%
  summarise(y = mean(y, na.rm = TRUE)) %>%
  mutate(
    perf_class = case_when(
      y >= 0.80 ~ "Overperforming",
      y >= 0.75 ~ "Sufficient",
      TRUE ~ "Underperforming"
    ),
    group_type = "Targeted Productivity",
    group = as.character(targeted_productivity)
  ) %>%
  dplyr::select(group_type, group, y, perf_class)


numeric_vars <- c("smv", "over_time", "incentive", 
                  "no_of_workers", "wip_sewing")

numeric_perf_all <- bind_rows(lapply(numeric_vars, function(v) make_numeric_perf(df.dt1.temp, v)))

print(numeric_perf_all, n = 50)

###########################################################################
# HEATMAPS ----------------------------------------------------------------
###########################################################################

# We will plot the effects of numerical variables on categories with heatmaps.

###########################################################################
# INCENTIVE ---------------------------------------------------------------
###########################################################################

# Calculate incentive_bin variable

df.dt1.temp <- df.dt1.temp %>%
  mutate(
    incentive_bin = cut(
      incentive,
      breaks = quantile(incentive[incentive > 0],
                        probs = seq(0, 1, 0.25),
                        na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Q1", "Q2", "Q3", "Q4")
    ),
    incentive_bin = ifelse(incentive == 0, "No Incentive", as.character(incentive_bin))
  )

# Calculate quantile tresholds
q_vals <- quantile(df.dt1.temp$incentive[df.dt1.temp$incentive > 0], 
                   probs = seq(0, 1, 0.25), na.rm = TRUE)

# Turn into strings
q_labels <- paste0("Q1: ", round(q_vals[1],2), "–", round(q_vals[2],2), " | ",
                   "Q2: ", round(q_vals[2],2), "–", round(q_vals[3],2), " | ",
                   "Q3: ", round(q_vals[3],2), "–", round(q_vals[4],2), " | ",
                   "Q4: ", round(q_vals[4],2), "–", round(q_vals[5],2))

###########################################################################
# INCENTIVE * DEPARTMENT --------------------------------------------------
###########################################################################

# Create a heatmap data frame
heatmap_data <- df.dt1.temp %>%
  group_by(department_real, incentive_bin) %>%
  summarise(y = mean(y, na.rm = TRUE), .groups = "drop") %>%
  complete(department_real, incentive_bin, fill = list(y = NA)) %>%
  mutate(
    perf_class = case_when(
      is.na(y) ~ "No Data",
      y < 0.75 ~ "Underperforming",
      y >= 0.75 & y <= 0.80 ~ "Sufficient",
      y > 0.80 ~ "Overperforming"
    )
  )

# Plot
plot_incentive_dept <- ggplot(heatmap_data, aes(
  x = factor(incentive_bin, 
             levels = c("No Incentive","Q1","Q2","Q3","Q4")),
  y = fct_rev(as.factor(department_real)),
  fill = perf_class
)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  geom_text(aes(label = ifelse(is.na(y), "", scales::percent(y, accuracy = 1))), 
            size = 3.5, color = "black") +
  scale_fill_manual(
    values = c(
      "Underperforming" = "#e74c3c",
      "Sufficient" = "#f1c40f",
      "Overperforming" = "#2ecc71",
      "No Data" = "grey80"
    ),
    name = "Performance"
  ) +
  labs(
    title = "Department Performance Across Incentive Quartiles",
    x = "Incentive Quartile",
    y = "Department",
    caption = paste("Incentive thresholds →", q_labels)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid = element_blank(),
    plot.caption = element_text(hjust = 1, face = "italic")
  )

###########################################################################
# INCENTIVE * TEAMS -------------------------------------------------------
###########################################################################

# Create a heatmap dataframe
heatmap_data_team <- df.dt1.temp %>%
  group_by(team_id, incentive_bin) %>%
  summarise(y = mean(y, na.rm = TRUE), .groups = "drop") %>%
  complete(team_id, incentive_bin, fill = list(y = NA)) %>%   # ensure full grid
  mutate(
    perf_class = case_when(
      is.na(y) ~ "No Data",
      y < 0.75 ~ "Underperforming",
      y >= 0.75 & y <= 0.80 ~ "Sufficient",
      y > 0.80 ~ "Overperforming"
    )
  )

# Plot
plot_incentive_team <- ggplot(heatmap_data_team, aes(
  x = factor(incentive_bin, 
             levels = c("No Incentive","Q1","Q2","Q3","Q4")),
  y = fct_rev(as.factor(team_id)),
  fill = perf_class
)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  geom_text(aes(label = ifelse(is.na(y), "", scales::percent(y, accuracy = 1))), 
            size = 3, color = "black") +
  scale_fill_manual(
    values = c(
      "Underperforming" = "#e74c3c",
      "Sufficient" = "#f1c40f",
      "Overperforming" = "#2ecc71",
      "No Data" = "grey80"
    ),
    name = "Performance"
  ) +
  labs(
    title = "Team Performance Across Incentive Quartiles",
    x = "Incentive Quartile",
    y = "Team",
    caption = paste("Incentive thresholds →", q_labels)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    plot.caption = element_text(hjust = 1, face = "italic")
  )


###########################################################################
# OVERTIME ----------------------------------------------------------------
###########################################################################

# Turn into hours since its easier to interpret

df.dt1.temp <- df.dt1.temp %>%
  mutate(over_time_hours = over_time / 60)


q_vals_ot <- quantile(
  df.dt1.temp$over_time_hours[df.dt1.temp$over_time_hours > 0], 
  probs = seq(0, 1, 0.25), na.rm = TRUE
)


q_labels_ot <- paste0(
  "Q1: ", round(q_vals_ot[1],1), "–", round(q_vals_ot[2],1), " hrs | ",
  "Q2: ", round(q_vals_ot[2],1), "–", round(q_vals_ot[3],1), " hrs | ",
  "Q3: ", round(q_vals_ot[3],1), "–", round(q_vals_ot[4],1), " hrs | ",
  "Q4: ", round(q_vals_ot[4],1), "–", round(q_vals_ot[5],1), " hrs"
)


df.dt1.temp <- df.dt1.temp %>%
  mutate(
    overtime_bin = cut(
      over_time_hours,
      breaks = quantile(over_time_hours[over_time_hours > 0],
                        probs = seq(0, 1, 0.25),
                        na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Q1", "Q2", "Q3", "Q4")
    ),
    overtime_bin = ifelse(over_time_hours == 0, "No Overtime", as.character(overtime_bin))
  )

###########################################################################
# OVERTIME * DEPARTMENT ---------------------------------------------------
###########################################################################

# Create a heatmap
heatmap_data_ot_dept <- df.dt1.temp %>%
  group_by(department_real, overtime_bin) %>%
  summarise(y = mean(y, na.rm = TRUE), .groups = "drop") %>%
  complete(department_real, overtime_bin, fill = list(y = NA)) %>%
  mutate(
    perf_class = case_when(
      is.na(y) ~ "No Data",
      y < 0.75 ~ "Underperforming",
      y >= 0.75 & y <= 0.80 ~ "Sufficient",
      y > 0.80 ~ "Overperforming"
    )
  )

# Plot
plot_overtime_dept <- ggplot(heatmap_data_ot_dept, aes(
  x = factor(overtime_bin, levels = c("No Overtime","Q1","Q2","Q3","Q4")),
  y = fct_rev(as.factor(department_real)),
  fill = perf_class
)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  geom_text(aes(label = ifelse(is.na(y), "", scales::percent(y, accuracy = 1))),
            size = 3.5, color = "black") +
  scale_fill_manual(
    values = c("Underperforming"="#e74c3c","Sufficient"="#f1c40f",
               "Overperforming"="#2ecc71","No Data"="grey80"),
    name = "Performance"
  ) +
  labs(
    title = "Department Performance Across Overtime Quartiles",
    x = "Overtime Quartile",
    y = "Department",
    caption = paste("Overtime thresholds →", q_labels_ot)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face="bold", hjust=0.5, size=16),
    axis.title = element_text(size=13, face="bold"),
    axis.text = element_text(size=11),
    legend.title = element_text(size=12, face="bold"),
    legend.text = element_text(size=11),
    panel.grid = element_blank(),
    plot.caption = element_text(hjust=1, face="italic")
  )


###########################################################################
# OVERTIME * TEAMS --------------------------------------------------------
###########################################################################

# Create a heatmap
heatmap_data_ot_team <- df.dt1.temp %>%
  group_by(team_id, overtime_bin) %>%
  summarise(y = mean(y, na.rm = TRUE), .groups = "drop") %>%
  complete(team_id, overtime_bin, fill = list(y = NA)) %>%
  mutate(
    perf_class = case_when(
      is.na(y) ~ "No Data",
      y < 0.75 ~ "Underperforming",
      y >= 0.75 & y <= 0.80 ~ "Sufficient",
      y > 0.80 ~ "Overperforming"
    )
  )

# Plot
plot_overtime_team <- ggplot(heatmap_data_ot_team, aes(
  x = factor(overtime_bin, levels = c("No Overtime","Q1","Q2","Q3","Q4")),
  y = fct_rev(as.factor(team_id)),
  fill = perf_class
)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  geom_text(aes(label = ifelse(is.na(y), "", scales::percent(y, accuracy = 1))),
            size = 3, color = "black") +
  scale_fill_manual(
    values = c(
      "Underperforming" = "#e74c3c",
      "Sufficient" = "#f1c40f",
      "Overperforming" = "#2ecc71",
      "No Data" = "grey80"
    ),
    name = "Performance"
  ) +
  labs(
    title = "Team Performance Across Overtime Quartiles",
    x = "Overtime Quartile",
    y = "Team",
    caption = paste("Overtime thresholds →", q_labels_ot)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    plot.caption = element_text(hjust = 1, face = "italic")
  )

###########################################################################
# NUMBER OF WORKERS -------------------------------------------------------
###########################################################################

# Create a dataframe

df_workers_team <- df.dt1.temp %>%
  group_by(department_real, team_id) %>%
  summarise(mean_workers = mean(no_of_workers, na.rm = TRUE), .groups = "drop")

# Scale and size specifications

dodge <- position_dodge2(width = 0.7, preserve = "single")
y_max <- max(df_workers_team$mean_workers, na.rm = TRUE)

# Plot

plot_mean_workers <- ggplot(
  df_workers_team,
  aes(x = factor(team_id), y = mean_workers, fill = department_real)
) +
  geom_col(position = dodge, width = 0.65) +
  geom_text(
    aes(label = number(mean_workers, accuracy = 0.1)),
    position = dodge, vjust = -0.35, size = 3.1, color = "black"
  ) +

  scale_y_continuous(
    limits = c(0, y_max * 1.12),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(values = c(Sewing = "gray20", Finishing = "gray")) +
  labs(
    title = "Mean Number of Workers per Team by Department",
    x = "Team", y = "Mean Number of Workers", fill = "Department"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5, size = 12, margin = margin(b = 4)),
    axis.title   = element_text(face = "bold", size = 10),
    axis.text    = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text  = element_text(size = 9),
    plot.margin  = margin(6, 6, 6, 6)
  ) +
  coord_cartesian(clip = "off")

###########################################################################
# COMBINE STRUCTURAL ASSESSMENT -------------------------------------------
###########################################################################

# All combined
p1 <- plot_incentive_dept
p2 <- plot_incentive_team
p3 <- plot_overtime_dept
p4 <- plot_overtime_team
p5 <- plot_mean_workers

# Layout
summary_plot <- (p1 | p2) / (p3 | p4) / p5 +
  plot_annotation(
    title = "Summary of Workforce Productivity and Staffing",
    subtitle = "Incentives, Overtime, and Staffing Structure Across Departments and Teams",
    theme = theme(plot.title = element_text(face="bold", size=18, hjust=0.5),
                  plot.subtitle = element_text(size=14, hjust=0.5))
  )

summary_plot

# Department
dept_summary <- (p1 / p3 / p5) +
  plot_annotation(
    title = "Department-Level Summary of Productivity and Staffing",
    subtitle = "Incentives, Overtime, and Staffing Structure at the Department Level",
    theme = theme(
      plot.title = element_text(face="bold", size=18, hjust = 0.5),
      plot.subtitle = element_text(size=14, hjust=0.5)
    )
  )

dept_summary

# Team

# Incentive and over time combined
team_summary <- (p2 / p4 / p5) + 
  plot_annotation(
    title = "Team-Level Summary of Productivity and Staffing",
    subtitle = "Incentives, Overtime, and Staffing Structure at the Team Level",
    theme = theme(
      plot.title = element_text(face="bold", size=18, hjust=0.5),
      plot.subtitle = element_text(size=14, hjust=0.5)
    )
  )

team_summary

# Incentive
team_incentive_summary <- (p2 / p5) +
  plot_annotation(
    title = "Team-Level Incentive Summary",
    subtitle = "Incentives and Staffing Structure Across Teams",
    theme = theme(
      plot.title = element_text(face="bold", size=18, hjust=0.5),
      plot.subtitle = element_text(size=14, hjust=0.5)
    )
  )

team_incentive_summary

# Overtime
team_overtime_summary <- (p4 / p5) +
  plot_annotation(
    title = "Team-Level Overtime Summary",
    subtitle = "Overtime and Staffing Structure Across Teams",
    theme = theme(
      plot.title = element_text(face="bold", size=18, hjust=0.5),
      plot.subtitle = element_text(size=14, hjust=0.5)
    )
  )

team_overtime_summary

###########################################################################
# PRODUCTIVITY ------------------------------------------------------------
###########################################################################

title_sz      <- 12
subtitle_sz   <- 10
axis_title_sz <- 10
axis_text_sz  <- 9
label_sz      <- 3.2

base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.title   = element_text(face = "bold", hjust = 0.5, size = title_sz, margin = margin(b = 4)),
    axis.title   = element_text(face = "bold", size = axis_title_sz),
    axis.text    = element_text(size = axis_text_sz),
    panel.grid.minor = element_blank(),
    plot.margin  = margin(6, 6, 6, 6)
  )

###########################################################################
# SMV (Standard-Minute-Value) ---------------------------------------------
###########################################################################

# Create dataframe 
df_smv_dept <- df.dt1.temp %>%
  group_by(department_real) %>%
  summarise(mean_smv = mean(smv, na.rm = TRUE), .groups = "drop")

# Plot
smv_dept <- ggplot(df_smv_dept, aes(x = department_real, y = mean_smv, fill = department_real)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = number(mean_smv, accuracy = 0.1)),
            vjust = -0.35, size = label_sz, color = "black") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.07)),
    labels = label_number(accuracy = 1)
  ) +
  scale_fill_manual(values = c("Sewing" = "gray20", "Finishing" = "gray")) +
  labs(
    title = "Mean SMV (Standard Minute Value) by Department",
    x = "Department", y = "Mean SMV", fill = "Department"
  ) +
  base_theme +
  theme(legend.position = "none") +
  coord_cartesian(clip = "off")

###########################################################################
# WIP (Work in Progress) --------------------------------------------------
###########################################################################

# Create a dataframe
df_wip_team <- df.dt1.temp %>%
  filter(department_real == "Sewing") %>%
  group_by(team_id) %>%
  summarise(mean_wip = mean(wip_sewing, na.rm = TRUE), .groups = "drop")

# Plot
wip_sewing_team <- ggplot(df_wip_team, aes(x = factor(team_id), y = mean_wip)) +
  geom_col(fill = "gray20", width = 0.7) +
  geom_text(aes(label = comma(round(mean_wip, 0))),
            vjust = -0.35, size = label_sz, color = "black") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.07)),
    labels = label_comma()
  ) +
  labs(
    title = "Mean Work-in-Progress (WIP) per Sewing Team",
    x = "Team", y = "Mean WIP (Sewing)"
  ) +
  base_theme +
  coord_cartesian(clip = "off")

wip_sewing_team

###########################################################################
# COMBINING PRODUCTIVITY --------------------------------------------------
###########################################################################

p6 <- smv_dept 
p7 <- wip_sewing_team

operational_summary <- ((p6 | p7) / p5) +
  plot_annotation(
    title = "Operational Load and Staffing Summary",
    subtitle = "Work-in-Progress at Team Level, Task Complexity at Department Level, and Workforce Structure",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5)
    )
  )

operational_summary

###########################################################################
# COMBINED ----------------------------------------------------------------
###########################################################################

interaction_plots <- list(
  plot_incentive_dept = p1,
  plot_incentive_team = p2,
  plot_overtime_dept = p3,
  plot_overtime_team = p4,
  plot_mean_workers = p5,
  smv_dept = p6,
  wip_sewing_team = p7
)

# NEXT -->  MODELING