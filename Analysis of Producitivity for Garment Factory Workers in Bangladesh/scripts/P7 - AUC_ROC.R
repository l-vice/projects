
# LIBS --------------------------------------------------------------------

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

# DATA --------------------------------------------------------------------

# df.dt1 <- read.csv(file = "german_garment_workers_productivity_project/Data/dataframe_variables_task1.csv")
# matrix.dt1 <- readRDS(file = "german_garment_workers_productivity_project/Data/matrix_variables_task1.rds")
# x <- readRDS(file = "german_garment_workers_productivity_project/Data/predictors.rds")
# y <- readRDS(file = "german_garment_workers_productivity_project/Data/target.rds")

##############################################################################
# AUC/ROC -----------------------------------------------------------------
##############################################################################
# ROC - AUC estimation
roc.fn <- function(formulas, data, model_spec, s = 5) {
  # Stratified folds
  folds <- caret::createFolds(y = data$y, k = s, list = F)
  # Storage
  results <- vector(mode = "list", length(formulas))
  
  # Function
  for (f in seq_along(formulas)) { # sometimes i use 1:m (where, m = length(formulas)), sometimes seq_len/seq_along, im slowly migrating to seq_len because of simplicity...
    auc_values <- numeric(s)
    all_roc <- vector(mode = "list", length = s)
    
    for (j in 1:s) {
      # Data
      train_subset <- data[folds != j, ]
      test_subset  <- data[folds == j, ]
      # Function 
      if (model_spec == "LOGIT") {
        fit <- stats::glm(formula = formulas[[f]], family = "binomial", data = train_subset)
        probs <- predict(fit, newdata = test_subset, type = "response")
      }
      if (model_spec == "LDA") {
        fit <- MASS::lda(formula = formulas[[f]], data = transform(train_subset, y = factor(y)))
        probs <- predict(fit, newdata = test_subset)$posterior[, 2]
      }
      if (model_spec == "RDA") {
        fit <- klaR::rda(formula = formulas[[f]], data = transform(train_subset, y = factor(y)))
        probs <- predict(fit, newdata = test_subset)$posterior[, 2]
      } 
    roc_obj <- pROC::roc(test_subset$y, probs)
    auc_values[j] <- pROC::auc(roc_obj)
    all_roc[[j]] <- roc_obj
    }    
     # Return list
    results[[f]] <- list(
    formula = deparse(formulas[[f]]),
    auc_mean = mean(auc_values),
    auc_sd   = sd(auc_values),
    rocs     = all_roc
    )
  }
  results
}

###########################################################################
# GRAPH HELPERS -----------------------------------------------------------
###########################################################################

# Colors

cols <- c("blue", "red", "darkred", "gray", "gray45")

# Convert roc list to dataframe

df.converter_roc <- function(roc_list, model_name) {
  purrr::map_dfr(seq_along(roc_list), function(j) {
    roc_obj <- roc_list[[j]]
    data.frame(
      fpr   = 1 - roc_obj$specificities,
      tpr   = roc_obj$sensitivities,
      fold  = j,
      model = model_name
    )
  })
}

##############################################################################
# Let's create the roc_result list and create ggplot objects
##############################################################################

# LOGIT -------------------------------------------------------------------

set.seed(1);logit.roc <- roc.fn(formulas = formulas, data = df.dt1, model_spec = "LOGIT", s = 5);names(logit.roc) <- 
  c("best", "best_optimized", "interpretable", "interpretable_log", "parsimonious")

# Matrix --> Dataframe
logit.roc_df <- purrr::map2_dfr(
  logit.roc,
  names(logit.roc),
  ~ df.converter_roc(.x$rocs, .y)
)

# Mean AUC
logit.roc_mean_AUC <- logit.roc_df %>%
  dplyr::group_by(model, fpr) %>%
  dplyr::summarise(tpr = mean(tpr), .groups = "drop")

# Labels LOGIT
logit.auc_means <- sapply(logit.roc, function(x) x$auc_mean)
logit.auc_labels <- paste0(names(logit.auc_means), " (AUC=", round(logit.auc_means, 3), ")")
names(logit.auc_labels) <- names(logit.auc_means)

# Graph LOGIT

logit.roc_graph <- ggplot(logit.roc_df, aes(x = fpr, y = tpr, group = fold, color = model)) +
  geom_line(alpha = 0.4) +
  geom_line(
    data = logit.roc_mean_AUC,
    aes(x = fpr, y = tpr, color = model, group = model),
    size = 1.2
  ) +
  geom_abline(linetype = "dashed", color = "gray50") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
  labs(
    title = "Logistic Regression",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  scale_color_manual(values = cols) +
  facet_wrap(~ model, labeller = labeller(model = logit.auc_labels), scales = "free") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    panel.spacing = unit(1, "lines")
  );logit.roc_graph

  
# LDA ---------------------------------------------------------------------

set.seed(1);lda.roc <- roc.fn(formulas = formulas, data = df.dt1, model_spec = "LDA", s = 5);names(lda.roc) <- 
  c("best", "best_optimized", "interpretable", "interpretable_log", "parsimonious")

# Matrix --> Dataframe
lda.roc_df <- purrr::map2_dfr(
  lda.roc,
  names(lda.roc),
  ~ df.converter_roc(.x$rocs, .y)
)

# Mean AUC (LDA)

lda.roc_mean_AUC <- lda.roc_df %>%
  dplyr::group_by(model, fpr) %>%
  dplyr::summarise(tpr = mean(tpr), .groups = "drop")

# Labels (LDA)

lda.auc_means <- sapply(lda.roc, function(x) x$auc_mean)
lda.auc_labels <- paste0(names(lda.auc_means), " (AUC=", round(lda.auc_means, 3), ")")
names(lda.auc_labels) <- names(lda.auc_means)

# Graph LDA

lda.roc_graph <- ggplot(lda.roc_df, aes(x = fpr, y = tpr, group = fold, color = model)) +
  geom_line(alpha = 0.4) +  # all folds
  geom_line(
    data = lda.roc_mean_AUC,
    aes(x = fpr, y = tpr, color = model, group = model),
    size = 1.2
  ) +
  geom_abline(linetype = "dashed", color = "gray50") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Linear Discriminant Analysis",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  scale_color_manual(values = cols) +
  facet_wrap(~ model, labeller = labeller(model = lda.auc_labels), scales = "free") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    panel.spacing = unit(1, "lines")
  );lda.roc_graph


# Combined

patchwork_roc_graphs <- (logit.roc_graph | lda.roc_graph) +
  plot_annotation(
    title = "Cross-validated ROC Curves",
    subtitle = "Comparison of LOGIT vs LDA Models",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 13, hjust = 0.5)
    )
  );patchwork_roc_graphs

# saveRDS(object = patchwork_roc_graphs, file = "patchwork_roc_graphs.rds")

##############################################################################
# Considering AUC_ROC is a sample based estimate we need to compute the
# confidence intervals. How sure we are in these results.
##############################################################################

# AUC
auc.ci <- function(roc.result, R = 100, strata = T) {
  results <- vector(mode = "list", length = length(roc.result))
  names(results) <- names(formulas)
  for (i in seq_along(roc.result)) {
    results[[i]] <- pROC::ci.auc(roc = roc.result[[i]]$rocs[[1]], method = "bootstrap", 
                                 boot.n = R, stratified = strata)
  }
  results
}

# ROC
roc.ci <- function(roc.result, R = 100, strata = T, specificities = seq(0, 1, 0.05)) {
  results <- vector(mode = "list", length = length(roc.result))
  names(results) <- names(formulas)
  
  for (i in seq_along(roc.result)) {
    results[[i]] <- pROC::ci.se(
      roc.result[[i]]$rocs[[1]],
      boot.n = R,
      boot.strat = strata,
      specificities = specificities
    )
  }
  results
}

##############################################################################
# Extract data - summary function
##############################################################################

# AUC
auc.summary_fn <- function(auc.ci) {
  results <- lapply(seq_along(auc.ci), function (i) {
    ci <- auc.ci[[i]]
    data.frame(
      formula = names(auc.ci)[i],
      mean_t = ci[2],
      CI_lower_bound = ci[1],
      CI_upper_bound = ci[3]
    )
  })
  df <- do.call(rbind, results)
  rownames(df) <- NULL
  df
}

# ROC
roc.summary_fn <- function(roc.ci) {
  results <- lapply(seq_along(roc.ci), function(i) {
    ci.mat <- as.data.frame(roc.ci[[i]])
    ci.mat$specificity <- as.numeric(rownames(ci.mat))
    ci.mat$model <- names(roc.ci)[i]
    ci.mat
  })
  df <- do.call(rbind, results)
  rownames(df) <- NULL
  df
}

##############################################################################
# CONFIDENCE INTERVALS ----------------------------------------------------
##############################################################################

##############################################################################
# AUC
##############################################################################

# LOGIT
logit.auc <- auc.ci(roc.result = logit.roc, R = 2000, strata = T)

logit.summary_auc_ci <- auc.summary_fn(auc.ci = logit.auc)
suppressWarnings(print(logit.summary_auc_ci))

logit.ciauc_graph <- ggplot(logit.summary_auc_ci, aes(x = formula)) +
  geom_errorbar(aes(ymin = CI_lower_bound, ymax = CI_upper_bound),
                width = 0.15, color = "black", linewidth = 0.5) +
  geom_point(aes(y = mean_t), shape = 16, size = 2, color = "blue") +
  labs(
    title = "Logistic Regression",
    x = "Model",
    y = "Error Rate"
  ) +
  ylim(0.5, 1) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

logit.auc <- auc.ci(roc.result = logit.roc, R = 2000, strata = T)

# LDA
lda.auc <- auc.ci(roc.result = lda.roc, R = 2000, strata = T)
lda.summary_auc_ci <- auc.summary_fn(auc.ci = lda.auc);print(lda.summary_auc_ci)

lda.ciauc_graph <- ggplot(lda.summary_auc_ci, aes(x = formula)) +
  geom_errorbar(aes(ymin = CI_lower_bound, ymax  = CI_upper_bound),
                width = 0.15, color = "black", linewidth = 0.5) +
  geom_point(aes(y = mean_t), shape = 16, size = 2, color = "blue") +
  labs(
    title = "Linear Discriminant Analysis",
    x = "Model",
    y = "Error Rate"
  ) +
  ylim(0.5, 1) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Combined

auc_ci_combined <- (logit.ciauc_graph | lda.ciauc_graph) +
  plot_annotation(
    title = "Bootstrap AUC with 95% Confidence Intervals",
    subtitle = "Comparison of LOGIT and LDA Models",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 13, hjust = 0.5)
    )
  );auc_ci_combined

# saveRDS(object = auc_ci_combined, file = "auc_ci_combined.rds")

##############################################################################
# ROC
##############################################################################

# LOGIT

logit.roc_ci <- roc.ci(roc.result = logit.roc, R = 2000, strata = T, specificities = seq(0, 1, 0.05))
logit.summary_roc_ci <- roc.summary_fn(roc.ci = logit.roc_ci);print(logit.summary_roc_ci)

logit.rocci_graph <- ggplot(logit.summary_roc_ci, aes(x = 1 - specificity, y = `50%`)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2, fill = "blue") +
  facet_wrap(~ model, scales = "free") +
  labs(
    title = "Logistic Regression",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  );print(logit.rocci_graph)

# LDA

lda.roc_ci <- roc.ci(roc.result = lda.roc, R = 2000, strata = T, specificities = seq(0, 1, 0.05))
lda.summary_roc_ci <- roc.summary_fn(roc.ci = lda.roc_ci);print(lda.summary_roc_ci)

lda.rocci_graph <- ggplot(lda.summary_roc_ci, aes(x = 1 - specificity, y = `50%`)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2, fill = "blue") +
  facet_wrap(~ model, scales = "free") +
  labs(
    title = "Linear Discriminant Analysis",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  );print(lda.rocci_graph)
# Combined


roc_ci_combined <- (logit.rocci_graph | lda.rocci_graph) +
  plot_annotation(
    title = "ROC Curves with 95% Bootstrap Confidence Intervals",
    subtitle = "Comparison of LOGIT vs LDA Models",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 13, hjust = 0.5)
    )
  )

roc_ci_combined
# saveRDS(object = roc_ci_combined, file = "roc_ci_combined.rds")

# END