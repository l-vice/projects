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

# BOOTSTRAPP CI -----------------------------------------------------------
# As always, we specify the requirements for bootstrapping
# 1. Specify the statistic of interest for which bootstrap will sample data for
# 2. Run boot function with desired number of R's

# STAT FN -------------------------------------------------------------------

# Statistic of interest

boot.fn <- function(formula, data, idx, model_spec, s = 5) {
  # data = chosen data frame
  # idx = placeholder for boot()
  # model_spec = Specify so we obtain model fit logic
  # s = Number of folds, 5 is the default value
  # k = Number of kNN's for knn algorithm, 1 is the default value
  data <- data[idx, ] # Classic bootstrap, it's not the best
  folds <- caret::createFolds(y = data$y, k = s, list = F)
  cv.errors <- numeric(s)
  
  for (j in 1:s) {
    train_subset <- data[folds != j, ]
    test_subset <- data[folds == j, ]
    
    if (model_spec == "LOGIT") {
      fit <- stats::glm(formula = formula, family = "binomial", 
                        data = train_subset)
      probs <- predict(object = fit, newdata = test_subset, type = "response")
      pred <- ifelse(probs > 0.5, 1, 0)
    }
    if (model_spec == "LDA") {
      fit <- MASS::lda(formula = formula, data = transform(train_subset, y = factor(y)))
      probs <- predict(object = fit, newdata = test_subset)
      pred <- as.integer(as.character(probs$class))
    }
    if (model_spec == "RDA") {
      fit <- klaR::rda(formula = formula, data = transform(train_subset, y = factor(y)))
      probs <- predict(object = fit, newdata = test_subset)
      pred <- as.integer(as.character(probs$class))
    }
      cv.errors[j] <- 1 - mean(pred == test_subset$y)
   }
  mean(cv.errors)
}

# kNN
boot.fn_knn <- function(dataframe, idx, k = 1, s = 5) {
  data <- dataframe[idx, ]
  folds <- caret::createFolds(y = data$y, k = 5, list = F)
  cv.errors <- numeric(s)
  #
  for (j in 1:s) {
    # Data
    train_subset <- data[folds != j, ]
    test_subset <- data[folds == j, ]
    # Requirements
    X.train <- as.matrix(train_subset[, names(train_subset) != "y"])
    X.test <- as.matrix(test_subset[, names(test_subset) != "y"])
    Y.train <- factor(train_subset$y)
    # Scale
    mu <- colMeans(X.train)
    std <- apply(X.train, 2, sd);std[std == 0] <- 1
    
    X.train <- scale(X.train, center = mu, scale = std)
    X.test <- scale(X.test, center = mu, scale = std)
    # Predict
    pred <- class::knn(train = X.train, test = X.test, cl = Y.train, k = k)
    pred <- as.integer(as.character(pred))
    
    cv.errors[j] <- 1 - mean(pred == test_subset$y)
  }
  mean(cv.errors)
}

# Run boot for multiple models (LOGIT, LDA, RDA)

run.boot <- function(formulas, model_spec, data, R = 100, strata = NULL, s = 5, ...) {
  results <- setNames(vector("list", length(formulas)), names(formulas))
  # 
  for (f in seq_along(formulas)) {
    fml <- formulas[[f]]
    results[[f]] = boot::boot(
      data = data,
      statistic = function(data, idx) {
        boot.fn(formula = fml, data = data, idx = idx, model_spec = model_spec, s = s)
      }, 
      R = R,
      strata = strata
    )
  }
  results
}

# Run boot for multiple models (kNN)

run.boot_knn <- function(dataframes, R = 100, strata = NULL, s = 5, k = 1, ...) { 
  results <- setNames(vector("list", length(dataframes)), names(dataframes))
  #   
  for (d in seq_along(dataframes)) {
    data <- dataframes[[d]]
    results[[d]] <- boot::boot(
      data = data,
      statistic = function(data, idx) {
        boot.fn_knn(dataframe = data, idx = idx, k = k, s = s)
      }, 
      R = R,
      strata = strata
    )
  }
  results
}

# BOOT --------------------------------------------------------------------

# LOGIT

# set.seed(1);logit.boot <- run.boot(formulas = formulas, model_spec = "LOGIT",
#                                    data = df.dt1, R = 1000, strata = df.dt1$y, s = 5);saveRDS(object = 
#                           logit.boot, file = "logit.boot_results.rds")

logit.boot <- readRDS(file = "Objects/logit.boot_results.rds")
# LDA

# set.seed(1);lda.boot <- run.boot(formulas = formulas, model_spec = "LDA",
#                                  data = df.dt1, R = 1000, strata = df.dt1$y, s = 5);saveRDS(object = 
#                                  lda.boot, file = "lda.boot_results.rds")

lda.boot <- readRDS(file = "Objects/lda.boot_results.rds") 

# Confidence Intervals ----------------------------------------------------  

# These are the best results from CV over 1000 iterations each.
# Considering we ran it 1000 times we can even take the lowest result, but lets stay conservative and take the mean
# error rate across s folds.
# LOGIT
cv.logit
(cv.logit_mean_across_ERR <-colMeans(cv.logit))
(cv.logit_mean_accross_AR <- rep(1, 5) - cv.logit_mean_across_ERR)

cv.logit_means <- data.frame(
  Model = c("best", "best_optimized", "interpretable", "interpretable_log", "parsimonious"),
  Accuracy = c(0.8045587, 0.8046260, 0.7791252, 0.7925109, 0.7593808),
  Error = c(0.1954413, 0.1953740, 0.2208748, 0.2074891, 0.2406192)
)

# LDA 
cv.lda
(cv.lda_mean_across_ERR <- colMeans(cv.lda))
(cv.logit_mean_across_AR <- rep(1, 5) - cv.lda_mean_across_ERR)

cv.lda_means <- data.frame(
  Model = c("best", "best_optimized", "interpretable", "interpretable_log", "parsimonious"),
  Accuracy = c(0.8068277, 0.8062841, 0.7730435, 0.7899820, 0.7593990),
  Error = c(0.1931723, 0.1937159, 0.2269565, 0.2100180, 0.2406010 )
)

# Instead of manually withdrawing results for all models let's create a function that will do that.
boot.summary_fn <- function(boot) {
  results <- list()
  
  for (f in names(boot)) {
    b <- boot[[f]] # b = boot[[f]]
    # Percentile CI's
    ci <- boot.ci(b, type = "perc", index = 1) 
    lower_bound <- ci$percent[4]
    upper_bound <- ci$percent[5]
     
    results[[f]] <- data.frame(
      formula = f,
      t0 = b$t0[1],
      mean_t = mean(b$t),
      CI_lower_bound = lower_bound,
      CI_upper_bound = upper_bound
    )
  }
  dplyr::bind_rows(results)
}

# GRAPH --------------------------------------------------------------------
# Now i need to plot the results. For this part I will rely on ChatGPT since I don't really know yet how to use ggplot fully and have yet to learn.
# I would have used base R to plot this but its even more difficult than ggplot.

# LOGIT

# Extract ci results; append the cv results - minimal
logit.summary_ci <- boot.summary_fn(boot = logit.boot);logit.summary_ci$cv_means <-
  cv.lda_means$Error;print(logit.summary_ci)

# Graph
logit.bootci_graph <- ggplot(logit.summary_ci, aes(x = formula)) +
  geom_errorbar(aes(ymin = CI_lower_bound, ymax = CI_upper_bound),
                width = 0.15, color = "black", linewidth = 0.5) +
  geom_point(aes(y = mean_t, color = "Bootstrap Mean"), shape = 16, size = 2) +
  geom_point(aes(y = cv_means, color = "CV Mean"), shape = 1, size = 2) +
  geom_hline(yintercept = 0.20, linetype = "dashed", color = "red", linewidth = 0.5) +
  scale_color_manual(values = c("Bootstrap Mean" = "black",
                                "CV Mean" = "blue")) +
  labs(
    title = "Logistic Regression",
    x = "Model",
    y = "Error Rate",
    color = "Metric"
  ) +
  ylim(0, 0.4) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )

# LDA

# Extract ci results; append CV results
lda.summary_ci <- boot.summary_fn(boot = lda.boot);lda.summary_ci$cv_means <-
  cv.lda_means$Error;print(lda.summary_ci)

# Graph

lda.bootci_graph <- ggplot(lda.summary_ci, aes(x = formula)) +
  geom_errorbar(aes(ymin = CI_lower_bound, ymax = CI_upper_bound),
                width = 0.15, color = "black", linewidth = 0.5) +
  geom_point(aes(y = mean_t, color = "Bootstrap Mean"), shape = 16, size = 2) +
  geom_point(aes(y = cv_means, color = "CV Mean"), shape = 1, size = 2) +
  geom_hline(yintercept = 0.20, linetype = "dashed", color = "red", linewidth = 0.5) +
  scale_color_manual(values = c("Bootstrap Mean" = "black",
                                "CV Mean" = "blue")) +
  labs(
    title = "Linear Discriminant Analysis",
    x = "Model",
    y = "Error Rate",
    color = "Metric"
  ) +
  ylim(0, 0.4) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  ) 
lda.bootci_graph

# Combined
bootci_graphs <- (logit.bootci_graph | lda.bootci_graph) +
  plot_annotation(
    title = "Bootstrap vs Cross-Validation Error Across Models",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  )

bootci_graphs
# saveRDS(object = bootci_graphs, file = "bootstrap_ci_plots.rds")

# NEXT --> AUC_ROC