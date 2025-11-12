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
# DATA --------------------------------------------------------------------
###########################################################################

# df.dt1 <- read.csv(file = "german_garment_workers_productivity_project/Data/dataframe_variables_task1.csv")
# matrix.dt1 <- readRDS(file = "german_garment_workers_productivity_project/Data/matrix_variables_task1.rds")
# x <- readRDS(file = "german_garment_workers_productivity_project/Data/predictors.rds")
# y <- readRDS(file = "german_garment_workers_productivity_project/Data/target.rds")

# kNN data

# In this part of the data section we will create a dataset specifically for knn
# The idea is to include all variables but with transformations so we can reduce function complexity and overall simplicity.
# Data for glm.best, glm.best_low
knn.set_best <- df.dt1 %>%
  dplyr::mutate(
    # Transformations
    log1p_incentive = log1p(incentive),
    log1p_incentive_squared = log1p(incentive)^2,  
    smv_squared = smv^2,
    sqrt_over_time = sqrt(over_time),
    # Interactions
    team8_no_of_style_change = team8 * no_of_style_change,
    team10_no_of_style_change = team10 * no_of_style_change,
    team7_log1p_incentive = team7 * log1p(incentive),
    department_no_of_workers = department * no_of_workers
  ) %>%
  dplyr::select(
    -incentive, -over_time
  )

# Data for standard_interpretable

knn.set_interpretable <- df.dt1 %>%
  dplyr::mutate(
    # Interactions
    department_no_of_workers = department * no_of_workers,
    team8_no_of_style_change = team8 * no_of_style_change,
    team10_no_of_style_change = team10 * no_of_style_change,
    team7_incentive = team7 * incentive,
    team10_incentive = team10 * incentive,
    team2_incentive = team2 * incentive
  )

# Data for Interpretable_1se,
knn.set_interpretable_log <- df.dt1 %>%
  dplyr::mutate(
    # Transformations
    log1p_incentive = log1p(incentive),
    # Interactions
    department_no_of_workers = department * no_of_workers,
    team8_no_of_style_change = team8 * no_of_style_change,
    team10_no_of_style_change = team10 * no_of_style_change,
    team7_log1p_incentive = team7 * log1p_incentive,
    team10_log1p_incentive = team10 * log1p_incentive,
    team2_log1p_incentive = team2 * log1p_incentive
  ) %>%
  dplyr::select(
    -incentive
  )

knn.parsimonious <- df.dt1

knn.dataframes <- list(
  transformed_best = knn.set_best, # dataset for best and best_low
  standard_interpretable = df.dt1, # classic
  interpretable_log = knn.set_interpretable_log, # log1p_incentive
  parsimonious = knn.parsimonious # No changes
)

# saveRDS(object = knn.dataframes, file = "knn.dataframes.rds")

###########################################################################
# MODELING ----------------------------------------------------------------
###########################################################################

# FUNCTION 1:
# Let's create a function that will pick the best model using CV for X stratified folds 
# over X number of repetitions for the following models: LDA, RDA, LOGIT.

cv.iteration_fn <- function(formulas = NULL, model_spec, data, s = 5, r = 50, rda_lambda = NA, rda_gamma = NA) {
  m <- length(formulas)
  cv.means <- matrix(data = NA, nrow = s, ncol = m, dimnames = list(NULL, paste(names(formulas))))
  for (f in 1:m) {
    cv.error.rep <- matrix(data = NA, nrow = r, ncol = s)
    # Stratified folds
    for (r in 1:r) {
      folds <- caret::createFolds(data$y, k = s, list = F)
      cv.errors <- rep(0, s)
      # Function
      for (j in 1:s) {
        train_subset <- data[folds != j, ]
        test_subset <- data[folds == j, ]
        
        if (model_spec == "LOGIT") {
          fit <- stats::glm(formula = formulas[[f]], family = "binomial", data = train_subset)
          probs <- predict(object = fit, newdata = test_subset, type = "response")
          pred <- ifelse(probs > 0.5, 1, 0)
        }
        if (model_spec == "LDA") {
          fit <- MASS::lda(formula = formulas[[f]], data = transform(train_subset, y = factor(y)))
          probs <- predict(object = fit, newdata = test_subset)
          pred <- as.integer(as.character(probs$class))
        }
        if (model_spec == "RDA") {
          fit <- klaR::rda(formula = formulas[[f]], data = transform(train_subset, y = factor(y)), 
                           lambda = rda_lambda, gamma = rda_gamma)
          probs <- predict(object = fit, newdata = test_subset)
          pred <- as.integer(as.character(probs$class))
        }
        cv.errors[j] <- 1 - mean(pred == test_subset$y) # Level 1: Storing error rates in cv.errors vector
      }
      cv.error.rep[r, ] <- cv.errors # Level 2: We take the mean CV error rate over r repetitions and store it in rows
    }
    cv.means[, f] <- colMeans(cv.error.rep) # Level 3: We take the the mean of error rates across all repetitions for each model m
  }
  return(cv.means)
}

# FUNCTION 2
# Let's create a function that is similar to one above however we will focus on the special case for kNN
# We basically want the same thing, mean CV error rate of r repetitions. 

cv.iteration.knn_fn <- function(dataframes, k = 1, s = 5, r = 20) {
  df <- length(dataframes)
  cv.means <- matrix(data = NA, nrow = s, ncol = df, dimnames = list(NULL, paste(names(dataframes))))
  for (d in 1:df) {
    data <- dataframes[[d]]
    
    cv.error.rep <- matrix(NA, nrow = r, ncol = s)
    
    for (rep in 1:r) {
      folds <- caret::createFolds(data$y, k = s, list = F)
      cv.errors <- numeric(s)
      
      for (j in 1:s) {
        # Data
        train_subset <- data[folds != j, ]
        test_subset <- data[folds == j, ]
        # Requirements
        X.train <- as.matrix(train_subset[, names(train_subset) != "y"])
        X.test <- as.matrix(test_subset[, names(test_subset) != "y"])
        Y.train <- factor(train_subset$y)
        # Scale
        mu = colMeans(X.train)
        std = apply(X.train, 2, sd);std[std == 0] <- 1
        
        X.train <- scale(X.train, center = mu, scale = std)
        X.test <- scale(X.test, center = mu, scale = std)
        # Predictions
        pred <- class::knn(train = X.train, test = X.test, cl = Y.train, k = k)
        pred <- as.integer(as.character(pred))
        
        cv.errors[j] <- 1 - mean(pred == test_subset$y)
      }
      cv.error.rep[rep, ] <- cv.errors
    }
    cv.means[, d] <- colMeans(cv.error.rep)
  }
  return(cv.means)
}

###########################################################################
# CV ----------------------------------------------------------------------
###########################################################################

# LOGIT
# set.seed(1);cv.logit <- cv.iteration_fn(formulas = formulas, model_spec = "LOGIT", data = df.dt1, s = 5, r = 1000);print(cv.logit)

# LDA
# set.seed(1);cv.lda <- cv.iteration_fn(formulas = formulas, model_spec = "LDA", data = df.dt1, s = 5, r = 1000);print(cv.lda)

# RDA
# set.seed(1);cv.rda <- cv.iteration_fn(formulas = formulas, model_spec = "RDA", data = df.dt1, s = 5, r = 2);print(cv.rda) 

# kNN
# set.seed(1);cv.knn <- cv.iteration.knn_fn(dataframes = knn.dataframes, k = 3, s = 5, r = 1000);print(cv.knn)

# cv.results_ls <- list(
#  LOGIT = cv.logit,
#  LDA = cv.lda,
#  RDA = cv.rda,
#  kNN = cv.knn
# )
cv.results_ls <- read_rds(file = "Objects/cv_mc_results.rds")
# saveRDS(cv.results_ls, file = "cv_mc_results.rds")

cv.logit <- cv.results_ls$LOGIT
cv.lda <- cv.results_ls$LDA
cv.rda <- cv.results_ls$RDA
cv.knn <- cv.results_ls$kNN

###########################################################################
# PLOT --------------------------------------------------------------------
###########################################################################

# LOGIT
cv.logit_best <- which(cv.logit == min(cv.logit), arr.ind = T);print(cv.logit_best);cv.logit[1, 2] # best_optimized; 0.194575
logit.idx <- which(cv.logit == min(cv.logit), arr.ind = T)
logit.fold <- logit.idx[1]
logit.model <- logit.idx[2]
s <- 5
cols <- c("blue", "red", "darkred", "gray", "gray45")

par(mfrow = c(1, 1));matplot(1:s, cv.logit, type = "b", pch = 20, lty = 1,
        col = cols, xlab = "Number of folds", ylab = "Error rate",
        main = "Error rates for LOGIT");legend("topright", legend = paste(names(formulas)),
        col = cols, cex = 0.75, lty = 1, pch = 20);points(x = logit.fold, y = cv.logit[logit.fold, logit.model], col = "red", cex = 1.5, pch = 20)

# LDA
cv.lda_best <- which(cv.lda == min(cv.lda), arr.ind = T);print(cv.lda_best);cv.lda[1, 2] # best_optimized; 0.1949663
lda.idx <- which(cv.lda == min(cv.lda), arr.ind = T)
lda.fold <- lda.idx[1]
lda.model <- lda.idx[2]

par(mfrow = c(1, 1));matplot(1:s, y = cv.lda, type = "b", xlab = "Number of folds", ylab = "Error rate",
                             col = cols, lty = 1, pch = 20, main = "Error rates for LDA");legend("topright", 
                     legend = paste(names(formulas)), col = cols, cex = 0.75, lty = 1, pch = 20);points(
                       x = lda.fold, y = cv.lda[lda.fold, lda.model], col = "red", cex = 1.5, pch = 20)

# RDA
cv.rda_best <- which(cv.rda == min(cv.rda),  arr.ind = T);print(cv.rda_best);cv.rda[2, 5] # parsimonious; 0.1984222
rda.idx <- which(cv.rda == min(cv.rda), arr.ind = T)
rda.fold <- rda.idx[1]
rda.model <- rda.idx[2]

par(mfrow = c(1, 1));matplot(1:s, y = cv.rda, type = "b", xlab = "Number of folds", ylab = "Error rate",
                             col = cols, lty = 1, pch = 20, main = "Error rates for RDA");legend("topright",
                     legend = paste(names(formulas)), col = cols, cex = 0.75, lty = 1, pch = 20);points(
                       x = rda.fold, y = cv.rda[rda.fold, rda.model], col = "red", cex = 1.5, pch = 20)

# kNN
cv.knn_best <- which(cv.knn == min(cv.knn), arr.ind = T);print(cv.knn_best);cv.knn[4, 1] # best; 0.2622459 
knn.idx <- which(cv.knn == min(cv.knn), arr.ind = T)
knn.fold <- knn.idx[1]
knn.model <- knn.idx[2]

par(mfrow = c(1, 1));matplot(1:s, y = cv.knn, type = "b", xlab = "Number of folds", ylab = "Error rate",
                             col = cols, lty = 1, pch = 20, main = "Error rates for kNN");legend("topright",
                             legend = paste(names(formulas)), col = cols, cex = 0.75, lty = 1, pch = 20);points(
                               x = knn.fold, y = cv.knn[knn.fold, knn.model], col = "red", cex = 1.5, pch = 20)

results <- data.frame(
  Model = c("LOGIT", "LDA", "RDA", "kNN"),
  Accuracy = c(0.8054250, 0.8050337, 0.8015778, 0.7377541),
  Error = c(0.194575, 0.1949663, 0.1984222, 0.2622459),
  Formula = c("best_optimized", "best_optimized", "parsimonious", "best")
)

# NEXT --> PROBABILITIES