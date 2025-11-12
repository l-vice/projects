# LIBS -------------------------------------------------------------------- 

LoadLibraries <- function() {
  require(tidyverse) # general
  require(patchwork) # Combining plots for interactive effects
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

# Quick notes about the data:
# department binary classification: finishing_department = 1, sewing_department = 0
# Y: if (actual_productivity >= targeted_productivity) 1 else 0
# no_of_style_change: NO CHANGES = 0; ANY CHANGES = 1
# wip_sewing: NA = 0; we did this to measure the impact of wip on sewing_department (0).
dt1 <- read.csv("Data/CF1_data.csv")

dt1 <- dt1 %>%
  dplyr::mutate(
    date = as.POSIXct(date, tz = "UTC", format = "%m/%d/%Y"),
    week_in_month = factor(week_in_month),
    day = factor(day),
    team = factor(team)
  ) %>%
  dplyr::select(
    -date, -actual_productivity
  )

# I am removing actual_productivity since we used that to create Y, and is technically a numerical representation of Y.
# I am also removing "date" since its not necessary for modeling. We can use it later for time series if 
# we see an opportunity.

# Split into train - test, let's use 50-50. we perform a random split, replace = F has to be on so we can actually split
train <- sample(x = 1:nrow(dt1), size = nrow(dt1) * .51, replace = F)
test <- setdiff(x = 1:nrow(dt1), y = train)
length(train)
length(test)

# Let's split break down the factors into individual into individual integers 
x <- model.matrix(object = Y ~ ., data = dt1)[, -1]
y <- dt1$Y
colnames(x)
# This way its much simpler to track effects of actual teams so team1 is stong here, team2 here, etc...

# Let's drop the near-zero variance variables
# To my limited knowledge these variables can only hurt performance of the model since they have near constant features.
nzv <- nearZeroVar(x = x, saveMetrics = T)
nzv[nzv$nzv,] # week_in_monthweek_5, idle_time, idle_men
x <- x[, !nzv$nzv]
colnames(x)
?L0Learn.fit
# I want to save x and y as well
saveRDS(object = x, file = "predictors.rds")
saveRDS(object = y, file = "target.rds")
matrix.dt1 <- cbind(x, y)
df.dt1 <- as.data.frame(matrix.dt1)
# Let's save the dataframe 
write.csv(x = df.dt1, file = "dataframe_variables_task1.csv", row.names = F)
saveRDS(object = matrix.dt1, file = "matrix_variables_task1.rds")

###########################################################################
# Shrinkage Methods -------------------------------------------------------
###########################################################################

# Considering we need to start somewhere let's apply lasso, Elastic Net and best subset selection

###########################################################################
# LASSO ~~~~~ -------------------------------------------------------------
###########################################################################

set.seed(1)
grid <- 10^seq(10, -2, length.out = 100)
lasso.mod <- glmnet(x = x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)
cv.out <- cv.glmnet(x = x[train, ], y = y[train], alpha = 1)
plot(cv.out) # 4.5 --- 28
bestlam <- cv.out$lambda.min
lasso.pred <- predict(object = lasso.mod, s = bestlam, newx= x[test, ])
mean((lasso.pred - y[test])^2) # 0.169302

# Let's see what we get
out <- glmnet(x = x, y = y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:27, ]
print(lasso.coef)
data.frame(lasso.coef[lasso.coef != 0])

# Let's see what happens with lambda one SE away
set.seed(1)
selam <- cv.out$lambda.1se
lasso.pred <- predict(object = lasso.mod, s = selam, newx = x[test, ])
mean((lasso.pred - y[test])^2) # 0.1862655
out <- glmnet(x =x , y = y, alpha = 1, lambda = selam)
lasso.coef <- predict(out, type = "coefficients", s = selam)[1:27, ]
print(lasso.coef)
data.frame(lasso.coef[lasso.coef != 0]) # Lasso with SE_lam constraint 

###########################################################################
# ELASTIC NET -------------------------------------------------------------
###########################################################################

set.seed(1)
elastic.mod <- glmnet(x = x[train, ], y = y[train], alpha = 0.5, lambda = grid) 
plot(elastic.mod)
cv.out <- cv.glmnet(x = x[train, ], y = y[train], alpha = 0.5)
plot(cv.out)
bestlam <- cv.out$lambda.min
elastic.predict <- predict(object = elastic.mod, s = bestlam, newx = x[test, ])
mean((elastic.predict - y[test])^2) # 0.1665706 
out <- glmnet(x = x, y = y, lambda = grid)
elastic.coef <- predict(object = elastic.mod, s = bestlam, type = "coefficients")[1:27, ]
print(elastic.coef)
data.frame(elastic.coef[elastic.coef != 0])

elastic.coef <- predict(object = elastic.mod, s = bestlam, type = "coefficients")

# Let's see what happens with lambda one SE away
set.seed(1)
selam <- cv.out$lambda.1se
elastic.predict <- predict(object = elastic.mod, s = selam, newx = x[test, ])
mean((elastic.predict - y[test])^2) # 0.1867334
# Let's see what the variables are
out <- glmnet(x = x, y = y, alpha = 0.5)
elastic.coef <- predict(object = elastic.mod, s = selam, type = "coefficients")[1:27, ]
print(elastic.coef)
data.frame(elastic.coef[elastic.coef != 0])

# LOLearn -----------------------------------------------------------------

# How did i come across this package? I wanted to use another method for variable selection
# Considering regsubsets is strictly for regression, I had to look out for other tools.
# glm.best has a hard cap on 15 variables. I will try with LOLlearn  
# I consulted chatGPT for this considering its very hard to find at least for today's standards
# the replacement for bestglm online by just using Google. 
?L0Learn.cvfit # Obviously, i need to see whats up with the function and how it works
# y must be binary, x must be a data matrix, perfect. 
# there is an option for a logistic loss function. this is what we want
# Algorithm CD usually better solutions for investing a little more time - fine
# ok now nlambda i suppose this is the number of lambda's the function will compute??? let's read a little more about this  
# Ok after reading a little it seems I discovered a great function
# for nlambda, ngamma we can just supply a large grid and we are fine 10^seq(10, -2, lengh.out = 100) # this is great
# gammaMax, gammaMin these are basically the stability parameter limits we can control the min max values
# maxiters = it seems like this only applies to CD, rtol, atol,  - fine
grid <- rev(10^seq(10, -2, length.out = 1))
# I am not sure why the custom grid won't work rev() + list() makes it a list of decreasing positive values
# However, the issue is the length.out = 100, it seems like i can only feed the exact number for gamma after it's
# i figure out what the minimum value is and then refit. Obviously this is just a guess.
# The only way to move forward at this moment is to use the built in grid.
L0.fit <- L0Learn.fit(x = x[train, ], y = y[train], loss = "Logistic", penalty = "L0",
                       algorithm = "CD")

lams <- L0.fit$lambda[[1]] # Here we store all lambda values - you have a list.out = 100
# I did use chatGPT. In here i want to select the lowest lambda. so i was looking at ideas
# on how I can do that considering there is no function like cv.glmnet() where i can just
# extract the best lambda value. The initial idea was a log loss function but I decided to
# go for a confusion matrix. So basically i would look for the lowest error rate from 
# each iteration.
# rows: validation cases, columns: each lambda on the fitted path
set.seed(1)
# Let's extract the coefficients
# Ok, now I'm thinking of running a k-folds on this. there is a function specifically for this L0LearnCV.
# however I'd like to apply what I've learned and perform a manual CV over k folds.

k <- 10 # Let's just run 10 which is the base for most models
p <- length(lams)
lams <- L0.fit$lambda[[1]]
folds <- sample(rep(1:k, size = nrow(x), replace = F))
cv.errors <- matrix(NA, nrow = k, ncol = p, dimnames = list(NULL, paste(1:p)))

for (j in 1:k) {
  set.seed(1)
  L0.fit <- L0Learn.fit(x = x[folds != j, ], y = y[folds != j], loss = "Logistic", penalty = "L0", 
              algorithm = "CD", lambdaGrid = list(lams))
  probs <- predict(object = L0.fit, newx = x[folds == j, ], type = "response", lambda = lams)
  pred <- (probs >= 0.5) * 1
  cv.errors[j, ] <- colMeans(pred != y[folds == j])
}

mean_cv.errors <- colMeans(cv.errors) # We compute the mean CV error at each lambda across

###########################################################################

plot(1:100, y = mean_cv.errors, xlab = "Lambda", ylab = "Error", type = "b", main = "Error rate for lambda")
points(which.min(mean_cv.errors), min(mean_cv.errors), col = "red", cex = 1.5, pch = 20)

# This is a visual representation of the smallest lambda count.

###########################################################################

mean_cv.errors
# folds (lambda)
se_cv.errors <- apply(cv.errors, 2, sd) / sqrt(k)
# Compute the standard error at each lambda: SE(lambda) = sd(e1, ..., eK)/sqrt(K)
# bestlam from the k=10 fold setup, we find the argmin(lambda[mean_cv.errors])
bestlam <- lams[which.min(mean_cv.errors)] # the constraint is larger
# We define a threshold which is one SE distance from lambmin in the error space 
thr <- min(mean_cv.errors) + se_cv.errors[which.min(mean_cv.errors)]
# We take the maximum value from that space
bestlam.1se <- max(lams[mean_cv.errors <= thr])

# Let's now see the variable results we obtain.
L0.coeff <- as.matrix(coef(L0.fit, lambda = bestlam))[-1, ]
names(L0.coeff) <- colnames(x)
data.frame(L0.coeff[L0.coeff != 0])

# Ok now we have the variable recommendations from the best lambda possible
# Let's try to replicate the results
L0.coeff <- as.matrix(coef(L0.fit, lambda = bestlam.1se))[-1, ]
names(L0.coeff) <- colnames(x)
data.frame(L0.coeff[L0.coeff != 0])

# Ok now let's recollect and see what are variables were. So basically now the idea is
# to test out glm.fit with the variable recommendations made by lasso, elastic_net and subsets (or L0)

lasso_bestlam.vars <- c("week_in_monthweek_2", "week_in_monthweek_3", "department", "daySaturday", "daySunday",
                        "dayThursday", "team3", "team4", "team6", "team7", "team8", "team10", "team11", "team12",
                        "targeted_productivity", "smv", "wip", "incentive", "no_of_style_change")


lasso_bestlam.1se.vars <- c("department", "team8")

enet_bestlam.vars <- c("week_in_monthweek_3", "department", "daySaturday", "daySunday", "dayThursday",
                       "team2", "team3", "team4", "team5", "team6", "team7", "team8", "team10", "team11",
                       "targeted_productivity", "smv", "wip", "incentive", "no_of_style_change")

enet_bestlam.1se.vars <- c("department","team8")

subset_bestlam.vars <- c("week_in_monthweek_2", "week_in_monthweek_3", "department", "daySaturday", "daySunday",
                         "dayThursday", "dayTuesday", "dayWednesday", "team2", "team3", "team4", "team5", "team6",
                         "team7", "team8", "team9", "team10", "team11", "team12", "targeted_productivity", "smv",
                         "wip", "over_time", "incentive", "no_of_style_change", "no_of_workers") 

subset_bestlam.1se.vars <- c("department", "team8")

# Now we have the variable selection done it seems like all three method's agree on department and team8  so we will
# go for that. We can also go for lambda min recommendations but we will choose interpretability.

###########################################################################
# FINDING THE BEST MODEL --------------------------------------------------
###########################################################################

### DISCLAIMER!!! This section includes a lot of manual fitting.

# We have our baseline variables = department + team8. 
# In here, let's do two things, interpret log-odds for the baseline (dept., team8)
# and explore some other combinations. poly, log, box-cox, etc...
# I wont split the data into train/test subsets because i want the full data for 
# coefficient stability since interpretation is my goal.
set.seed(1);glm.fit <- glm(y ~ department + team8, 
                           family = "binomial", data = df.dt1);summary(glm.fit)

exp(cbind(OR = coef(glm.fit), confint(glm.fit)))
# We apply formulas 4.6 and 4.7 from the book
# Now let's try to increase the number of variables in a meaningful way. I'd
# say that lambda.1se is too conservative and lambda.min is too optimistic
# we need to find something in between. 

###########################################################################
# MODEL 1 - TIME EFFECTS --------------------------------------------------
###########################################################################

# week_in_month_dummies + day_dummies

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + week_in_monthweek_2 +
                             week_in_monthweek_3 + week_in_monthweek_4 +
                             daySaturday + daySunday + dayThursday + dayTuesday +
                             dayWednesday, family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit)

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + week_in_monthweek_3 +
                             daySunday + dayThursday, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit)

###########################################################################
# MODEL 2 - OPERATIONAL FACTORS -------------------------------------------
###########################################################################

# In here we will try all operational factors such as smv, targeted_productivity, no_of_workers, etc...

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + targeted_productivity +
                             smv + wip_sewing + over_time + incentive + no_of_style_change +
                             no_of_workers, 
                           family = "binomial", data = df.dt1);summary(glm.fit);vif(glm.fit)

# Let's drop no_of_workers

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + targeted_productivity +
                             smv + wip_sewing + over_time + incentive + no_of_style_change, 
                           family = "binomial", data = df.dt1);summary(glm.fit);vif(glm.fit)

# Let's drop smv

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + targeted_productivity + 
                             wip_sewing + over_time + incentive + no_of_style_change, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit)

# Let's drop wip_sewing

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + targeted_productivity +
                             over_time + incentive + no_of_style_change, 
                           family = "binomial", data = df.dt1);summary(glm.fit);vif(glm.fit)

# Let's drop targeted_productivity, over_time, and incentive

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + no_of_style_change,
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit)

# We dropped no_of_workers, smv, and wip_sewing because they were reducing the statistical
# significance of department (our core variable). Obviously, we could choose to 
# select a different one depending on our research question.

###########################################################################
# 3. INTERACTIONS ---------------------------------------------------------
###########################################################################

# Let's explore whether there are some hidden interactions between our variables,
# that could improve our model

# 1. Department and workforce factors -------------------------------------

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + department:no_of_workers, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit)

# department:no_of_workers - statistically significant, vif = ~5

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + department * targeted_productivity, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit)

# Not ideal, neither * or :, since the vif = +65, we will not consider this

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + department:incentive, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# There is a relationship between department * incentive, however MMCOL is +180, its too much

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + department:over_time, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# No relationship with either : or *, drop

# The only department workforce factor is department:no_of_workers but mmcol is barely
# acceptable.


# 2. Team Effects ---------------------------------------------------------

###########################################################################
# team8
set.seed(1);glm.fit <- glm(formula = y ~ department + team8 * incentive, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# team8 * incentive, has statistical significance

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 * over_time, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# No effect either * or :

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 * no_of_style_change, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# team8 * no_style_change has an effect which is stat. sign. and vif = ~1.5

###########################################################################

# team7

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team7 + team7:incentive, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# team7:incentive + team7, there is a statistical relationship and the vif is ~1 which is perfect.

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 * over_time, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# No relationship I assume we can stop testing relationships with over_time considering
# it proved to be statistically insignificant over many times.

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team7 * no_of_style_change, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# team7 * no_of_style_change, there is a stat.sign rela with ~1 vif

###########################################################################

# team2

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team2:incentive, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# There is a relationship between team2 * incentive but its MMCOL > ~9, it's too high

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team2 * no_of_style_change, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")
# team2 significance is not strong enough we drop

###########################################################################

# team5

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team5 * incentive, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# team5 effect too weak/no effect

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team5 * no_of_style_change, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

###########################################################################

# team6
set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team6 + team6:incentive, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# team6:incentive + team6, there is a stat. relationship + it's statistically significant

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team6 * no_of_style_change, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# team6 * no_of_style_change, there is no relationship that is stat. sign.

###########################################################################

# team9
set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team9 + team9:incentive, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# There is a relationship between team9:incentive although weak.

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team9 * no_of_style_change, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# No relationship

###########################################################################

# team10

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team10:incentive, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# There is no significant relationship that is not multicolinear

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team10:no_of_style_change, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# This is an interesting case, where team10:no_of_style_change is significant on its own
# however, team10 is insignificant on its own.

###########################################################################

# team11

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team11 + team11:incentive, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# There is a stat. relationship but its very weak. vif is ~1.5.

set.seed(1);glm.fit <- glm(formula = y ~ department + team8 + team11:no_of_style_change, 
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# There is no stat effect.

###########################################################################
# PUTTING IT ALL TOGETHER -------------------------------------------------
###########################################################################

# interactions
# Baseline: department, no_of_style_change, incentive - interactions no_of_workers, teams

set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + incentive +
                             department:no_of_workers + team2:incentive + team8:no_of_style_change +
                             team7:incentive + team7:no_of_style_change + team9:incentive +
                             team10:incentive + team10:no_of_style_change + team11:incentive,
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# add team8 (low-performance), add team7 (low performance)
set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + incentive + team7 + team8 + 
                             department:no_of_workers + team2:incentive + team8:no_of_style_change +
                             team7:incentive + team7:no_of_style_change + team9:incentive +
                             team10:incentive + team10:no_of_style_change + team11:incentive,
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# let's add team6 (low performance), and team11 (low performance)

set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + incentive + team6 + 
                             team7 + team8 + team11 +
                             department:no_of_workers + team2:incentive + team8:no_of_style_change +
                             team7:incentive + team7:no_of_style_change + team9:incentive +
                             team10:incentive + team10:no_of_style_change + team11:incentive,
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# add team10 (possibly weakest, training etc. required) and team9 (underperforms)

set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + incentive + team6 + 
                             team7 + team8 + team9 + team10 + team11 + 
                             department:no_of_workers + team2:incentive + team8:no_of_style_change +
                             team7:incentive + team7:no_of_style_change + team9:incentive +
                             team10:incentive + team10:no_of_style_change + team11:incentive,
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# We drop team10 since it doesn't add value on it's own, add team5

set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + incentive + 
                             + team5 + team6 + team7 + team8 + team9 + team11 + 
                             department:no_of_workers + team2:incentive + team8:no_of_style_change +
                             team7:incentive + team7:no_of_style_change + team9:incentive +
                             team10:incentive + team10:no_of_style_change + team11:incentive,
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")


# Ok now we have a set of teams that stand out and interactions within those teams
# before moving forward. I would like to center incentive to reduce VIF and see 

# Ok now let's remodel with the incentive_centered (It didn't help no point in running)

# set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + incentive_centered + 
#                             + team5 + team6 + team7 + team8 + team9 + team11 + 
#                             department:no_of_workers + team2:incentive_centered + team8:no_of_style_change +
#                             team7:incentive_centered + team7:no_of_style_change + team9:incentive_centered +
#                             team10:incentive_centered + team10:no_of_style_change + team11:incentive_centered,
#                          family = "binomial", 
#                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")


# It didn't give us immediate benefits so we must reduce our model to address mmcol

# Let's drop stat insignificant incentive interaction variables.
set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + incentive + 
                             + team5 + team6 + team7 + team8 + team9 + team11 + 
                             department:no_of_workers + team2:incentive + team8:no_of_style_change +
                             team7:incentive +
                             team10:incentive + team10:no_of_style_change,
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")


# This would be our final model explaining the team effects through department, style changes and incentives

###########################################################################
# TEMPORAL EFFECTS --------------------------------------------------------
###########################################################################

# Now we expand and include temporal effects such as  week_in_monthweek_2, daySaturday, etc...

set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + incentive + # main effects
                             + team5 + team6 + team7 + team8 + team9 + team11 + # team
                             department:no_of_workers + team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interations
                             team7:incentive + team10:incentive + team2:incentive + # incentive interactions
                             week_in_monthweek_2 + week_in_monthweek_3 + week_in_monthweek_4 + # temporal
                             daySaturday + daySunday + dayThursday + dayTuesday + dayWednesday,   
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

# All the previous effects remain, we just enriched the model with temporal effects. Let's polish it further

set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + incentive + # main effects
                             + team5 + team6 + team7 + team8 + team9 + team11 + # team
                             department:no_of_workers + team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interations
                             team7:incentive + team10:incentive + team2:incentive + # incentive interactions
                             week_in_monthweek_3 + # weeks
                             daySunday + dayThursday,   # days
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")


# This is the final model so far if we only look at interpretability. 

#########################################################################
# LINEARITY ASSUMPTION --------------------------------------------------
#########################################################################

set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + incentive + # main effects
                             + team5 + team6 + team7 + team8 + team9 + team11 + # team
                             department:no_of_workers + team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interations
                             team7:incentive + team10:incentive + team2:incentive + # incentive interactions
                             week_in_monthweek_3 + # weeks
                             daySunday + dayThursday,   # days
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")


# Let's test logit link assumption for each variable individually.
numerical_vars <- c("incentive", "wip_sewing", "no_of_workers", "smv", "targeted_productivity", "over_time")
par(mfrow = c(2, 3))

for (var in numerical_vars) {
  glm.fit <-glm(formula = as.formula(paste("y ~", var)), family = "binomial", data = df.dt1)
  logit <- predict(object = glm.fit, type = "link")
  
  plot(df.dt1[[var]], logit,
       main = paste("Logit vs", var), 
       xlab = var, ylab = "Logit")
  lines(lowess(df.dt1[[var]], logit), col = "red")
}

par(mfrow = c(1, 1))
# Univariate fit is fine, for all numerical variables.
 

# Now let's test this on the interpretable model

# We will use car::residualPlots() since that is more appropriate for multivariate checks

set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + incentive + no_of_workers + smv + over_time + wip_sewing + # main effects
                 + team5 + team6 + team7 + team8 + team9 + team11 + # team
                 department:no_of_workers + team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
                 team7:incentive + team10:incentive + team2:incentive + # incentive interactions
                 week_in_monthweek_3 + # weeks
                 daySunday + dayThursday,   # days
               family = "binomial", 
               data = df.dt1);summary(glm.fit);vif(glm.fit)

residualPlots(glm.fit, terms = ~ incentive + no_of_workers + smv + over_time + wip_sewing)

# The residualPlots output indicates that smv and incentive violate the logit link assumption. 
# These variables therefore require transformation.
# The plots from residualPlots() are not fully informative in this case, 
# so I will manually inspect fitted vs. logit plots with lowess smoothing to guide the choice of transformations.
# For incentive, the distribution is heavily skewed with several extreme 
# values and many observations close to zero. Since the variable contains zeros, a log(incentive + 1) 
# transformation is appropriate to reduce skewness and stabilize variance.
# For smv, the transformation choice is less obvious. I will examine the lowess curve to determine 
# whether a logarithmic or polynomial (e.g., quadratic) term provides a better fit.

numerical_vars <- c("incentive", "wip_sewing", "no_of_workers", "smv", "over_time")

par(mfrow = c(2, 3))

for (var in numerical_vars) {
  glm.fit <- glm(formula = y ~ incentive + wip_sewing + no_of_workers + smv + over_time, family = "binomial", data = df.dt1)
  logit <- predict(object = glm.fit, type = "link")
  
  plot(df.dt1[[var]], logit,
       main = paste("Logit vs", var),
       xlab = var, ylab = "Logit")
  lines(lowess(df.dt1[[var]], logit), col = "red")
}
par(mfrow = c(1, 1))

# Recommendations: (Visual + residPlots() output)

# incentive: log(incentive + 1) and a quadratic term even so log1p(incentive) + log1p(incentive)^2 so we capture skew and curvature.
# smv: log or quadratic - we will proceed with quadratic
# no_of_workers: mild quadratic
# over_time: sqrt considering the skew is on the left side

# Let's see the distribution of values for each of the variables

par(mfrow = c(2, 3))

for (var in numerical_vars) {
  hist(df.dt1[[var]],
       main = paste("Distribution of", var),
       xlab = var,
       col = "blue",
       border = "white")
}

par(mfrow = c(1, 1))

# OK now we look to apply transformations. 
df.dt1.temp <- df.dt1
df.dt1.temp <- df.dt1.temp %>%
  dplyr::mutate(
    log1p_incentive = log1p(incentive),
    smv_squared = smv^2,
    sqrt_over_time = sqrt(over_time),
    no_of_workers_squared = no_of_workers^2
  )
transformed_vars <- c("log1p_incentive", "wip_sewing", "no_of_workers_squared", "smv_squared", "sqrt_over_time")
# Transformations:
#   1. Incentive: log1p(incentive) + I(log1p(incentive^2))
#   2. smv: smv^2
#   3. over_time: sqrt(over_time), log1p(over_time)
#   4. no_of_workers: no_of_workers^2

par(mfrow = c(2, 3))

for (var in transformed_vars) {
  glm.fit <- glm(formula = y ~ log1p_incentive + wip_sewing + smv_squared + sqrt_over_time + no_of_workers_squared, 
                 family = "binomial", data = df.dt1.temp)
  logit <- predict(object = glm.fit, type = "link")
  
  plot(df.dt1.temp[[var]], logit, 
       main = paste("Logit vs", var),
       xlab = var, ylab = "Logit")
  lines(lowess(df.dt1.temp[[var]], logit), col = "red")
}
  
par(mfrow = c(1, 1))

# Let's check the residualPlots() output

set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + log1p(incentive) + I(no_of_workers^2) + sqrt(over_time) + # main effects
                             team5 + team6 + team7 + team8 + team9 + team11 + # team
                             department:no_of_workers + team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
                             team7:log1p(incentive) + team10:log1p(incentive) + team2:log1p(incentive) + # incentive interactions
                             I(smv^2) + wip_sewing + # productivity 
                             week_in_monthweek_3 + # weeks
                             daySunday + dayThursday,   # days
                          family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit)


residualPlots(model = glm.fit, terms = ~ log1p(incentive) + I(no_of_workers^2) + I(smv^2) + sqrt(over_time) + wip_sewing)
# log1p(incentive)     15.8805        6.747e-05 ***
# I(no_of_workers^2)    6.7007         0.009637 ** 
# I(smv^2)              3.5043         0.061211 .  
# sqrt(over_time)       0.9802         0.322145    
# wip_sewing            0.7828         0.376287    

# Next:
# incentive: log1p(incentive) + I(log1p(incentive)^2)
# no_of_workers: no_of_workers + I(no_of_workers^2)
# smv: check smv + I(smv^2); I(smv^2) + I(smv^3); I(log1p(smv)^2)

set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + log1p(incentive) + I(log1p(incentive)^2) + no_of_workers + I(no_of_workers^2) + sqrt(over_time) +# main effects
                           team5 + team6 + team7 + team8 + team9 + team11 + # team
                           department:no_of_workers + team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
                           team7:log1p(incentive) + team10:log1p(incentive) + team2:log1p(incentive) + # incentive interactions
                           I(log1p(smv)^2) + wip_sewing + # productivity 
                           week_in_monthweek_3 + # weeks
                           daySunday + dayThursday, # days
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit)

residualPlots(glm.fit, terms = ~ log1p(incentive) + I(log1p(incentive)^2) + no_of_workers + 
                I(no_of_workers^2) + I(log1p(smv)^2) + sqrt(over_time) + wip_sewing
              )

# Next:
# incentive: ... + I(log1p(incentive)^3)
# smv: I(smv^2)

set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + log1p(incentive) + I(log1p(incentive)^2) + sqrt(over_time) +# main effects
                             team5 + team6 + team7 + team8 + team9 + team11 + # team
                             department:no_of_workers + team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
                             team7:log1p(incentive) + team10:log1p(incentive) + team2:log1p(incentive) + # incentive interactions
                             I(smv^2) + I(smv^3) + wip_sewing + log(no_of_workers) + # productivity 
                             week_in_monthweek_3 + # weeks
                             daySunday + dayThursday, # days
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

residualPlots(glm.fit, terms = ~ log1p(incentive) + I(log1p(incentive)^2) + log(no_of_workers) + I(smv^2) + I(smv^3) + sqrt(over_time) + wip_sewing
)

# Ok we are fine now we satisfied the logit link assumption however MMCOL is extremely high.
# Let's first try to orthogonal polynomials first

# Next steps
# Polynomials: Orthogonal polynomials through poly()

set.seed(1);glm.fit <- glm(formula = y ~ department + no_of_style_change + poly(log1p(incentive), degree = 2) + sqrt(over_time) + # main effects
                             team5 + team6 + team7 + team8 + team9 + team11 + # team
                             team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
                             team7:log1p(incentive) + team10:log1p(incentive) + team2:log1p(incentive) + # incentive interactions
                             poly(smv, 2) + wip_sewing + department:no_of_workers + # productivity 
                             week_in_monthweek_3 + # weeks
                             daySunday + dayThursday, # days
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit);vif(glm.fit, type = "predictor")

set.seed(1);glm.fit.nodept <- glm(formula = y ~ no_of_style_change + poly(log1p(incentive), degree = 2) + sqrt(over_time) + # main effects
                             team5 + team6 + team7 + team8 + team9 + team11 + # team
                             team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
                             team7:log1p(incentive) + team10:log1p(incentive) + team2:log1p(incentive) + # incentive interactions
                             poly(smv, 2) + wip_sewing + department:no_of_workers + # productivity 
                             week_in_monthweek_3 + # weeks
                             daySunday + dayThursday, # days
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit.nodept);vif(glm.fit.nodept, type = "predictor")

BIC(glm.fit, glm.fit.nodept)
anova(glm.fit, glm.fit.nodept)
residualPlots(glm.fit, terms = ~ poly(log1p(incentive), degree = 2) + poly(smv, 2) + sqrt(over_time) + wip_sewing
)

residualPlots(glm.fit.nodept, terms = ~ poly(log1p(incentive), degree = 2) + poly(smv, 2) + sqrt(over_time) + wip_sewing
)

range(df.dt1$over_time)
set.seed(1);glm.fit <- glm(
  formula = y ~ department + no_of_style_change + log1p(incentive) + wip_sewing + # main effects 
    team5 + team6 + team7 + team8 + team9 + team11 + team10 + team12 + # team
    department:no_of_workers + team8:no_of_style_change + team10:no_of_style_change + # no of style change interactions 
    team7:log1p(incentive) + team10:log1p(incentive) + # incentive interactions
    I(smv^2) + I(smv^2):team5 + I(smv^2):team12 + # productivity
    week_in_monthweek_3 + # weeks
    dayThursday, # days
  family = "binomial",
  data = df.dt1
);summary(glm.fit);vif(glm.fit, type = "predictor")

###########################################################################
# FINAL MODELS ------------------------------------------------------------
###########################################################################

###########################################################################
# BEST MODELS -------------------------------------------------------------
###########################################################################

# Best possible model (to my ability and the scope of the course) - It's very hard to interpret

set.seed(1);glm.best <- glm(formula = y ~ no_of_style_change + poly(log1p(incentive), degree = 2) + sqrt(over_time) + # main effects
                                    team5 + team6 + team7 + team8 + team9 + team11 + # team
                                    team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
                                    team7:log1p(incentive) + team10:log1p(incentive) + team2:log1p(incentive) + # incentive interactions
                                    poly(smv, 2) + wip_sewing + department:no_of_workers + # productivity 
                                    week_in_monthweek_3 + # weeks
                                    daySunday + dayThursday, # days
                                  family = "binomial", 
                                  data = df.dt1);summary(glm.best);vif(glm.best, type = "predictor")

set.seed(1);glm.best_low <- glm(formula = y ~ no_of_style_change + poly(log1p(incentive), degree = 2) + sqrt(over_time) + # main effects
                               team6 + team7 + team8 + team9 + team11 + # team
                               team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
                               team7:log1p(incentive) + # incentive interactions
                               poly(smv, 2) + wip_sewing + department:no_of_workers + # productivity 
                               week_in_monthweek_3 + # weeks
                               daySunday + dayThursday, # days
                             family = "binomial", 
                             data = df.dt1);summary(glm.best_low);vif(glm.best_low, type = "predictor")

###########################################################################
# INTERPRETABILITY --------------------------------------------------------
###########################################################################

# The most interpretable model
# The idea behind this model is to actually offer some kind of feedback to management.
# We can easily interpret the log-odds and general interactions between variables, we can interpret how teams react to style changes
# for example, we see that the probability of success drops to 20% relative to the baseline and we can see that the probability of success
# even when they are incentive drops marginally. The management should look into the performance of team10 

set.seed(1);glm.fit.clear <- glm(formula = y ~ department + no_of_style_change + incentive + # main effects
                             team6 + team7 + team8 + team9 + team10 + team11 + # team
                             department:no_of_workers + team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
                             team7:incentive + team10:incentive + team2:incentive + # incentive interactions
                             wip_sewing + # productivity
                             week_in_monthweek_3 + # weeks
                             daySunday + dayThursday, # days
                           family = "binomial", 
                           data = df.dt1);summary(glm.fit.clear);vif(glm.fit.clear, type = "predictor")

# This model is a little less interpretable but resolves the issue of mmcol, so at least one Logistic Regression assumption is met.
# All the variables are <10. 
set.seed(1);glm.fit.clear_1as <- glm(formula = y ~ department + no_of_style_change + log1p(incentive) + # main effects
                                   team6 + team7 + team8 + team9 + team10 + team11 + # team
                                   department:no_of_workers + team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
                                   team7:log1p(incentive) + team10:log1p(incentive) + team2:log1p(incentive) + # incentive interactions
                                   wip_sewing + # productivity
                                   week_in_monthweek_3 + # weeks
                                   daySunday + dayThursday, # days
                                 family = "binomial", 
                                 data = df.dt1);summary(glm.fit.clear_1as);vif(glm.fit.clear_1as, type = "predictor")

###########################################################################
# PARSIMONY ---------------------------------------------------------------
###########################################################################

# Let's also add the most parsimonious model which was suggested by lasso, elastic net and best-subsets

set.seed(1);glm.fit.parsi <- glm(formula = y ~ department + team8, 
                                 family = "binomial", 
                                 data = df.dt1);summary(glm.fit.parsi);vif(glm.fit.parsi)

# These are our final models. So we have a combination of the best statistical fit and most interpretability.

# Let's do quickly BIC, AIC and anova tests just to wrap everything up
BIC(object = glm.best, glm.best_low, glm.fit.clear, glm.fit.clear_1as, glm.fit.parsi)
AIC(object = glm.best, glm.best_low, glm.fit.clear, glm.fit.clear_1as, glm.fit.parsi)
anova(object = glm.best, glm.best_low, glm.fit.clear, glm.fit.clear_1as, glm.fit.parsi) # 3, 5

# Let's save the names of these models in an object so we don't go back constantly.
models <- list(glm.best = glm.best, 
               glm.best_low = glm.best_low, # without team5 + team10:log1p(incentive) + team2:log1p(incentive)
               glm.fit.clear = glm.fit.clear, 
               glm.fit.clear_1as = glm.fit.clear_1as, 
               glm.fit.parsi = glm.fit.parsi
               )

# let's also create formula's so we can fit the functions when required
formulas <- list(
  # Best mod
  best = y ~ no_of_style_change + poly(log1p(incentive), degree = 2) + sqrt(over_time) + # main effects
    team5 + team6 + team7 + team8 + team9 + team11 + # team
    team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
    team7:log1p(incentive) + team10:log1p(incentive) + team2:log1p(incentive) + # incentive interactions
    poly(smv, 2) + wip_sewing + department:no_of_workers + # productivity 
    week_in_monthweek_3 + # weeks
    daySunday + dayThursday,
  # Best mod (3 insignificant features dropped)
  best_optimized = y ~ no_of_style_change + poly(log1p(incentive), degree = 2) + sqrt(over_time) + # main effects
    team6 + team7 + team8 + team9 + team11 + # team
    team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
    team7:log1p(incentive) + # incentive interactions
    poly(smv, 2) + wip_sewing + department:no_of_workers + # productivity 
    week_in_monthweek_3 + # weeks
    daySunday + dayThursday,
  # Interpretable model
  interpretable = y ~ department + no_of_style_change + incentive + # main effects
    team6 + team7 + team8 + team9 + team10 + team11 + # team
    department:no_of_workers + team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
    team7:incentive + team10:incentive + team2:incentive + # incentive interactions
    wip_sewing + # productivity
    week_in_monthweek_3 + # weeks
    daySunday + dayThursday,
  # Interpretable model but with log1p transformation to address high leverage observations and control MMCOL
  interpretable_log = y ~ department + no_of_style_change + log1p(incentive) + # main effects
    team6 + team7 + team8 + team9 + team10 + team11 + # team
    department:no_of_workers + team8:no_of_style_change + team10:no_of_style_change + # no_of_style_change interactions
    team7:log1p(incentive) + team10:log1p(incentive) + team2:log1p(incentive) + # incentive interactions
    wip_sewing + # productivity
    week_in_monthweek_3 + # weeks
    daySunday + dayThursday,
  # Parsimonious model; the model proposed by lasso, e-net, and subsets
  parsimonious = y ~ department + team8
)

class(formulas[[1]]) # formula, we are good

# saveRDS(object = models, file = "top_models.rds")
# saveRDS(object = formulas, file = "model_formulas.rds")

# Next --> INTERACTIONS