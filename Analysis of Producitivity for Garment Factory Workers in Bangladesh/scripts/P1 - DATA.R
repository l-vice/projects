
###########################################################################
# PACKAGES ----------------------------------------------------------------
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
# DATA ANALYSIS -----------------------------------------------------------
###########################################################################

df <- read.csv(file = "Data/g_prod.csv")
str(df)
glimpse(df) # we can do either one of these. the idea is to just get a "glimpse" of data, we spot NA's...
sum(is.na(df)) # 506
remove_na <- function(data) if (anyNA(data)) na.omit(data) else data
506/nrow(df) # We can't use the function above because we would lose 42% of our 
# observations from a data set that is already very small. We need to look for alternative measures.
# One idea is simply drop wip variable from the data set, but let's see if we can keep its predictive power.

# While we think about possible solutions let's quickly convert chr to appropriate formats (dates and multi-level factors)
# we should also consider different transformations but for now let's just see how many levels there are.
df$date[1:50] # I want to have a look at the format
df <- df %>%
  dplyr::mutate(
    date = as.POSIXct(trimws(date), tz = "UTC", format = "%m/%d/%Y"),
    quarter = factor(quarter),
    department = factor(department),
    day = factor(day)
  )
str(df)

# for date i had to get help from ChatGPT due to unknown formatting issues. The function that solved my issue
# is trimws. I could also go online and try to find a solution or open excel and inspect but it's much faster this way.
?trimws # I read about the function and it's general applications, and store it in my memory for future.

###########################################################################
# TRANSFORMATIONS ---------------------------------------------------------
###########################################################################

###########################################################################
# FACTORS -----------------------------------------------------------------
###########################################################################

# Now I want to inspect the factor levels
factors <- c("quarter", "department", "day")
lapply(df[factors], levels)

# Ok so now I know that workers work for 6 days a week and that there are 5 quarters????
30/6 # My intuition was correct. Quarter is basically an ordered week factor 1, 2, 3, 4, 5
# so there could be latent predictive power in this. Workers might be more productive in the last
# week than the first week of the month. 

# We also gain information about the number of departments. We know that there are two deparments
# sweing and finishing
# Dictionary: sweing???
# sweing: This word does not exist, so i will assume it's a sewing department within a Garment Factory.

# Let's try to organize these factors a little better

df <- df %>%
  dplyr::rename(
    week_in_month = quarter
  ) %>% 
  dplyr::mutate(
    department = fct_recode(
      department,
      sewing_department = "sweing",
      finishing_department = "finishing"
    ),
    week_in_month = fct_recode(week_in_month,
                         week_1 = "Quarter1",
                         week_2 = "Quarter2",
                         week_3 = "Quarter3",
                         week_4 = "Quarter4",
                         week_5 = "Quarter5"
                         ),
    week_in_month = fct_relevel(week_in_month, "week_1") # baseline for logit
  )

df$department[1:20] # Let's check if its all good. It is we have two unique levels: sewing_department, finishing_department
df$week_in_month[1:20] # Let's check this one, we have 5 levels week_1 .... week_5
str(df)

###########################################################################
# DEPARTMENT --------------------------------------------------------------
###########################################################################

# We also convert department to 0 and 1 since binary 0, 1 encoding is usually better for two level factors.
# We can also rely on model.matrix() function to do this for us but we will do this manually
# to ensure control in interpretation. so Sewing = 0, Finishing = 1.
# finishing_department = 1, sewing_department = 0
department <- rep(0, 1197)
department[df$department == "finishing_department"] = 1
department <- as.integer(department)
class(department)
table(department)

###########################################################################
# NUMERICAL ---------------------------------------------------------------
###########################################################################

# Now we have sorted factors. Let's check out the numeric variables
numerical <- c("team", "targeted_productivity", "smv", "wip", "over_time", 
               "incentive", "idle_time", "idle_men", "no_of_style_change",
               "no_of_workers", "actual_productivity"
               )

range_numerical <- data.frame(apply(df[numerical], 2, range))
print(range_numerical)

# team: 1 - 12, this could be a factor
# targeted_productivity: 0.07 - 0.8? 0.07 is a little questionable since having 7% as a target makes no sense and you might as well give 
  # people a day off so they are happier and more productive the next day.
# We tried to see what 0.07 target_prod was but it seems it was something related specifically to team 7. i checked any dates, 
# events or external factors that could influence this but it seems like there aren't any. I am going to say that this is a typo.

# smv: standard-minute-value, so the allocated time for a task (minutes per unit of work)
# standard minute value: allocated time for a task - minutes per units of work - fine 
# i don't see anything out of the ordinary in here. However, we could convert to seconds since it would be more precise.

# over_time: OT in minutes by the whole team, range is fine. However, we could convert this to hours or even seconds but let's see.

# incentive: in BDT, this is the incentive the factory is providing it's employees for overtime work
# I would convert this value to EUR or USD considering we do not have an immediate quantifiable perception 
# of what the value of BDT is. Some of the questions we need to ask ourselves is:
# What is the hourly overtime rate? is there an incentive if you actually achieve or overachieve your
# target_productivity... and many more but let's see what we come up with along our analysis.

# idle_time: The amount of time in minutes the production was interrupted for several/unknown reasons.
# This is 5 hours in total, we could convert to hours. again let's think about it a little more since its not necessary.

# idle_men: Number of workers who were idle for various/unknown reasons. This is a count so fine.

# no_of_syle_change: Number of changes in the style of a particular product, this could be a factor? there could be some relationships in here
# with other variables. but considering we will use classification it T1 it could be irrelevant. let's see.

# no_of_workers: total number of workers in the team. We could use this to see how many workers there are per team (obviously).

# actual_productivity: This is the % of expected_productivity??? what is the expected productivity...
# There is no expected_productivity value in our data set. However, we can safely assume that both actual_productivity and
# targeted_productivity are created relative to the same conceptual variable called expected_productivity. 
# We need this data so we can explore the option of creating a dummy variable with the following logic:
# if (actual_productivity >= expected_productivity) 1 else 0

###########################################################################
# targeted_productivity ---------------------------------------------------
###########################################################################

df.temp <- df
which.min(df.temp$targeted_productivity)
df.temp$targeted_productivity[634] <- as.numeric(0.7)
# I am fairly certain that 0.07 is an error. I did not find any external factors for which the value could be reduced to 7%
# for that day. Furthermore, unlike team 7, other teams did not have their targets reduced and there were no product style changes, which indicates
# that this is either an error or a case  where an unknown external factor led management to set a .07 target for the day. 
# Considering that the latter is highly unlikely, we will consider the former to be the true.   

targeted_productivity <- df.temp$targeted_productivity
which.min(targeted_productivity)
targeted_productivity[147]
rm(df.temp)

###########################################################################
# Target Variable - Y -----------------------------------------------------
###########################################################################

# Y variable
# While inspecting the data set in excel when I initially opened the file just to see what's in there I observed that
# some values are very close to the gap.
df.temp <- df
df.temp$targeted_productivity <- targeted_productivity
gap <- df.temp$actual_productivity - df.temp$targeted_productivity
sum(abs(gap) <  0.005, na.rm = T) # 414
sum(abs(gap) < 0.001, na.rm = T) # 402 
sum(abs(gap) < 0.0001, na.rm = T) # 76
sum(abs(gap) < 0.00001, na.rm = T) # 8
sum(abs(gap) < 1e-6, na.rm = T) # 6

# The best way to resolve this is just to simply round actual_productivity to two decimals and call it.
# Let's do that here: we will just prepare the variable now.
df.temp <- df.temp %>%
  mutate(
    actual_r2 = round(actual_productivity, 2),
    target_r2 = round(targeted_productivity, 2),
    Y = as.integer(actual_r2 >= target_r2)
  )

Y <- df.temp$Y # We only want to store our target Y we will later just add it to our df

table(df.temp$Y) # 0 unperformed; 1 achieve performance target
303/1197 # .25 or ~25% of the days workers did not outperform the target set for the day

rm(df.temp, gap) # We remove the temporary files 

###########################################################################
# no_of_workers -----------------------------------------------------------
###########################################################################

df.temp <- df
df.temp$targeted_productivity <- targeted_productivity
df.temp$Y <- Y
mean(df.temp$no_of_workers) # 34.60986 mean

# I noticed that some rows have 30.5 workers, this is obviously not possible since a worker 1 worker is not equal to 8 hours.
# Let's fix that and round up the numbers.
is_nonint <- function(x) !is.na(x) & abs(x - round(x))

df.temp <- df.temp %>%
  mutate(
    workers_fractional = as.integer(is_nonint(no_of_workers)),
    no_of_workers_int = floor(no_of_workers + 0.5)
  )

# Logistic Regression

set.seed(1);logit.num <- glm(Y ~ targeted_productivity + smv + no_of_workers +
                 workers_fractional + over_time + idle_time + idle_men +
                 department + week_in_month + day,
               data = df.temp[1:800, ], family = "binomial")

set.seed(1);logit.int <- glm(Y ~ targeted_productivity + smv + no_of_workers_int +
                   workers_fractional + over_time + idle_time + idle_men +
                   department + week_in_month + day,
                 data = df.temp[1:800, ], family = "binomial")

anova(logit.num, logit.int)
BIC(logit.num, logit.int)

# Linear Regression

set.seed(1);identity.num <- lm(Y ~ targeted_productivity + smv + no_of_workers +
                     workers_fractional + over_time + idle_time + idle_men +
                     department + week_in_month + day,
                    data = df.temp[1:800, ])

set.seed(1);identity.int <- lm(Y ~ targeted_productivity + smv + no_of_workers_int +
                     workers_fractional + over_time + idle_time + idle_men +
                     department + week_in_month + day,
                   data = df.temp[1:800, ])

anova(identity.int, identity.num)
BIC(identity.int, identity.num)

# how many rows are non-rounded
sum(df.temp$no_of_workers_int != df.temp$no_of_workers) # 140
140/nrow(df) # 11.6% that's quite a bit. 

# let's go a step further and compare predictions for Linear Regression
set.seed(1);identity.int_pred <- predict(identity.int, newdata = df.temp[801:1197, ], type = "response")
set.seed(1);identity.num_pred <- predict(identity.num, newdata = df.temp[801:1197, ], type = "response")
max(abs(identity.num_pred - identity.int_pred)) # 1.099121e-14

# Let's compare the predictions for Logistic Regression
set.seed(1);logit.int_pred <- predict(logit.int, newdata = df.temp[801:1197, ], type = "response")
set.seed(1);logit.num_pred <- predict(logit.num, newdata = df.temp[801:1197, ], type = "response")
max(abs(logit.num_pred - logit.int_pred)) # 1.237899e-14

# These results are exactly what we wanted. So basically, it's obvious that these are employees that worked
# half a day. A more appropriate way of reporting this would be as let's say total_hours_worked, where we would
# have the information of how much work in hours was done that day. Also it's true that we could compute that ourselves
# by simply multiplying the number of workers by the number of hours in the workday. However, the issue with this logic
# is that we don't have that information. Assuming a standard 8 hour shift is very risky considering that our target group/customer is a
# manufacturing business in Bangladesh. There is a high probability that these factories run >8hrs, and even more than 12hr
# with multiple shifts to meet the demand. For that reason, we will simply round to the higher number since 30.5 workers must equal to 31 persons who worked in a specific department
# on that day in that week/month/year. We choose transparency over more information.
no_of_workers <- as.integer(floor(df$no_of_workers + .5)) # We store this variable separately
no_of_workers[1:150]
mean(no_of_workers) # I double check the mean here
mean(df.temp$no_of_workers)

rm(df.temp, identity.int, identity.num, identity.int_pred, identity.num_pred, 
   logit.int, logit.num, logit.int_pred, logit.num_pred, is_nonint) # We remove the temporary objects

#############################################################################
# no_of_style_change  -------------------------------------------------------
#############################################################################

# Now i need to investigate further no_style_changes since it's a little unclear the magnitude effect this variable has
table(df$no_of_style_change)
prop.table(table(df$no_of_style_change))
aggregate(smv ~ no_of_style_change, data = df, median) # it seems like we can merge both of them since the difference on smv is not too high
aggregate(targeted_productivity ~ no_of_style_change, data = df, median) # expected
aggregate(actual_productivity ~ no_of_style_change, data = df, median) # this shows that they actually affect productivity
# it seems like the productivity is higher when there are two style changes instead of one. This further supports the idea of just
# combining/reducing this factor to two-levels. Why? because it seems like this variable has a "shock" effect on the worker productivity
# so there isn't much difference whether its 1 or 2.
with(df, table(department, no_of_style_change)) # This is expected as well considering people who work in the sewing dept. 
# need to make these changes.

# Let's investigate this further - we can actually test the effect of whether this improves our model by simply fitting
# a linear regression model and comparing the two with anova and BIC
df.temp <- df
df.temp$style3 <- factor(df$no_of_style_change, levels = c(0, 1, 2), labels = c("0", "1", "2"))
df.temp$style2 <- factor(as.integer(df$no_of_style_change > 0), levels = c(0, 1), labels = c("0", "1+"))
df.temp$targeted_productivity <- targeted_productivity
df.temp$Y <- Y

which.min(df.temp$targeted_productivity)

# Linear Regression

set.seed(1);identity.m3 <- lm(actual_productivity ~ style3 + targeted_productivity + smv + idle_time +
           idle_men + no_of_workers + department + week_in_month + day, data = df.temp)

set.seed(1);identity.m2 <- lm(actual_productivity ~ style2 + targeted_productivity + smv + idle_time +
           idle_men + no_of_workers + department + week_in_month + day, data = df.temp)

anova(identity.m3, identity.m2)
BIC(identity.m3, identity.m2)

# Logistic Regression

set.seed(1);logit.m3 <- glm(Y ~ style3 + targeted_productivity + smv + idle_time +
                  idle_men + no_of_workers + department + week_in_month + day, data = df.temp, family = "binomial") 
  
set.seed(1);logit.m2 <- glm(Y ~ style2 + targeted_productivity + smv + idle_time +
                  idle_men + no_of_workers + department + week_in_month + day, data = df.temp, family = "binomial")

anova(logit.m3, logit.m2)
BIC(logit.m3, logit.m2) # Same result, again we are safe to combine Levels 2, 3 (1, 2) because of the "shock" effect. 

# Both anova and BIC support our initial decision. We should apply the logic 
# We will test this once more for our log. regression, these results are not irrelevant as we plant to flip the Y from binary in Task 1 to numerical for Task 2
no_of_style_change <- rep(0, 1197)
no_of_style_change[df.temp$no_of_style_change > 0] = 1
no_of_style_change <- as.integer(no_of_style_change)
table(no_of_style_change)
147/1197 # ~12% - Style Changes
1050/1197 # ~88% No Style Changes

rm(df.temp, identity.m3, identity.m2, logit.m2, logit.m3)

###########################################################################
# Incentive ---------------------------------------------------------------
###########################################################################

# My though was to convert the values from BDT to EUR or USD, just so we gauge magnitude of incentive.

range(df$incentive)
sum(df$incentive == 0)
table(df$incentive)


# Incentives are recorded in Bangladeshi Taka (BDT). We report and model them in BDT 
# Because local currency preserves scale and context (e.g., how an incentive compares to typical monthly pay in Bangladesh). 
# Converting to EUR would only rescale the variable and make amounts look deceptively 
# small, without improving model performance. For reader transparency, there is a possibility that
# we later add a side note with an approximate EUR conversion, but we will refrain from converting
# to EUR for modeling purposes.

# Monthly pay in Bangladesh in 2015: 5300.00 BDT/Month in 2015. source: Employment Ministry. 

# Final verdict: Keep the value in BDT since having it in local currency is more beneficial due to the location
# of the factory.

###########################################################################
# idle_men ----------------------------------------------------------------
###########################################################################

# Quick distribution glimpse
barplot(table(df$idle_men), main = "Barplot of idle_men")
sum(df$idle_men == 0) # 1179
1179/1197 # .98; 98% of observations for idle_men is zero. let's see if we can do something about it
(1197-1179)/1197 # 1.5% are non-zero. This is a candidate for removal but let's see maybe we can do something
# Let's prepare a temporary df
df.temp <- df
df.temp$targeted_productivity <- targeted_productivity
df.temp$no_of_workers <- no_of_workers
df.temp$no_of_style_change <- no_of_style_change
df.temp$Y <- Y

# Let's see if we can salvage this variable by converting it to 0 and 1
# Let's say that we want the var to equal 1 when there is idle_men and 0 otherwise
# The aim is to capture and see whether idle_men is a meaningful event
df.temp <- df.temp %>%
  dplyr::mutate(
    idle_any = as.integer(idle_men > 0)
  )

# I am using dplyr::mutate in case I come up with something that i'd like to add along the way
# otherwise just doing it directly df.temp$idle_any = as.integer(df.temp$idle_men > 0) is faster.

# Linear Regression

set.seed(1);identity.g01 <- lm(Y ~ targeted_productivity + smv + no_of_workers +
                    over_time + idle_time + department + week_in_month + day + idle_any,
                  data = df.temp)

set.seed(1);identity.g02 <- lm(Y ~ targeted_productivity + smv + no_of_workers +
                       over_time + idle_time + department + week_in_month + day + idle_men,
                   data = df.temp)

anova(identity.g01, identity.g02)
BIC(identity.g01, identity.g02)

# Logistic Regression

set.seed(1);logit.g01 <- glm(Y ~ targeted_productivity + smv + no_of_workers +
                  over_time + idle_time + department + week_in_month + day + idle_any,
                data = df.temp, family = "binomial")

set.seed(1);logit.g02 <- glm(Y ~ targeted_productivity + smv + no_of_workers +
                  over_time + idle_time + department + week_in_month + day + idle_men,
                data = df.temp, family = "binomial")

anova(logit.g01, logit.g02)
BIC(logit.g01, logit.g02)

# So adding 1 and 0's does not improve our model. Let's see what would happen if we remove the variable.

set.seed(1);identity.g00 <- lm(Y ~ targeted_productivity + smv + no_of_workers +
                     over_time + idle_time + department + week_in_month + day,
                   data = df.temp)

set.seed(1);logit.g00 <- glm(Y ~ targeted_productivity + smv + no_of_workers +
                  over_time + idle_time + department + week_in_month + day,
                data = df.temp, family = "binomial")

anova(logit.g00, logit.g01, logit.g02)
BIC(logit.g00, logit.g01, logit.g02)

anova(identity.g00, identity.g01, identity.g02)
BIC(identity.g00, identity.g01, identity.g02)
sum(df.temp$idle_any == 1)
1179/nrow(df.temp)

# For now, we leave this variable as it is, since replacing it with a binary encoding 
# does not bring benefits.


rm(identity.g00, identity.g01, identity.g02, logit.g00, logit.g01, logit.g02, df.temp)

###########################################################################
# wip ---------------------------------------------------------------------
###########################################################################

df.temp <- df
df.temp$Y <- Y
df.temp$targeted_productivity <- targeted_productivity
df.temp$no_of_workers <- no_of_workers
df.temp$no_of_style_change <- no_of_style_change 

# Let's see the overall number of NA's
df.temp %>%
  dplyr::summarise(
    n = nrow(.),
    wip.na = sum(is.na(wip)),
    wip.na_mean = mean(is.na(wip))
  ) # 506, .42 of data 

# I earlier observed that most or all NA's are in the finishing department, let's actually see what
# the split is.
df.temp %>%
  dplyr::group_by(department) %>%
  summarise(
    n = n(),
    wip.na = sum(is.na(wip)),
    wip.na_mean = mean(is.na(wip))) %>%
  as.data.frame(arrange(desc(wip.na_mean)
  ))

# This is perfect, since it's exactly what we wanted to see. All variables in the 
# finishing_department have NA values.
# We will use 0 to replace NA's considering there were actually 0 wip's in finishing department. 

wip <- if_else(df.temp$department == 0, df.temp$wip, 0) # 0 = Sewing, 1 = Finishing
wip[1:200]
sum(wip == 0)
df.temp$wip <- wip
# Considering that the effect of wip is only measured for sewing department we need to create a new variable wip_sewing = wip * (1 - department)
# Mathematically, this doesn't change anything but when it comes to interpretation our model will know that for finishing (department = 1), wip_sewing = 0 and else for sewing (department = 0) 1.
wip_sewing <- df.temp$wip * (1 - df.temp$department) # I always store separate objects so i can go back to their original form

rm(df.temp, wip)

###########################################################################
# ADDING CHANGES ----------------------------------------------------------
###########################################################################

# We answered the "hard" questions. Now let's put everything together.
df_old <- df # we save the current version of our df before making transformations

df$team <- factor(df$team) # For now, converting to factors is enough.
df$targeted_productivity <- targeted_productivity # fixed 0.07, it must be an error.
df$no_of_style_change <- no_of_style_change # binary classification, to measure shock effect
df$no_of_workers <- no_of_workers # rounded up to represent the true number of workers
# wip
df <- df %>%
  dplyr::mutate(
    wip_sewing = wip_sewing
  ) %>%
  dplyr::select(
    -wip
  ) # variable manipulated to measure the effect of wip on sewing department only.
df$department <- department # binary classification, simplicity
df$Y <- Y # If (actual_productivity >= target_productivity) 1 else 0

str(df)
# write.csv(x = df, file = "CF1_data.csv", row.names = F)

# Next --> VARIABLES