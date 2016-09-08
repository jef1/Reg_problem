#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("~/Desktop/data/SpringReg/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
 hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

# 1)
en.met <- subset(states.data, select = c("energy", "metro"))
cor(en.met) # appears to show a very strong positive correlation
plot(en.met) # gives a much more unclear idea of a relation between the two variables
hist(states.data$energy) # long tailed distribution, as the response variable would probably benefit from log transformation
hist(states.data$metro) # leaning towards there being more states with a high proportion living in metropolitan areas

par(mfrow = c(1, 3))
plot(states.data$energy, states.data$metro)
plot(log(states.data$energy), log(states.data$metro))
plot(log(states.data$energy), states.data$metro)

# whilst the log transformation of the dependent appears to have improved the relationship with
# metro, transforming metro only seems to muddy the picture.

model1 <- lm(energy ~ metro, states.data)
summary(model1) # As expected the summary has produced a very poor model with an R-squared of
# 0.1154. This means that only a small proportion of the varience is being explained by the model.

model2 <-lm(log(energy) ~ metro, states.data)
summary(model2) # as can be seen R-squared has been improved a bit, but it is still very low,
# meaning that it is likely that the model can be greatly improved. I will add the log of 
# energy to states.data.

states.data$log.en <- log(states.data$energy)

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

# Upon the hypothesis that states with more pollution will use more energy, as they are more
# likely to be engaging in greater levels of heavy industry I will add indicators of industry.
# Also upon the hypothesis that richer areas will demand more controls over pollution, I will
# experiment will look into the effect of income levels, and also include indicators of environmental
# voting preferences.

hist(states.data$toxic)
par(mfrow = c(1, 2))
plot(states.data$log.en, log(states.data$toxic))
plot(states.data$log.en, states.data$toxic)
# so will use the log of toxic
states.data$log.tox <- log(states.data$toxic)
hist(states.data$log.tox)

plot(log(states.data$green), states.data$log.en)
hist(log(states.data$green))
# Log green might be a better predictor than toxic
states.data$log.gn <- log(states.data$green)
cor(states.data$green, states.data$log.en, use = "complete.obs")
cor(states.data$log.gn, states.data$log.en, use = "complete.obs")

# looking at income and voting patterns
plot(states.data$log.en, states.data$income)
hist(states.data$income) # income is normally distributed but does not have the strongest correlation
# with log.en
cor(states.data$income, states.data$log.en, use = "complete.obs")

plot(states.data$log.en, states.data$house) # relationship looks linear with a few outliers
cor(states.data$log.en, states.data$house, use = "complete.obs") # reasonably strong negative correlation
hist(states.data$house) # dist looks normal on visual inspection

plot(states.data$log.en, states.data$senate)
hist(states.data$senate)
plot(states.data$senate, states.data$house) # As expected there is a correlation between
#senate and house voting, but in an attempt to prevent correlations between variables I will
# use house, as it appeared to have the strongest relationship.

model3 <- lm(log.en ~ metro + log.gn + house + log.tox, states.data)
summary(model3)



# This model has greatly improved predictability, with adjusted R-squared at 0.713. Nonetheless
# house and metro are not statistically significant explanatory variables. The model appears to
# be saying that, as hypothesised, there is a strong relationship between the ammount of polutants
# in a state and the ammount of energy used.
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) 
plot(model3, which = c(1, 2))

# Having looked at the residuals it seems that there is still work to be done on this model -
# the spread of residuals is not evenly distributed.

# With having two predictors that measure simular things (log.tox and log.gn) I will test for 
# multicolinearity
car::vif(model3) # as the veriables are all beneith 4 I will assume there is negligable multicolinearity.

# To test assumptions of homoscedasticity
lmtest::bptest(model3) # technically accepting a 0.05 threshold the model passes - but is so 
# close that inspecting along with visual inspections means more work is needed.

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

# 1)
# After some exploratory analysis I have added an interaction term between income and metro.
# This is because the level of income appears to have an effect on both the percentage of people
# that are living in a metropolitan area, with more living in wealthier states, as well as the
# ammount of energy and lower income states using more energy - prehaps because of the more
# polutive industries that tend to be popular.

qplot(log.en, metro, data = states.data, col = income)
model4 <- lm(log.en ~ metro*income + log.gn + log.tox + house, states.data)
summary(model4)
model3 <- lm(log.en ~ metro + log.gn + house + log.tox, states.data)
summary(model3)

# With the interaction is not significant, and the interactions have stayed positive.

# 2) # In order to see the difference that regions play on the model will add region.
# First need to make sure that R knows it is dealing with a factor
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
en.region <- lm(log.en ~ metro*income + log.gn + log.tox + house + region, states.data)
summary(en.region)

#Show the results
coef(summary(en.region)) # show regression coefficients table
anova(en.region) # show A

# As can be seen from the ANOVA, metro, income, log.gn, log.tox are statistically significant
# populations, however house, region and the interaction between metro and income are not 
# statistically significant.