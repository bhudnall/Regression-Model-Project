# You work for Motor Trend, a magazine about the automobile industry. 
# Looking at a data set of a collection of cars, they are interested 
# in exploring the relationship between a set of variables and miles 
# per gallon (MPG) (outcome). They are particularly interested in the 
# following two questions:
#     
#     1. Is an automatic or manual transmission better for MPG
#     2. Quantify the MPG difference between automatic and manual transmissions

# Must Include:
#     1. Did the student interpret the coefficients correctly? 
#     2. Did the student do some exploratory data analyses? 
#     3. Did the student fit multiple models and detail their strategy for model selection? - 
#     4. Did the student answer the questions of interest or detail why the question(s) is 
#        (are) not answerable? 
#     5. Did the student do a residual plot and some diagnostics? 
#     6. Did the student quantify the uncertainty in their conclusions and/or perform an inference correctly?
#     7. Was the report brief (about 2 pages long) for the main body of the report and no 
#        longer than 5 with supporting appendix of figures?
#     8. Did the report include an executive summary? 

require(UsingR)
require(dplyr)
require(ggplot2)
require(GGally)
require(leaps)

## explore variables of data set
summary(mtcars)
glimpse(mtcars)

## Pull up details of the mtcars dataset
help(mtcars)

# A data frame with 32 observations on 11 variables.
# 
# [, 1]	mpg	Miles/(US) gallon
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (lb/1000)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	V/S
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors


## First do a pairs plot to see how all variables correlate
## with the mpg variable
pairs_plot <- ggpairs(data = mtcars
                      , columns = 1:ncol(mtcars)
                      , title = "Explore Mtcars Data"
                      , diag = list(continuous = "bar", params = c(color="green"))
                      , upper = list(params=list(carSize = 55), axisLabels = 'show')
                      , lower = list(continuous = "smooth", params = c(color="blue"))
)
pairs_plot

## Next do a boxplot to explore the relationship of the
## different levels of the transmission type with mpg

mpg_boxplot <- ggplot(mtcars
                      , aes(factor(am)
                      , mpg)) + 
    geom_boxplot(aes(fill = factor(am))) + 
    geom_jitter() +
    ggtitle("MPG with manual(1) and auto(0) transmissions")
mpg_boxplot

## Based on this plot automatic transmission has the lower
## mpgs. let's calculate the means.

mtcars_summary <- mtcars %>%
    group_by(am) %>%
    summarise(avg_mpg = mean(mpg))
mtcars_summary

## Model selection - by including unecessary variables, there is a possibility
## increasing the standard erros of other regressors by including variables that have
## high correlations with the outcome variable. So it's important to do
## a thorough check of the correct model, or ensure that the appropriate control
## variables are included. Higher variance is a sign of unecessary variables being included.
## It's also important to not exclude important variables
## to ensure bias is not introduced to the model. 

## Run a linear model on just am and mpg
lm_model <- lm(mpg ~ factor(am), data = mtcars); 
summary(lm_model)

## Next we use regsubset to perform regression subset selection
## and view the ranked results of models accorsing to the adjusted
## R squared. With tis plot we can see the adjusted R-squared value
## on the y-axis and the variables included by their shade -- 
## black is included and white is not. As we know, the higher
## the R^2 the better, as we have accounted for more of the 
## variability.
model_select <- regsubsets(mpg ~ ., data = mtcars)
model_select_plot <- plot(model_select, scale = "adjr2")
model_select_plot

## Run a multi variable linear regression model with mpg as the outcome and
## the rest of the variables as predictors. Use step-wise function both backward and 
## forward to uncover which variables to include.

lm_multi_model <- lm(mpg ~ . , data = mtcars)
lmm <- step(lm_multi_model, direction="both") 
summary(lmm)

## Now we know which independent variables to include so
## we run our final model

best_model <- lm(mpg ~ factor(am) + qsec + wt, data = mtcars)
bm <- step(best_model, direction="both") 
summary(bm)

## Now we plot to take a look at the residuals
## For the first plot we don't see anything systematic
## or model departures to uncover hederskedasticity
## Also, in looking at the residual vs. leverage plot
## there doesn't seem to be any systematic trends.
par(mfrow = c(2, 2))
plot(bm)





