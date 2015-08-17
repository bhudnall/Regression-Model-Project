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
#     3. Did the student fit multiple models and detail their strategy for model selection?
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

## create an instance of mtcars
cars_inst <- mtcars

## explore variables of data set
summary(cars_inst)
glimpse(cars_inst)

# help(mtcars) - description
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
pairs_plot <- ggpairs(data = cars_inst
                      , columns = 1:ncol(cars_inst)
                      , title = "Explore Mtcars Data"
                      , diag = list(continuous = "bar", params = c(color="green"))
                      , upper = list(params=list(carSize = 55), axisLabels = 'show')
                      , lower = list(continuous = "smooth", params = c(color="blue"))
)
pairs_plot

## Run a linear regression model with mpg as the outcome and
## the rest of the variables as predictors

lm_model <- lm(mpg ~ cyl
               + disp
               + hp
               + drat
               + wt
               + qsec
               + vs
               + am
               + gear
               + carb
               , data = cars_inst)
summary(lm_model)
