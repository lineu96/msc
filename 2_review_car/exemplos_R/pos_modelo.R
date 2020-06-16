#---------------------------
library(car)
#---------------------------

## Modified Functions for Summarizing Linear, Generalized Linear, and Some Other Models

mod.prestige <- lm(prestige ~ education + income + type, Prestige)
S(mod.prestige, vcov.=hccm)
S(mod.prestige, brief=TRUE)
Confint(mod.prestige, vcov.=hccm)

# A logit model
mod.mroz <- glm(lfp ~ ., data=Mroz, family=binomial)
S(mod.mroz)
Confint(mod.mroz)

#---------------------------

## Bootstrapping for regression models

# simple front-end to the boot function in the boot package that is tailored to bootstrapping based on regression models. Whereas boot is very general and therefore has many arguments, the Boot function has very few arguments.

m1 <- lm(Fertility ~ ., swiss)
betahat.boot <- Boot(m1, R=199) # 199 bootstrap samples--too small to be useful
summary(betahat.boot)  # default summary
confint(betahat.boot)
hist(betahat.boot)

# Bootstrap for the estimated residual standard deviation:
sigmahat.boot <- Boot(m1, R=199, f=sigmaHat, labels="sigmaHat")
summary(sigmahat.boot)
confint(sigmahat.boot)

#---------------------------

## Print estimated coefficients and their standard errors in a table for several regression models.

#  extracts estimates of regression parameters and their standard errors from one or more models and prints them in a table.

mod1 <- lm(prestige ~ income + education, data=Duncan)
mod2 <- lm(prestige ~ income + education + type, data=Duncan)

compareCoefs(mod1)
compareCoefs(mod1, mod2)
compareCoefs(mod1, mod2, zvals=TRUE, pvals=TRUE)
compareCoefs(mod1, mod2, se=FALSE)
compareCoefs(mod1, mod1, vcov.=list(vcov, hccm))

#---------------------------

## Confidence Ellipses

?confidenceEllipse

par(mfrow = c(1,2))

confidenceEllipse(lm(prestige~income+education, data=Duncan), Scheffe=TRUE)

confidenceEllipse(lm(prestige~income+education, data=Duncan), vcov.=hccm)

#---------------------------

## Influence Index Plot

#index plots of influence and related diagnostics for a regression model

influenceIndexPlot(lm(prestige ~ income + education + type, Duncan))

#---------------------------

# dfbeta and dfbetas Index Plots

#display index plots of dfbeta (effect on coefficients of deleting each observation in turn) and dfbetas (effect on coefficients of deleting each observation in turn, standardized by a deleted estimate of the coefficient standard error). 
#In the plot of dfbeta, horizontal lines are drawn at 0 and +/- one standard error; in the plot of dfbetas, horizontal lines are drawn and 0 and +/- 1.

#where bj is the coefficient computed using all of the data, and b(−i)j
# is the same coefficient computed with case i omitted

#A standardized version, dfbetasij, divides dfbetaij by an 
#estimate of the standard error of bj computed with case i removed.

dfbetaPlots(lm(prestige ~ income + education + type, data=Duncan))

dfbetasPlots(glm(partic != "not.work" ~ hincome + children, 
                 data=Womenlf, family=binomial))

#---------------------------

## Scale estimate for a regression model

m1 <- lm(prestige ~ income + education, data=Duncan)
sigmaHat(m1)

#---------------------------

# Variance Inflation Factors

# Calculates variance-inflation and generalized variance-inflation factors for linear, generalized linear, and other models.

# are interpretable as the inflation in size of the confidence ellipse or ellipsoid for the coefficients of the term in comparison with what would be obtained for orthogonal data.

vif(lm(prestige ~ income + education, data=Duncan))
vif(lm(prestige ~ income + education + type, data=Duncan))

#---------------------------

# Plot Output from regsubsets Function in leaps package

subsets	#ANÁLISE DO MODELO	?

#The regsubsets function in the leaps package finds optimal subsets of predictors based on some criterion statistic. This function plots a measure of fit against subset size.

if (require(leaps)){
  subsets(regsubsets(undercount ~ ., data=Ericksen),
          legend=c(3.5, -37))
}

#---------------------------

# Inverse Response Plots to Transform the Response

# response Y on the vertical axis and the fitted values Yhat on the horizontal axis. Uses nls to estimate lambda in the function Yhat = b0 + b1(Y)^(lambda). Adds the fitted curve to the plot.
# only lm

m2 <- lm(rate ~ log(len) + log(adt) + slim + shld + log(sigs1), Highway1)
invResPlot(m2)


#---------------------------

# Marginal Model Plotting

# For a regression object, draw a plot of the response on the vertical axis versus a linear combination u of regressors in the mean function on the horizontal axis. 
#Added to the plot are a smooth for the graph, along with a smooth from the plot of the fitted values on u. mmps is an alias for marginalModelPlots, and mmp is an alias for marginalModelPlot.

c1 <- lm(infantMortality ~ ppgdp, UN)
mmps(c1)

c2 <- update(c1, ~ log(ppgdp))
mmps(c2)

# include SD lines
p1 <- lm(prestige ~ income + education, Prestige)
mmps(p1, sd=TRUE)

# condition on type:
mmps(p1, ~. | type)

# logisitic regression example
# smoothers return warning messages.
# fit a separate smoother and color for each type of occupation.
m1 <- glm(lfp ~ ., family=binomial, data=Mroz)
mmps(m1)

#---------------------------

# Draw Linear Model Marginal and Conditional Plots in Parallel or Overlaid

# draws two graphs or overlays the two graphs. 
# For a response Y and a regressor X, the first plot is the marginal plot of Y versus X with both variables centered, visualizing the conditional distribution of Y given X ignoring all other regressors. 
# The second plot is an added-variable for X after all other regressors, visualizing the conditional distribution of Y given X after adjusting for all other predictors.

# The second conditional plot is the added-variable plot of e(Y|Z) versus e(X|Z) where e(a|b) means the Pearson residuals from the regression of a on b

m1 <- lm(partic ~ tfr + menwage + womwage + debt + parttime, data = Bfox)
mcPlot(m1, "womwage")
mcPlot(m1, "womwage", overlaid=FALSE, ellipse=TRUE)

#---------------------------

## Quantile-Comparison Plot

m <- lm(prestige ~ income + education + type, data=Duncan)
qqPlot(m, envelope=.99)

#---------------------------

# Residual Plots for Linear and Generalized Linear Models

m1 <- lm(prestige ~ income, data=Prestige)
residualPlots(m1)
residualPlots(m1, terms= ~ 1 | type) # plot vs. yhat grouping by type

#---------------------------

# Regression Influence Plot

#“bubble” plot of Studentized residuals versus hat values, with the areas of the circles representing the observations proportional to the value Cook's distance. Vertical reference lines are drawn at twice and three times the average hat value, horizontal reference lines at -2, 0, and 2 on the Studentized-residual scale.

influencePlot(lm(prestige ~ income + education, data=Duncan))

#---------------------------

# Spread-Level Plots

#Creates plots for examining the possible dependence of spread on level, or an extension of these plots to the studentized residuals from linear models.

#A spread-level plot (Tukey, 1977) is a scatterplot of the logarithm of the
#interquartile range, which measures spread, versus the logarithm of the withingroup median, which measures level. Interquartile ranges and medians are
#insensitive to a few outliers, and so the spread-level plot provides a robust
#representation of the dependence of spread on level.

par(mfrow = c(1,2))

m1 <- glm(interlocks + 1 ~ assets + sector + nation, data=Ornstein,
         family = 'Gamma')
slp(m1)
plot(log(abs(rstudent(m1)))~log(m1$fitted.values))

m2 <- lm(interlocks + 1 ~ assets + sector + nation, data=Ornstein)

slp(m2)
plot(log(abs(rstudent(m2)))~log(m2$fitted.values))

#---------------------------

# Component+Residual (Partial Residual) Plots

# Construct component+residual plots, also called partial-residual plots, for linear and generalized linear models.

# The model cannot contain interactions, but can contain factors. Parallel boxplots of the partial residuals are drawn for the levels of a factor.

crPlots(m<-lm(prestige ~ income + education, data=Prestige)) 

crPlots(m, terms=~ . - education) # get only one plot

crPlots(lm(prestige ~ log2(income) + education + poly(women,2), data=Prestige))

crPlots(glm(partic != "not.work" ~ hincome + children, 
            data=Womenlf, family=binomial), smooth=list(span=0.75))

#---------------------------

#Ceres Plots

#Ceres plots are a generalization of component+residual (partial residual) plots that are less prone to leakage of nonlinearity among the predictors.

#The model cannot contain interactions, but can contain factors. Factors may be present in the model, but Ceres plots cannot be drawn for them.

ceresPlots(lm(prestige~income+education+type, data=Prestige), 
           terms= ~ . - type)

#---------------------------
# Added-Variable Plots

# Construct added-variable, also called partial-regression, plots for linear and generalized linear models.

avPlots(lm(prestige ~ income + education + type, data=Duncan))

avPlots(glm(partic != "not.work" ~ hincome + children, 
            data=Womenlf, family=binomial), id=FALSE)

m1 <- lm(partic ~ tfr + menwage + womwage + debt + parttime, Bfox)
par(mfrow=c(1,3))

# marginal plot, ignoring other predictors:
with(Bfox, dataEllipse(womwage, partic, levels=0.5)) 
abline(lm(partic ~ womwage, Bfox), col="red", lwd=2)
# AV plot, adjusting for others:
avPlots(m1, ~ womwage, ellipse=list(levels=0.5)) 
# AV plot, adjusting and scaling as in marginal plot
avPlots(m1, ~ womwage, marginal.scale=TRUE, ellipse=list(levels=0.5))

#---------------------------

# Regression Leverage Plots

#generalization, of added-variable plots to multiple-df terms in a linear model. 
#When a term has just 1 df, the leverage plot is a rescaled version of the usual added-variable (partial-regression) plot.

leveragePlots(lm(prestige~(income+education)*type, data=Duncan))

#---------------------------  