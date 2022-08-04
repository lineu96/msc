
#-----------------------------------------------------------------------
# REPLICATION MATERIAL
#-----------------------------------------------------------------------
# Hypothesis tests for multiple responses regression models in R: The 
# htmcglm Package
#
# Authors: Lineu Alberto Cavazani de Freitas - UFPR
#          Wagner Hugo Bonat - UFPR
#
# Date: 03/08/2022
#-----------------------------------------------------------------------


# Installing the mcglm package
# install.packages('mcglm')

# Installing the htmcglm package
# install.packages('htmcglm')

#-----------------------------------------------------------------------

# BEGIN HEADER
rm(list=ls())

#-----------------------------------------------------------------------

# Loading packages 
library(mcglm)
library(Matrix)
library(htmcglm)

#-----------------------------------------------------------------------

# Example 1: soya
data("soya", package = "mcglm")

## Linear predictors
form.grain <- grain ~ block + water * pot
form.seed <- seeds ~ block + water * pot

soya$viablepeasP <- soya$viablepeas / soya$totalpeas
form.peas <- viablepeasP ~ block + water * pot

## Matrix linear predictor
Z0 <- mc_id(soya)

## Model
fit_joint <- mcglm(linear_pred = c(form.grain, 
                                   form.seed, 
                                   form.peas),
                   matrix_pred = list(c(Z0), 
                                      c(Z0), 
                                      c(Z0)),
                   link = c("identity",
                            "log", 
                            "logit"),
                   variance = c("constant", 
                                "tweedie", 
                                "binomialP"),
                   Ntrial = list(NULL, 
                                 NULL, 
                                 soya$totalpeas),
                   power_fixed = c(T,T,T),
                   data = soya)

## ANOVA type I
mc_anova_I(fit_joint)

## ANOVA type II
mc_anova_II(fit_joint)

## ANOVA type III
mc_anova_III(fit_joint)

## MANOVA type I
mc_manova_I(fit_joint)

## MANOVA type II
mc_manova_II(fit_joint)

## MANOVA type III
mc_manova_III(fit_joint)

## Test on a single regression parameter
mc_linear_hypothesis(object =  fit_joint, 
                     hypothesis = c('beta11 = 0'))

## Test on more than one regression parameter
mc_linear_hypothesis(object =  fit_joint, 
                     hypothesis = c('beta11 = 0', 
                                    'beta12 = 0'))

## Test of equality of effects between regression parameters}
mc_linear_hypothesis(object =  fit_joint, 
                     hypothesis = c('beta11 = beta21'))

#-----------------------------------------------------------------------

# Example 2: Hunting
data("Hunting", package = "mcglm")

## Linear predictors
form.OT <- OT ~ METHOD * SEX
form.BD <- BD ~ METHOD * SEX

## Matrix linear predictor
Z0 <- mc_id(Hunting)
Z1 <- mc_mixed(~ 0 + HUNTER.MONTH, data = Hunting)

## Model
fit <- mcglm(linear_pred = c(form.BD, form.OT),
             matrix_pred = list(c(Z0, Z1), 
                                c(Z0, Z1)),
             link = c("log", "log"),
             variance = c("poisson_tweedie",
                          "poisson_tweedie"),
             offset = list(log(Hunting$OFFSET), 
                           log(Hunting$OFFSET)),
             data = Hunting)

## Test on a single dispersion parameter
mc_linear_hypothesis(object =  fit, 
                     hypothesis = c('tau11 = 0'))

## Test on more than one dispersion parameter
mc_linear_hypothesis(object =  fit, 
                     hypothesis = c('tau11 = 0',
                                    'tau21 = 0'))

## Test of equality of effects between dispersion parameters
mc_linear_hypothesis(object =  fit,
                     hypothesis = c('tau12 = tau22'))

## ANOVA type III for dispersion
mc_anova_dispersion(fit,
                    p_var = list(c(0,1), c(0,1)),
                    names = list(c('tau10', 'tau11'),
                                 c('tau20', 'tau21')))

## MANOVA type III for dispersion
mc_manova_dispersion(fit,
                     p_var = c(0,1),
                     names = c('tau0', 'tau1'))

## Univariate multiple comparisons test
mc_multcomp(object = fit,
            effect = list(c('METHOD', 'SEX'), 
                          c('METHOD', 'SEX')), 
            data = Hunting)

## Multivariate multiple comparisons test
mc_mult_multcomp(object = fit, 
                 effect = c('METHOD', 'SEX'), 
                 data = Hunting)

#-----------------------------------------------------------------------