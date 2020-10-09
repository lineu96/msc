#-------------------------------------------------------------------

# Comparativo funções com car

library(mcglm)
library(car)
source('~/msc/3_th_mcglm/0_funcoes/functions.R')

#-------------------------------------------------------------------

# Dados 
dados <- labestData::PaulaEg1.12.5[ , -1]

#-------------------------------------------------------------------

# MODELO SEM INTERAÇÕES
form  <- cons ~ (taxa + licen + renda + estr)

#-------------------------------------------------------------------

# GLM
m1 <- glm(formula = form, data = dados)

#-------------------------------------------------------------------

# McGLM
Z0 <- mc_id(dados) # Identidade

m2 <- mcglm(linear_pred = c(form),
            matrix_pred = list(c(Z0)),
            data = dados)

#-------------------------------------------------------------------

# Estimativas
data.frame(glm = round(m1$coefficients, 2),
           mcglm = round(m2$Regression, 2))

#-------------------------------------------------------------------

# Anova

## Tipo II
Anova(m1, type = 'II', test.statistic = 'Wald')
mc_anova_II(m2)
mc_manova_II(m2)

## Tipo III
Anova(m1, type = 'III', test.statistic = 'Wald')
mc_anova_III(m2)[[1]]
mc_manova_III(m2)

#-------------------------------------------------------------------

# Hipóteses lineares

linearHypothesis(m1, c("(Intercept) = 0", 
                       "taxa = 0",
                       "licen = 0",
                       "renda = 0",
                       "estr = 0"))

mc_linear_hypothesis(object =  m2, 
                     parameters = c('beta10', 
                                    'beta11',
                                    'beta12',
                                    'beta13',
                                    'beta14'),
                     null_hyp = c(0,0,0,0,0))

linearHypothesis(m1, c("(Intercept) = 380"))

mc_linear_hypothesis(object =  m2, 
                     parameters = c('beta10'),
                     null_hyp = c(380))

#-------------------------------------------------------------------

# MODELO COM INTERAÇÕES
form  <- cons ~ (taxa + licen + renda + estr)^2

#-------------------------------------------------------------------

# GLM
m1 <- glm(formula = form, data = dados)

#-------------------------------------------------------------------

# McGLM
Z0 <- mc_id(dados) # Identidade

m2 <- mcglm(linear_pred = c(form),
            matrix_pred = list(c(Z0)),
            data = dados)

#-------------------------------------------------------------------

# Estimativas
data.frame(glm = round(m1$coefficients, 2),
           mcglm = round(m2$Regression, 2))

#-------------------------------------------------------------------

# Anova

## Tipo II
Anova(m1, type = 'II', test.statistic = 'Wald')
mc_anova_II(m2)
mc_manova_II(m2)

## Tipo III
Anova(m1, type = 'III', test.statistic = 'Wald')
mc_anova_III(m2)[[1]]
mc_manova_III(m2)

#-------------------------------------------------------------------

# Hipóteses lineares

linearHypothesis(m1, c("(Intercept) = 0", 
                       "taxa = 0",
                       "licen = 0",
                       "renda = 0",
                       "estr = 0"))

mc_linear_hypothesis(object =  m2, 
                     parameters = c('beta10', 
                                    'beta11',
                                    'beta12',
                                    'beta13',
                                    'beta14'),
                     null_hyp = c(0,0,0,0,0))

linearHypothesis(m1, c("(Intercept) = 313"))

mc_linear_hypothesis(object =  m2, 
                     parameters = c('beta10'),
                     null_hyp = c(313))

#-------------------------------------------------------------------
