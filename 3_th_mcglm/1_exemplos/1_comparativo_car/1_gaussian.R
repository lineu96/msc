#-------------------------------------------------------------------

# Comparativo funções com car

library(mcglm)
library(car)
source('~/msc/3_th_mcglm/0_funcoes/functions.R')

#-------------------------------------------------------------------

# Dados 
?labestData::PaulaEg1.12.5
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
mc_anova_II(m2)[[1]]
mc_manova_II(m2)

## Tipo III
Anova(m1, type = 'III', test.statistic = 'Wald')
mc_anova_III(m2)[[1]]
mc_manova_III(m2)

#-------------------------------------------------------------------

# Hipóteses lineares

## Testando se todos os parametros sao 0
linearHypothesis(m1, c("(Intercept) = 0", 
                       "taxa = 0",
                       "licen = 0",
                       "renda = 0",
                       "estr = 0"))

parametros <- coef(m2, type = 'beta')
parametros$beta_names <- Reduce(c,m2$beta_names)
parametros

mc_linear_hypothesis(object =  m2, 
                     hypothesis =  c('beta10 = 0', 
                                     'beta11 = 0',
                                     'beta12 = 0',
                                     'beta13 = 0',
                                     'beta14 = 0'))

## Testando se o intercepto é igual a 380
linearHypothesis(m1, c("(Intercept) = 380"))

mc_linear_hypothesis(object =  m2, 
                     hypothesis =  c('beta10 = 380'))

## Testando se beta taxa = beta licen
linearHypothesis(m1, c("taxa = licen"))

mc_linear_hypothesis(object =  m2, 
                     hypothesis =  c('beta11 = beta12'))

## Testando se beta taxa = beta licen e beta renda = beta estr
linearHypothesis(m1, c("taxa = licen",
                       "renda = estr"))

mc_linear_hypothesis(object =  m2, 
                     hypothesis =  c('beta11 = beta12',
                                     'beta13 = beta14' ))

#-------------------------------------------------------------------

# MODELO COM INTERAÇÕES
form2  <- cons ~ (taxa + licen + renda + estr)^2

#-------------------------------------------------------------------

# GLM
m1 <- glm(formula = form2, data = dados)

#-------------------------------------------------------------------

# McGLM
Z0 <- mc_id(dados) # Identidade

m2 <- mcglm(linear_pred = c(form2),
            matrix_pred = list(c(Z0)),
            data = dados)

#-------------------------------------------------------------------

# Estimativas
data.frame(glm = round(m1$coefficients, 2),
           mcglm = round(m2$Regression, 2))

#-------------------------------------------------------------------

# Anova

## Tipo III
Anova(m1, type = 'III', test.statistic = 'Wald')
mc_anova_III(m2)[[1]]
mc_manova_III(m2)

## Tipo II
### An R Companion to Applied Regression, página 367

Anova(m1, type = 'II', test.statistic = 'Wald')
mc_anova_II(m2)[[1]]
mc_manova_II(m2)

linearHypothesis(m1, c("taxa = 0",
                       "taxa:licen = 0",
                       "taxa:renda = 0",
                       "taxa:estr = 0"))

linearHypothesis(m1, c("licen = 0",
                       "taxa:licen = 0",
                       "licen:renda = 0",
                       "licen:estr = 0"))

linearHypothesis(m1, c("renda = 0",
                       "taxa:renda = 0",
                       "licen:renda = 0",
                       "renda:estr = 0"))

linearHypothesis(m1, c("estr = 0",
                       "taxa:estr = 0",
                       "licen:estr = 0",
                       "renda:estr = 0"))

#-------------------------------------------------------------------

# Hipóteses lineares

## Testando se todos os parametros que tem estr são iguais a 0
linearHypothesis(m1, c("estr = 0",
                       "taxa:estr = 0",
                       "licen:estr = 0",
                       "renda:estr = 0"))

parametros <- coef(m2, type = 'beta')
parametros$beta_names <- Reduce(c,m2$beta_names)
parametros

mc_linear_hypothesis(object =  m2, 
                     hypothesis =  c('beta14 = 0', 
                                     'beta17 = 0',
                                     'beta19 = 0',
                                     'beta110 = 0'))

## Testando se o intercepto é igual a 148
linearHypothesis(m1, c("(Intercept) = 148"))

mc_linear_hypothesis(object =  m2, 
                     hypothesis =  c('beta10 = 148'))

## Testando se beta taxa = beta licen
linearHypothesis(m1, c("taxa:licen = taxa:renda"))

mc_linear_hypothesis(object =  m2, 
                     hypothesis =  c('beta15 = beta16'))

## Testando se beta taxa = beta licen e beta renda = beta estr
linearHypothesis(m1, c("taxa = licen",
                       "renda = estr"))

mc_linear_hypothesis(object =  m2, 
                     hypothesis =  c('beta11 = beta12',
                                     'beta13 = beta14' ))

#-------------------------------------------------------------------

linearHypothesis(m1, c("taxa = licen",
                       "taxa = renda",
                       "taxa = estr"))

mc_linear_hypothesis(object =  m2, 
                     hypothesis =  c('beta11 = beta12',
                                     'beta11 = beta13',
                                     'beta11 = beta14' ))

#-------------------------------------------------------------------