#-------------------------------------------------------------------

# Comparativo funções com car 2

library(mcglm)
library(car)
source('~/msc/3_th_mcglm/0_funcoes/functions.R')

#-------------------------------------------------------------------

# Dados 
dados <- read.csv2("~/msc/3_th_mcglm/1_exemplos/0_dados/dados_ovelhas.csv", 
                   header = T, 
                   sep = ";", 
                   dec = ',')

dados2 <- dados[,c(1,2,3,4,
                   14,15,17,21,
                   7,9,10,11)]

dados2$momento <- ordered(dados2$tempo, 
                          levels=c("Antes", "Durante", "Depois"))


#-------------------------------------------------------------------

# MODELO SEM INTERAÇÕES
form  <- ncorpo  ~ (sessao + tempo + linhagem)

#-------------------------------------------------------------------

# GLM
m1 <- glm(formula = form, 
          family = 'poisson', 
          data = dados2)

#-------------------------------------------------------------------

# McGLM
Z0 <- mc_id(dados2) # Identidade

m2 <- mcglm(linear_pred = c(form), 
            matrix_pred = list(c(Z0)),
            link = "log", variance = "tweedie",
            power_fixed = TRUE, data = dados2)

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
mc_anova_III(m2)
mc_manova_III(m2)

#-------------------------------------------------------------------

# Hipóteses lineares

linearHypothesis(m1, c("(Intercept) = 0", 
                       "sessaoSe2 = 0",
                       "sessaoSe3 = 0",
                       "tempoDurante = 0",
                       "tempoDepois = 0",
                       "linhagems+ = 0"))

mc_linear_hypothesis(object =  m2, 
                     parameters = c('beta10', 
                                    'beta11',
                                    'beta12',
                                    'beta13',
                                    'beta14',
                                    'beta15'),
                     null_hyp = c(0,0,0,0,0,0))

linearHypothesis(m1, c("(Intercept) = 0.7"))

mc_linear_hypothesis(object =  m2, 
                     parameters = c('beta10'),
                     null_hyp = c(0.7))

#-------------------------------------------------------------------

# MODELO COM INTERAÇÕES
form  <- ncorpo  ~ (sessao + tempo + linhagem)^2

#-------------------------------------------------------------------

# GLM
m1 <- glm(formula = form, 
          family = 'poisson', 
          data = dados2)

#-------------------------------------------------------------------

# McGLM
Z0 <- mc_id(dados2) # Identidade

m2 <- mcglm(linear_pred = c(form), 
            matrix_pred = list(c(Z0)),
            link = "log", variance = "tweedie",
            power_fixed = TRUE, data = dados2)

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
                       "sessaoSe2 = 0",
                       "sessaoSe3 = 0",
                       "tempoDurante = 0",
                       "tempoDepois = 0",
                       "linhagems+ = 0"))

mc_linear_hypothesis(object =  m2, 
                     parameters = c('beta10', 
                                    'beta11',
                                    'beta12',
                                    'beta13',
                                    'beta14',
                                    'beta15'),
                     null_hyp = c(0,0,0,0,0,0))

linearHypothesis(m1, c("(Intercept) = 0.7"))

mc_linear_hypothesis(object =  m2, 
                     parameters = c('beta10'),
                     null_hyp = c(0.7))


#-------------------------------------------------------------------
