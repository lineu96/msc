#---------------------------------------------------------------
# SOYA MCGLM
#---------------------------------------------------------------

library(mcglm)
library(Matrix)

library(ggplot2)
library(ggpubr)

source('~/msc/0_funcoes/functions.R')

#---------------------------------------------------------------

# Contexto

# Contagens bivariadas longitudinais, dados apresentados em Bonat et al.
# (2017)

# animais caçados na vila de Basile Fang, Bioko Norte Province, Bioko 
# Island, Equatorial Guinea.

# Números mensais de blue duikers (BD) e outros pequenos animais (OT) 
# baleados ou capturados foram coletados para uma amostra aleatória de 
# 52 caçadores comerciais de agosto de 2010 a setembro de 2013.

# Covariáveis:

## ALT: fator de 5 níveis que indica a altitude em que o animal foi 
##      caçado

## SEX: fator com 2 níveis (Female and Male)

## METHOD: fator com 2 níveis (Firearm and Snare) 

## MONTH. 

# OFFSET: número de dias de caça por mês


# Aqui, apresentamos uma análise simplificada considerando apenas a 
# estrutura de medidas repetidas introduzidas pelas observações 
# realizadas para o mesmo caçador e mês (HUNTER.MONTH). 

# Para uma descrição mais detalhada dos dados e análise completa, 
# consultamos Grande-Vega, Farfán, Ondo e Fa (2016) e 
# Bonat et al. (2017). Seguindo Bonat et al. (2017) 

#---------------------------------------------------------------

# Dados

data("Hunting", package = "mcglm")
str(Hunting)

#---------------------------------------------------------------

# Exploratória

#---------------------------------------------------------------

# Preditores para média

form.OT <- OT ~ METHOD * SEX
form.BD <- BD ~ METHOD * SEX

#---------------------------------------------------------------

# Preditor matricial

Z0_ex5 <- mc_id(Hunting)
Z1_ex5 <- mc_mixed(~ 0 + HUNTER.MONTH, data = Hunting)

#---------------------------------------------------------------

# Ajuste

fit1_ex5 <- mcglm(linear_pred = c(form.BD, form.OT),
                  matrix_pred = list(c(Z0_ex5, Z1_ex5), 
                                     c(Z0_ex5, Z1_ex5)),
                  link = c("log", "log"), 
                  variance = c("poisson_tweedie",
                               "poisson_tweedie"),
                  offset = list(log(Hunting$OFFSET), 
                                log(Hunting$OFFSET)),
                  control_algorithm = list(max_iter = 200, 
                                           verbose = FALSE),
                  data = Hunting)

#---------------------------------------------------------------

# Resumos

summary(fit1_ex5, verbose = TRUE, print = "Regression")
summary(fit1_ex5, verbose = TRUE, print = "power")
summary(fit1_ex5, verbose = TRUE, print = "Dispersion")
summary(fit1_ex5, verbose = TRUE, print = "Correlation")

#---------------------------------------------------------------

# Anovas

mc_anova_I(fit1_ex5)
mc_anova_II(fit1_ex5)
mc_anova_III(fit1_ex5)

#---------------------------------------------------------------

# Manovas

mc_manova_I(fit1_ex5)
mc_manova_II(fit1_ex5)
mc_manova_III(fit1_ex5)

#---------------------------------------------------------------

# Avaliando parâmetros de dispersão

mc_anova_disp(fit1_ex5,
              p_var = list(c(0,1), c(0,1)),
              names = list(c('tau10', 'tau11'),
                           c('tau30', 'tau11')))

mc_manova_disp(fit1_ex5,
               p_var = c(0,1),
               names = c('tau11', 'tau21'))

#---------------------------------------------------------------

# Hipóteses lineares gerais

parametros <- coef(fit1_ex5, type = 'beta')[,-3]
parametros$beta_names <- Reduce(c,fit1_ex5$beta_names)
parametros

# Testando um único parâmetro
mc_linear_hypothesis(object =  fit1_ex5, 
                     hypothesis = c('beta11 = 0'))

# Testando dois parâmetros de uma mesma resposta
mc_linear_hypothesis(object =  fit1_ex5, 
                     hypothesis = c('beta11 = 0', 
                                    'beta12 = 0'))

# Testando parâmetros de diferentes respostas
mc_linear_hypothesis(object =  fit1_ex5, 
                     hypothesis = c('beta11 = 0', 
                                    'beta12 = 0',
                                    'beta21 = 0',
                                    'beta22 = 0'))

# Testando igualdade entre parâmetros
mc_linear_hypothesis(object =  fit1_ex5, 
                     hypothesis = c('beta11 = beta21'))

# Testando parêmtros de dispersão

coef(fit1_ex5, type = 'tau')[,-3]

mc_linear_hypothesis(object =  fit1_ex5, 
                     hypothesis = c('tau11 = 0'))

mc_linear_hypothesis(object =  fit1_ex5, 
                     hypothesis = c('tau11 = 0',
                                    'tau21 = 0'))

mc_linear_hypothesis(object =  fit1_ex5,
                     hypothesis = c('tau12 = tau22'))

#---------------------------------------------------------------

# Comparações múltiplas

## Por resposta

mc_multcomp(object = fit1_ex5,
            effect = list(c('METHOD', 'SEX'), 
                          c('METHOD', 'SEX')), 
            data = Hunting)

## Multivariado
mc_mult_multcomp(object = fit1_ex5, 
                 effect = c('METHOD', 'SEX'), 
                 data = Hunting)

#---------------------------------------------------------------


