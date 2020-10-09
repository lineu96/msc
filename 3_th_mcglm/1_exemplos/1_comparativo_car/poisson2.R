#-------------------------------------------------------------------

library(mcglm)
library(Matrix)
source('~/msc/3_th_mcglm/0_funcoes/functions.R')

#-------------------------------------------------------------------

dados <- read.csv2("~/msc/3_th_mcglm/1_exemplos/0_dados/NBA.csv", 
                   header = T, 
                   sep = ";", 
                   dec = ',')

dados$X6 <- sample(as.factor(rep(c('nivel_1', 'nivel_2', 
                                   'nivel_3', 'nivel_4', 
                                   'nivel_5', 'nivel_6'), 9)))

names(dados) <- c('x1', 'x2', 'y1', 'y2', 'y3', 'x3')

dados$count <- rpois(nrow(dados), 2)

#-------------------------------------------------------------------

form <- count ~ x1+x2+x3

#-------------------------------------------------------------------

# GLM

m1 <- glm(formula = form, 
          family = 'poisson', 
          data = dados)

#-------------------------------------------------------------------

# McGLM
Z0 <- mc_id(dados) # Identidade

m2 <- mcglm(linear_pred = c(form), 
            matrix_pred = list(c(Z0)),
            link = "log", variance = "tweedie",
            power_fixed = TRUE, data = dados)

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
                       "x1 = 0",
                       "x2 = 0"))

mc_linear_hypothesis(object =  m2, 
                     parameters = c('beta10', 
                                    'beta11',
                                    'beta12'),
                     null_hyp = c(0,0,0))

linearHypothesis(m1, c("(Intercept) = 0.13"))

mc_linear_hypothesis(object =  m2, 
                     parameters = c('beta10'),
                     null_hyp = c(0.13))

#-------------------------------------------------------------------

form <- y1 ~ (x1+x2+x3)^2

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
                       "x1 = 0",
                       "x2 = 0"))

mc_linear_hypothesis(object =  m2, 
                     parameters = c('beta10', 
                                    'beta11',
                                    'beta12'),
                     null_hyp = c(0,0,0))

linearHypothesis(m1, c("(Intercept) = 0.13"))

mc_linear_hypothesis(object =  m2, 
                     parameters = c('beta10'),
                     null_hyp = c(0.13))

#-------------------------------------------------------------------