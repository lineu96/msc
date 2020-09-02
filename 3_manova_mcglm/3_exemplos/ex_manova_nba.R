
###############################################################
########################## EXEMPLO 2 ##########################
###############################################################

library(mcglm)
library(Matrix)
source('~/msc/3_manova_mcglm/2_funcoes/functions.R')

#---------------------------------------------------------------
# Reference: 
# The official NBA basketball Encyclopedia, Villard Books
#---------------------------------------------------------------

# Data set original

# The following data (X1, X2, X3, X4, X5) are for each player.
# X1 = height in feet
# X2 = weight in pounds
# X3 = percent of successful field goals (out of 100 attempted)
# X4 = percent of successful free throws (out of 100 attempted)
# X5 = average points scored per game

#---------------------------------------------------------------

# Modificado para ilustração

# x1: altura
# x2: peso
# x3: categórica qualquer (eu incluí)
# y1: % de cestas durante o jogo (exceto lance livre)
# y2: % de cestas lance livre
# y3: média de pontos

#---------------------------------------------------------------

dados <- read.csv2("NBA.csv", 
                   header = T, 
                   sep = ";", 
                   dec = ',')

dados$X6 <- sample(as.factor(rep(c('nivel_1', 'nivel_2', 
                            'nivel_3', 'nivel_4', 
                            'nivel_5', 'nivel_6'), 9)))

names(dados) <- c('x1', 'x2', 'y1', 'y2', 'y3', 'x3')

#---------------------------------------------------------------

# Exploratória

summary(dados)

disp <- function(form, data){
  plot(form, data)
  abline(lm(form, data), col = 2, lwd = 2)  
}

x11()
par(mfrow = c(3,3))

disp(y1~x1, dados)
disp(y1~x2, dados)
plot(y1~x3, dados)

disp(y2~x1, dados)
disp(y2~x2, dados)
plot(y2~x3, dados)

disp(y3~x1, dados)
disp(y3~x2, dados)
plot(y3~x3, dados)

cor1 <- cor(dados[,-ncol(dados)])

par(mfrow = c(1,1))
corrplot::corrplot.mixed(cor1, 
                         lower = 'number', 
                         upper = 'ellipse')

################################################################
#---------------------------------------------------------------
# Modelo sem interações
#----------------------------------------------------------------
################################################################

# PREDITOR

form1 <- y1 ~ x1+x2+x3
form2 <- y2 ~ x1+x2+x3
form3 <- y3 ~ x1+x2+x3

#----------------------------------------------------------------

# MATRIX LINEAR PREDICTOR

Z0 <- mc_id(dados) # Identidade

#----------------------------------------------------------------

# AJUSTE DO MODELO

fit <- 
  mcglm(linear_pred = c(form1,
                        form2,
                        form3),
        matrix_pred = list(c(Z0),
                           c(Z0),
                           c(Z0)),
        link = c("identity","identity","identity"),
        variance = c("tweedie",
                     "tweedie",
                     "tweedie"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 0.1,
                                 max_iter = 20,
                                 tol = 1e-01),
        data = dados)

#----------------------------------------------------------------

# RESUMO DO MODELO

fit$beta_names[[1]]
summary(fit)

#---------------------------------------------------------------

# ANOVA

mc_anova_pc <- anova(fit)

i = 1
mc_anova_pc[[i]]
mc_anova_I(fit)[[i]]
mc_anova_II(fit)[[i]]
mc_anova_III(fit)[[i]]

mc_anova_disp(object =  fit,
              p_var = c(0,0,0),
              names = c('t0', 't0', 't0'))

#---------------------------------------------------------------

# MANOVA

mc_manova(fit)
mc_manova_I(fit)
mc_manova_II(fit)
mc_manova_III(fit)

mc_manova_disp(fit,
               p_var = 0,
               names = 't1')


################################################################
#---------------------------------------------------------------
# Modelo com interações
#----------------------------------------------------------------
################################################################

# PREDITOR

form1 <- y1 ~ (x1+x2+x3)^2
form2 <- y2 ~ (x1+x2+x3)^2
form3 <- y3 ~ (x1+x2+x3)^2

#----------------------------------------------------------------

# MATRIX LINEAR PREDICTOR

Z0 <- mc_id(dados) # Identidade

#----------------------------------------------------------------

# AJUSTE DO MODELO

fit <- 
  mcglm(linear_pred = c(form1,
                        form2,
                        form3),
        matrix_pred = list(c(Z0),
                           c(Z0),
                           c(Z0)),
        link = c("identity","identity","identity"),
        variance = c("tweedie",
                     "tweedie",
                     "tweedie"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 0.1,
                                 max_iter = 20,
                                 tol = 1e-01),
        data = dados)

#----------------------------------------------------------------

# RESUMO DO MODELO

fit$beta_names[[1]]
summary(fit)

#---------------------------------------------------------------

# ANOVA

mc_anova_pc <- anova(fit)

i = 1
mc_anova_pc[[i]]
mc_anova_I(fit)[[i]]
mc_anova_II(fit)[[i]]
mc_anova_III(fit)[[i]]

mc_anova_disp(object =  fit,
              p_var = list(c(0),
                           c(0),
                           c(0)),
              names = list(c('t1'),
                           c('t1'),
                           c('t1')))


#---------------------------------------------------------------

# MANOVA

mc_manova(fit)
mc_manova_I(fit)
mc_manova_II(fit)
mc_manova_III(fit)

mc_manova_disp(fit,
               p_var = 0,
               names = 't1')

#---------------------------------------------------------------
