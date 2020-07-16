
###############################################################
########################## EXEMPLO 1 ##########################
###############################################################

library(mcglm)
library(Matrix)
source('~/msc/3_manova_mcglm/2_funcoes/functions2.R')

#---------------------------------------------------------------
# Análise comportamental de ovelhas submetidas à intervenção 
# humana
#---------------------------------------------------------------

#---------------------------------------------------------------
dados <- read.csv2("dados_ovelhas.csv", 
                   header = T, 
                   sep = ";", 
                   dec = ',')

dados2 <- dados[,c(1,2,3,4,
                   14,15,17,21,
                   7,9,10,11)]

dados2$momento <- ordered(dados2$tempo, 
                          levels=c("Antes", "Durante", "Depois"))

#---------------------------------------------------------------

################################################################
#---------------------------------------------------------------
# Modelo sem interações
#----------------------------------------------------------------
################################################################

# PREDITOR

form.ncorpo  <- ncorpo  ~ (sessao + tempo + linhagem)
form.ncorpo2 <- ncabeca ~ (sessao + tempo + linhagem)
form.ncorpo3 <- norelha ~ (sessao + tempo + linhagem)

#----------------------------------------------------------------

# MATRIX LINEAR PREDICTOR

Z0 <- mc_id(dados2) # Identidade

Z1 <- mc_mixed(~0 + factor(animal), data = dados2) # Animal

dados2$comb <- paste(dados2$animal, dados2$sessao)
Z2 <- mc_mixed(~0 + comb, data = dados2) # Animal Momento

#----------------------------------------------------------------

# AJUSTE DO MODELO

fit_jointP <- 
  mcglm(linear_pred = c(form.ncorpo,
                        form.ncorpo2,
                        form.ncorpo3),
        matrix_pred = list(c(Z0, Z1, Z2),
                           c(Z0, Z1, Z2),
                           c(Z0, Z1, Z2)),
        link = c("log","log","log"),
        variance = c("poisson_tweedie",
                     "poisson_tweedie",
                     "poisson_tweedie"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 0.1,
                                 max_iter = 20,
                                 tol = 1e-01),
        power_fixed = c(F, F, F),
        data = dados2)
#----------------------------------------------------------------

# RESUMO DO MODELO

fit_jointP$beta_names[[1]]
summary(fit_jointP)

#---------------------------------------------------------------

# ANOVA

mc_anova_pc <- anova(fit_jointP)

i = 1
mc_anova_pc[[i]]
mc_anova_I(fit_jointP)[[i]]
mc_anova_II(fit_jointP)[[i]]
mc_anova_III(fit_jointP)[[i]]

mc_anova_disp(fit_jointP)
mc_anova_power(fit_jointP)

#---------------------------------------------------------------

# MANOVA

mc_manova(fit_jointP)
mc_manova_I(fit_jointP)
mc_manova_II(fit_jointP)
mc_manova_III(fit_jointP)

mc_manova_disp(fit_jointP)
mc_manova_power(fit_jointP)

################################################################
#---------------------------------------------------------------
# Modelo com interações
#----------------------------------------------------------------
################################################################

# PREDITOR

form.ncorpo  <- ncorpo  ~ (sessao + tempo + linhagem)^2
form.ncorpo2 <- ncabeca ~ (sessao + tempo + linhagem)^2
form.ncorpo3 <- norelha ~ (sessao + tempo + linhagem)^2

#----------------------------------------------------------------

# MATRIX LINEAR PREDICTOR

Z0 <- mc_id(dados2) # Identidade

Z1 <- mc_mixed(~0 + factor(animal), data = dados2) # Animal

dados2$comb <- paste(dados2$animal, dados2$sessao)
Z2 <- mc_mixed(~0 + comb, data = dados2) # Animal Momento

#----------------------------------------------------------------

# AJUSTE DO MODELO

fit_jointP <- 
  mcglm(linear_pred = c(form.ncorpo,
                        form.ncorpo2,
                        form.ncorpo3),
        matrix_pred = list(c(Z0, Z1, Z2),
                           c(Z0, Z1, Z2),
                           c(Z0, Z1, Z2)),
        link = c("log","log","log"),
        variance = c("poisson_tweedie",
                     "poisson_tweedie",
                     "poisson_tweedie"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 0.1,
                                 max_iter = 20,
                                 tol = 1e-01),
        power_fixed = c(F, F, F),
        data = dados2)
#----------------------------------------------------------------

# RESUMO DO MODELO

fit_jointP$beta_names[[1]]
summary(fit_jointP)

#---------------------------------------------------------------

# ANOVA

mc_anova_pc <- anova(fit_jointP)

i = 1
mc_anova_pc[[i]]
mc_anova_I(fit_jointP)[[i]]
mc_anova_II(fit_jointP)[[i]]
mc_anova_III(fit_jointP)[[i]]

mc_anova_disp(fit_jointP)
mc_anova_power(fit_jointP)

#---------------------------------------------------------------

# MANOVA

mc_manova(fit_jointP)
mc_manova_I(fit_jointP)
mc_manova_II(fit_jointP)
mc_manova_III(fit_jointP)

mc_manova_disp(fit_jointP)
mc_manova_power(fit_jointP)

#---------------------------------------------------------------