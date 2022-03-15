#---------------------------------------------------------------
# AJUSTE
#---------------------------------------------------------------

# Preditor

pred_YFAS <- YFAS_taxa ~ momento + grupo + momento:grupo

pred_BES <- BES_taxa ~ momento + grupo + momento:grupo

#----------------------------------------------------------------

# Matrix linear predictor

Z0 <- mc_id(dados_dissertacao) # Identidade

Z1 <- mc_mixed(~0 + factor(id), data = dados_dissertacao) # individuo

#----------------------------------------------------------------

# Ajuste

fit <- 
  mcglm(linear_pred = c(pred_YFAS,
                        pred_BES),
        matrix_pred = list(c(Z0,Z1),
                           c(Z0,Z1)),
        link = c("logit", "logit"),
        variance = c("binomialP", "binomialP"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 0.1,
                                 max_iter = 110,
                                 tol = 1e-4),
        power_fixed = c(F,F),
        data = dados_dissertacao)

#----------------------------------------------------------------

matplot(fit$IterationCovariance, type = 'l', xlim = c(1,160))

#----------------------------------------------------------------

# Resumo do modelo

summary(fit)

coef(fit, type = 'beta')
coef(fit, type = 'tau')
coef(fit, type = 'power')

#----------------------------------------------------------------