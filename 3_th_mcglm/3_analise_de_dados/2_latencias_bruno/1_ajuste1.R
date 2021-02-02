#---------------------------------------------------------------
# AJUSTE
#---------------------------------------------------------------

# Preditor

form.min_lat <- min_lat ~ (lock + p0156 + p06 + p23 + p237 + p4 + p1 + p15 + p0)
form.max_lat <- max_lat ~ (lock + p0156 + p06 + p23 + p237 + p4 + p1 + p15 + p0)

#----------------------------------------------------------------

# Matrix linear predictor

Z0 <- mc_id(dados) # Identidade

#----------------------------------------------------------------

# Ajuste

fit <- 
  mcglm(linear_pred = c(form.min_lat,
                        form.max_lat),
        matrix_pred = list(c(Z0),
                           c(Z0)),
        link = c("log", "log"),
        variance = c("poisson_tweedie", "poisson_tweedie"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 0.1,
                                 max_iter = 600,
                                 tol = 0.1),
        power_fixed = c(F,F),
        data = dados)

#----------------------------------------------------------------

matplot(fit$IterationCovariance, type = 'l', xlim = c(1,60)) 

 #----------------------------------------------------------------

# Resumo do modelo

summary(fit)

coef(fit, type = 'beta')
coef(fit, type = 'tau')
coef(fit, type = 'power')

#Poisson (ϕ = 1 & p = 1)
#----------------------------------------------------------------