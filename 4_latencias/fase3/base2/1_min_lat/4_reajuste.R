#---------------------------------------------------------------
# REAJUSTE
#---------------------------------------------------------------

# Preditor

form.min_lat <- min_lat ~ (p23 + p4 + p015 + p5 + p0 + p01 + count)

#----------------------------------------------------------------

# Matrix linear predictor

Z0 <- mc_id(massa2) # Identidade

#----------------------------------------------------------------

# Ajuste

fit <- 
  mcglm(linear_pred = c(form.min_lat),
        matrix_pred = list(c(Z0)),
        link = c("log"),
        variance = c("poisson_tweedie"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 0.1,
                                 max_iter = 20,
                                 tol = 1e-01),
        power_fixed = c(F),
        data = massa2)

#----------------------------------------------------------------

matplot(fit$IterationCovariance, type = 'l', xlim = c(1,10)) 

#----------------------------------------------------------------

# Resumo do modelo

summary(fit)

coef(fit, type = 'beta')
coef(fit, type = 'tau')
coef(fit, type = 'power')

#Poisson (ϕ = 1 & p = 1)

#----------------------------------------------------------------