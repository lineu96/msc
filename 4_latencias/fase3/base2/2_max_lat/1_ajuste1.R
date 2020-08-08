#---------------------------------------------------------------
# AJUSTE
#---------------------------------------------------------------

# Preditor

form.max_lat <- max_lat ~ (p23 + p4 + p015 + p5 + p0 + p01)

#----------------------------------------------------------------

# Matrix linear predictor

Z0 <- mc_id(massa) # Identidade

#----------------------------------------------------------------

# Ajuste

fit <- 
  mcglm(linear_pred = c(form.max_lat),
        matrix_pred = list(c(Z0)),
        link = c("log"),
        variance = c("poisson_tweedie"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 1,
                                 max_iter = 200,
                                 tol = 1e-04),
        power_fixed = c(F),
        data = massa)

#----------------------------------------------------------------

matplot(fit$IterationCovariance, type = 'l', xlim = c(1,45)) 

#----------------------------------------------------------------

# Resumo do modelo

summary(fit)

coef(fit, type = 'beta')
coef(fit, type = 'tau')
coef(fit, type = 'power')

#Poisson (ϕ = 1 & p = 1)
#----------------------------------------------------------------