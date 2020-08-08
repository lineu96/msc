#---------------------------------------------------------------
# AJUSTE
#---------------------------------------------------------------

# Preditor

form.max_lat <- max_lat ~ (lock + p0156 + p06 + p23 + p237 + p4 + p1 + p15 + p0# + p0156*p23 + count
                                    )

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
                                 tuning = 0.1,
                                 max_iter = 20,
                                 tol = 1e-01),
        power_fixed = c(F),
        data = massa)

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