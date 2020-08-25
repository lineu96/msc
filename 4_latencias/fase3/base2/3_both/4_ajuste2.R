#---------------------------------------------------------------
# AJUSTE
#---------------------------------------------------------------

# Preditor

form.min_lat <- min_lat ~ (p23 + p015 + p5 + p0 + p01)
form.max_lat <- max_lat ~ (p23 + p015 + p5 + p0 + p01)

#----------------------------------------------------------------

# Matrix linear predictor

Z0 <- mc_id(massa) # Identidade

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
                                 tuning = 0.2,
                                 max_iter = 500,
                                 tol = 1e-08),
        #power_fixed = c(F,F),
        data = massa)

#----------------------------------------------------------------

matplot(fit$IterationCovariance, type = 'l', xlim = c(1,490)) 

#----------------------------------------------------------------

# Resumo do modelo

summary(fit)

coef(fit, type = 'beta')
coef(fit, type = 'tau')
coef(fit, type = 'power')

#Poisson (ϕ = 1 & p = 1)
#----------------------------------------------------------------