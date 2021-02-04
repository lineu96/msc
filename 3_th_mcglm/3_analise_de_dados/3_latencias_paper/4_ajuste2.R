#---------------------------------------------------------------
# AJUSTE
#---------------------------------------------------------------

# Preditor

form.min_lat <- min_lat ~ (lock + p0156 + p06 + p23 + p237 + p4 + p1# + p15 + p0
                           )
form.max_lat <- max_lat ~ (lock + p0156 + p06 + p23 + p237 + p4 + p1# + p15 + p0
                           )

#----------------------------------------------------------------

# Matrix linear predictor

Z0 <- mc_id(massa) # Identidade

#----------------------------------------------------------------

# Ajuste

fit2 <- 
  mcglm(linear_pred = c(form.min_lat,
                        form.max_lat),
        matrix_pred = list(c(Z0),
                           c(Z0)),
        link = c("log", "log"),
        variance = c("poisson_tweedie", "poisson_tweedie"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 0.5,
                                 max_iter = 60,
                                 tol = 1e-8),
        power_fixed = c(F,F),
        data = massa)

#----------------------------------------------------------------

matplot(fit2$IterationCovariance, type = 'l', xlim = c(1,50)) 

#----------------------------------------------------------------

# Resumo do modelo

summary(fit2)

coef(fit2, type = 'beta')
coef(fit2, type = 'tau')
coef(fit2, type = 'power')

#Poisson (ϕ = 1 & p = 1)
#----------------------------------------------------------------