#---------------------------------------------------------------
# AJUSTE
#---------------------------------------------------------------

# Preditor

form.count <- count ~ (session + moment + lineage)^2
form.prop <- prop ~ (session + moment + lineage)^2

#----------------------------------------------------------------

# Matrix linear predictor

Z0 <- mc_id(data_rbb) # Identidade

Z1 <- mc_mixed(~0 + factor(animal), data = data_rbb) # Animal

data_rbb$comb <- paste(data_rbb$animal, data_rbb$session)
Z2 <- mc_mixed(~0 + comb, data = data_rbb) # Animal Sessão

#----------------------------------------------------------------

# Ajuste

fit <- 
  mcglm(linear_pred = c(form.count, form.prop),
        matrix_pred = list(c(Z0, Z1, Z2), 
                           c(Z0, Z1, Z2)),
        link = c("log", "logit"),
        variance = c("poisson_tweedie", "binomialP"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 0.5,
                                 max_iter = 80,
                                 tol = 1e-08),
        power_fixed = c(F,F),
        data = data_rbb)

#----------------------------------------------------------------

matplot(fit$IterationCovariance, type = 'l', xlim = c(1,85)) 

#----------------------------------------------------------------

# Resumo do modelo

summary(fit)

coef(fit, type = 'beta')
coef(fit, type = 'tau')
coef(fit, type = 'power')

#Poisson (ϕ = 1 & p = 1)
#----------------------------------------------------------------