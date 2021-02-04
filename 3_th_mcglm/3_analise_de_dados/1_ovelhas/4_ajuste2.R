# #---------------------------------------------------------------
# # AJUSTE
# #---------------------------------------------------------------
# 
# # Preditor
# 
# form.count <- count ~ (session + moment + lineage)^2
# form.prop <- prop ~ (session + moment + lineage)^2
# 
# #----------------------------------------------------------------
# 
# # Matrix linear predictor
# 
# Z0 <- mc_id(data_rbb2) # Identidade
# 
# Z1 <- mc_mixed(~0 + factor(animal), data = data_rbb2) # Animal
# 
# data_rbb2$comb <- paste(data_rbb2$animal, data_rbb2$session)
# Z2 <- mc_mixed(~0 + comb, data = data_rbb2) # Animal Sessão
# 
# #----------------------------------------------------------------
# 
# # Ajuste
# 
# fit2 <- 
#   mcglm(linear_pred = c(form.count, form.prop),
#         matrix_pred = list(c(Z0, Z1, Z2), 
#                            c(Z0, Z1, Z2)),
#         link = c("log", "logit"),
#         variance = c("poisson_tweedie", "binomialP"), 
#         control_algorithm = list(verbose = T, 
#                                  tuning = 0.9,
#                                  max_iter = 80,
#                                  tol = 1e-04),
#         power_fixed = c(F,F),
#         data = data_rbb2)
# 
# #----------------------------------------------------------------
# 
# matplot(fit2$IterationCovariance, type = 'l', xlim = c(1,70)) 
# 
# #----------------------------------------------------------------
# 
# # Resumo do modelo
# 
# summary(fit2)
# 
# coef(fit2, type = 'beta')
# coef(fit2, type = 'tau')
# coef(fit2, type = 'power')
# 
# #Poisson (ϕ = 1 & p = 1)
# #----------------------------------------------------------------