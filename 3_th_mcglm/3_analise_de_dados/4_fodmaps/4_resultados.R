#---------------------------------------------------------------
# RESULTADOS
#---------------------------------------------------------------

# SAIDA DO MODELO
resumo <- summary(fit)

#---------------------------------------------------------------

# REGRESSÃO

betas_nausea <- round(resumo[[1]]$Regression, 4)
betas_fe_duras <- round(resumo[[2]]$Regression, 4)
betas_urg_defec <- round(resumo[[3]]$Regression, 4)
betas_n_esvaz <- round(resumo[[4]]$Regression, 4)

#---------------------------------------------------------------

# TAU

round(resumo$`Resp.Variable 1`$tau,4)
round(resumo$`Resp.Variable 2`$tau,4)
round(resumo$`Resp.Variable 3`$tau,4)
round(resumo$`Resp.Variable 4`$tau,4)

#---------------------------------------------------------------

# INTERVALOS DE CONFIANÇA

confint(fit)

#---------------------------------------------------------------

# PARAMETROS DE REGRESSAO

beta_nausea <- data.frame(name = rownames(resumo$`Resp.Variable 1`$Regression),
                       exp_est = exp(round(resumo$`Resp.Variable 1`$Regression$Estimates,2)),
                       ic_min = as.vector(exp(confint(fit)[1:13,])[,1]),
                       ic_max = as.vector(exp(confint(fit)[1:13,])[,2]))

beta_fe_duras <- data.frame(name = rownames(resumo$`Resp.Variable 2`$Regression),
                       exp_est = exp(round(resumo$`Resp.Variable 2`$Regression$Estimates,2)),
                       ic_min = as.vector(exp(confint(fit)[14:26,])[,1]),
                       ic_max = as.vector(exp(confint(fit)[14:26,])[,2]))

beta_urg_defec <- data.frame(name = rownames(resumo$`Resp.Variable 3`$Regression),
                            exp_est = exp(round(resumo$`Resp.Variable 3`$Regression$Estimates,2)),
                            ic_min = as.vector(exp(confint(fit)[27:39,])[,1]),
                            ic_max = as.vector(exp(confint(fit)[27:39,])[,2]))

beta_n_esvaz <- data.frame(name = rownames(resumo$`Resp.Variable 4`$Regression),
                             exp_est = exp(round(resumo$`Resp.Variable 4`$Regression$Estimates,2)),
                             ic_min = as.vector(exp(confint(fit)[40:52,])[,1]),
                             ic_max = as.vector(exp(confint(fit)[40:52,])[,2]))


beta_nausea[,2:4] <- round(beta_nausea[,2:4],2)
beta_fe_duras[,2:4] <- round(beta_fe_duras[,2:4],2)
beta_urg_defec[,2:4] <- round(beta_urg_defec[,2:4],2)
beta_n_esvaz[,2:4] <- round(beta_n_esvaz[,2:4],2)

beta_nausea
beta_fe_duras
beta_urg_defec
beta_n_esvaz

#---------------------------------------------------------------
