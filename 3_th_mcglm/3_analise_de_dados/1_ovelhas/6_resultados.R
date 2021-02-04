#---------------------------------------------------------------
# RESULTADOS
#---------------------------------------------------------------

# SAIDA DO MODELO
resumo <- summary(fit)

#---------------------------------------------------------------

# REGRESSÃO

betas_count <- round(resumo[[1]]$Regression, 4)
betas_prop <- round(resumo[[2]]$Regression, 4)

#---------------------------------------------------------------

# TAU E P

round(resumo$`Resp.Variable 1`$Power,4)
round(resumo$`Resp.Variable 1`$tau,4)

round(resumo$`Resp.Variable 2`$Power,4)
round(resumo$`Resp.Variable 2`$tau,4)

#---------------------------------------------------------------

# INTERVALOS DE CONFIANÇA

confint(fit)

#---------------------------------------------------------------

# PARAMETROS DE REGRESSAO

beta_count <- data.frame(name = rownames(resumo$`Resp.Variable 1`$Regression),
                       exp_est = exp(round(resumo$`Resp.Variable 1`$Regression$Estimates,2)),
                       ic_min = as.vector(exp(confint(fit)[1:14,])[,1]),
                       ic_max = as.vector(exp(confint(fit)[1:14,])[,2]))

beta_prop <- data.frame(name = rownames(resumo$`Resp.Variable 2`$Regression),
                       exp_est = exp(round(resumo$`Resp.Variable 2`$Regression$Estimates,2)),
                       ic_min = as.vector(exp(confint(fit)[15:28,])[,1]),
                       ic_max = as.vector(exp(confint(fit)[15:28,])[,2]))

beta_count[,2:4] <- round(beta_count[,2:4],2)
beta_prop[,2:4] <- round(beta_prop[,2:4],2)

beta_count
beta_prop

#---------------------------------------------------------------
