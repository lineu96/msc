#---------------------------------------------------------------
# RESULTADOS
#---------------------------------------------------------------

# SAIDA DO MODELO
resumo <- summary(fit)

#---------------------------------------------------------------

# REGRESSÃO

betas_yale <- round(resumo[[1]]$Regression, 4)
betas_ecap <- round(resumo[[2]]$Regression, 4)

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

beta_yale <- data.frame(name = rownames(resumo$`Resp.Variable 1`$Regression),
                       exp_est = exp(round(resumo$`Resp.Variable 1`$Regression$Estimates,2)),
                       ic_min = as.vector(exp(confint(fit)[1:6,])[,1]),
                       ic_max = as.vector(exp(confint(fit)[1:6,])[,2]))

beta_ecap <- data.frame(name = rownames(resumo$`Resp.Variable 2`$Regression),
                       exp_est = exp(round(resumo$`Resp.Variable 2`$Regression$Estimates,2)),
                       ic_min = as.vector(exp(confint(fit)[7:12,])[,1]),
                       ic_max = as.vector(exp(confint(fit)[7:12,])[,2]))

beta_yale[,2:4] <- round(beta_yale[,2:4],2)
beta_ecap[,2:4] <- round(beta_ecap[,2:4],2)

beta_yale
beta_ecap

#---------------------------------------------------------------

table4 <- expand.grid(Grupo=levels(dados4$grupo),
                      Momento=levels(dados4$momento))

attach(table4)

# YALE

table4$lin_pred_yale <- 
  
  betas_yale$Estimate[1] +
  
  betas_yale$Estimate[2]*I(Momento==levels(dados4$momento)[2])+
  betas_yale$Estimate[3]*I(Momento==levels(dados4$momento)[3])+
  betas_yale$Estimate[4]*I(Grupo==levels(dados4$grupo)[2])+
  
  betas_yale$Estimate[5]*I(Momento==levels(dados4$momento)[2])*I(Grupo==levels(dados4$grupo)[2])+
  betas_yale$Estimate[6]*I(Momento==levels(dados4$momento)[3])*I(Grupo==levels(dados4$grupo)[2])

table4$mean_pred_yale <- round(exp(table4$lin_pred_yale),2)
table4$lin_pred_yale <- round(table4$lin_pred_yale,2)

# ECAP

table4$lin_pred_ecap <- 
  
  betas_ecap$Estimate[1] +
  
  betas_ecap$Estimate[2]*I(Momento==levels(dados4$momento)[2])+
  betas_ecap$Estimate[3]*I(Momento==levels(dados4$momento)[3])+
  betas_ecap$Estimate[4]*I(Grupo==levels(dados4$grupo)[2])+
  
  betas_ecap$Estimate[5]*I(Momento==levels(dados4$momento)[2])*I(Grupo==levels(dados4$grupo)[2])+
  betas_ecap$Estimate[6]*I(Momento==levels(dados4$momento)[3])*I(Grupo==levels(dados4$grupo)[2])

table4$mean_pred_ecap <- round(exp(table4$lin_pred_ecap),2)
table4$lin_pred_ecap <- round(table4$lin_pred_ecap,2)

detach(table4)

tabela <- table4[,c(1,2,4,6)]
names(tabela) <- c('Grupo', 'Momento', 'Yale predito', 'Ecap predito')

tabela

table4_plot <- data.frame(Grupo = rep(table4$Grupo,2),
                          Momento = rep(table4$Momento,2),
                          Métrica = c(rep('Yale',6),
                                      rep('Ecap',6)),
                          Predito = c(table4$mean_pred_yale, table4$mean_pred_ecap))

ggplot(table4_plot, aes(x=Momento, 
                        y=Predito, 
                        group = Grupo))+ 
  theme_bw() + 
  geom_line(aes(group=Grupo, linetype=Grupo)) +
  theme(legend.position = 'top') +
  #labs(title = "Ecap")+ 
  xlab('Experimental moment') + 
  ylab('Estimate')+ geom_point() +
  facet_wrap(~Métrica, scales = 'free')


#---------------------------------------------------------------