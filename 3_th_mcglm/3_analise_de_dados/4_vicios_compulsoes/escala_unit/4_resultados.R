#---------------------------------------------------------------
# RESULTADOS
#---------------------------------------------------------------

# SAIDA DO MODELO
resumo <- summary(fit)

#---------------------------------------------------------------

# REGRESSÃO

betas_YFAS <- round(resumo[[1]]$Regression, 4)
betas_BES <- round(resumo[[2]]$Regression, 4)

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

beta_YFAS <- data.frame(name = rownames(resumo$`Resp.Variable 1`$Regression),
                       exp_est = exp(round(resumo$`Resp.Variable 1`$Regression$Estimates,2)),
                       ic_min = as.vector(exp(confint(fit)[1:6,])[,1]),
                       ic_max = as.vector(exp(confint(fit)[1:6,])[,2]))

beta_BES <- data.frame(name = rownames(resumo$`Resp.Variable 2`$Regression),
                       exp_est = exp(round(resumo$`Resp.Variable 2`$Regression$Estimates,2)),
                       ic_min = as.vector(exp(confint(fit)[7:12,])[,1]),
                       ic_max = as.vector(exp(confint(fit)[7:12,])[,2]))

beta_YFAS[,2:4] <- round(beta_YFAS[,2:4],2)
beta_BES[,2:4] <- round(beta_BES[,2:4],2)

beta_YFAS
beta_BES

#---------------------------------------------------------------

table <- expand.grid(Grupo=levels(dados_dissertacao$grupo),
                      Momento=levels(dados_dissertacao$momento))

attach(table)

# YFAS

table$lin_pred_YFAS <- 
  
  betas_YFAS$Estimate[1] +
  
  betas_YFAS$Estimate[2]*I(Momento==levels(dados_dissertacao$momento)[2])+
  betas_YFAS$Estimate[3]*I(Momento==levels(dados_dissertacao$momento)[3])+
  betas_YFAS$Estimate[4]*I(Grupo==levels(dados_dissertacao$grupo)[2])+
  
  betas_YFAS$Estimate[5]*I(Momento==levels(dados_dissertacao$momento)[2])*I(Grupo==levels(dados_dissertacao$grupo)[2])+
  betas_YFAS$Estimate[6]*I(Momento==levels(dados_dissertacao$momento)[3])*I(Grupo==levels(dados_dissertacao$grupo)[2])

table$mean_pred_YFAS <- round(exp(table$lin_pred_YFAS),2)
table$lin_pred_YFAS <- round(table$lin_pred_YFAS,2)

# BES

table$lin_pred_BES <- 
  
  betas_BES$Estimate[1] +
  
  betas_BES$Estimate[2]*I(Momento==levels(dados_dissertacao$momento)[2])+
  betas_BES$Estimate[3]*I(Momento==levels(dados_dissertacao$momento)[3])+
  betas_BES$Estimate[4]*I(Grupo==levels(dados_dissertacao$grupo)[2])+
  
  betas_BES$Estimate[5]*I(Momento==levels(dados_dissertacao$momento)[2])*I(Grupo==levels(dados_dissertacao$grupo)[2])+
  betas_BES$Estimate[6]*I(Momento==levels(dados_dissertacao$momento)[3])*I(Grupo==levels(dados_dissertacao$grupo)[2])

table$mean_pred_BES <- round(exp(table$lin_pred_BES),2)
table$lin_pred_BES <- round(table$lin_pred_BES,2)

detach(table)

tabela <- table[,c(1,2,4,6)]
names(tabela) <- c('Grupo', 'Momento', 'YFAS predito', 'BES predito')

tabela

table_plot <- data.frame(Grupo = rep(table$Grupo,2),
                          Momento = rep(table$Momento,2),
                          Métrica = c(rep('YFAS',6),
                                      rep('BES',6)),
                          Predito = c(table$mean_pred_YFAS, 
                                      table$mean_pred_BES))

ggplot(table_plot, aes(x=Momento, 
                        y=Predito, 
                        group = Grupo))+ 
  theme_bw() + 
  geom_line(aes(group=Grupo, linetype=Grupo)) +
  theme(legend.position = 'top') +
  #labs(title = "BES")+ 
  xlab('Experimental moment') + 
  ylab('Estimate')+ geom_point() +
  facet_wrap(~Métrica, scales = 'free')


#---------------------------------------------------------------