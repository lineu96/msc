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

dat <- expand.grid(Lineage=c("R-","R+"),
                   Session=c("Session 1","Session 2","Session 3"),
                   Moment=c("Pre","Dur.","Post"))

attach(dat)

coef_count <- as.data.frame(betas_count$Estimates)
coef_prop <- as.data.frame(betas_prop$Estimates)
coef_count$name <- row.names(betas_count)
coef_prop$name <- row.names(betas_count)

dat$lin.pred_count <- 
  coef_count[1,1] +
  coef_count[2,1]*I(Session=="Session 2")+
  coef_count[3,1]*I(Session=="Session 3")+
  coef_count[4,1]*I(Moment=="Post")+
  coef_count[5,1]*I(Moment=="Dur.")+
  coef_count[6,1]*I(Lineage=="R+")+
  
  coef_count[7,1]*I(Session=="Session 2")*I(Moment=="Post")+
  coef_count[8,1]*I(Session=="Session 3")*I(Moment=="Post")+
  coef_count[9,1]*I(Session=="Session 2")*I(Moment=="Dur.")+
  coef_count[10,1]*I(Session=="Session 3")*I(Moment=="Dur.")+
  
  coef_count[11,1]*I(Session=="Session 2")*I(Lineage=="R+")+
  coef_count[12,1]*I(Session=="Session 3")*I(Lineage=="R+")+
  
  coef_count[13,1]*I(Lineage=="R+")*I(Moment=="Post")+
  coef_count[14,1]*I(Lineage=="R+")*I(Moment=="Dur.")

dat$lin.pred_prop <- 
  coef_prop[1,1] +
  coef_prop[2,1]*I(Session=="Session 2")+
  coef_prop[3,1]*I(Session=="Session 3")+
  coef_prop[4,1]*I(Moment=="Post")+
  coef_prop[5,1]*I(Moment=="Dur.")+
  coef_prop[6,1]*I(Lineage=="R+")+
  
  coef_prop[7,1]*I(Session=="Session 2")*I(Moment=="Post")+
  coef_prop[8,1]*I(Session=="Session 3")*I(Moment=="Post")+
  coef_prop[9,1]*I(Session=="Session 2")*I(Moment=="Dur.")+
  coef_prop[10,1]*I(Session=="Session 3")*I(Moment=="Dur.")+
  
  coef_prop[11,1]*I(Session=="Session 2")*I(Lineage=="R+")+
  coef_prop[12,1]*I(Session=="Session 3")*I(Lineage=="R+")+
  
  coef_prop[13,1]*I(Lineage=="R+")*I(Moment=="Post")+
  coef_prop[14,1]*I(Lineage=="R+")*I(Moment=="Dur.")

detach(dat)

dat$mean.pred_count <- exp(dat$lin.pred_count)
dat$mean.pred_prop <- plogis(dat$lin.pred_prop)

pred_mod <- dat[,c("Lineage","Session","Moment",
                   "mean.pred_count","mean.pred_prop")]

names(pred_mod) <- c("Lineage", "Session", "Moment",
                     "Contagem predita", "Proporção predita")


pred_mod[,4:5] <- round(pred_mod[,4:5], 3)

pred_mod

ggplot(pred_mod, aes(x=Moment, 
                     y=`Contagem predita`, 
                     group=Lineage))+ 
  theme_bw() + 
  geom_line(aes(group=Lineage, linetype=Lineage)) +
  theme(legend.position = 'top') +
  facet_wrap(~Session) +
  #labs(title = "norelha")+ 
  xlab('Experimental moment') + 
  ylab('Estimate')+ geom_point()

ggplot(pred_mod, aes(x=Moment, 
                     y=`Proporção predita`, 
                     group=Lineage))+ 
  theme_bw() + 
  geom_line(aes(group=Lineage, linetype=Lineage)) +
  theme(legend.position = 'top') +
  facet_wrap(~Session) +
  #labs(title = "norelha")+ 
  xlab('Experimental moment') + 
  ylab('Estimate')+ geom_point()

#---------------------------------------------------------------