#---------------------------------------------------------------
# RESULTADOS
#---------------------------------------------------------------

# SAIDA DO MODELO

resumo <- summary(fit)

#---------------------------------------------------------------

# TAU E P PARA CADA RESPOSTA
round(resumo$`Resp.Variable 1`$Power,2)
round(resumo$`Resp.Variable 1`$tau,2)

round(resumo$`Resp.Variable 2`$Power,2)
round(resumo$`Resp.Variable 2`$tau,2)

#---------------------------------------------------------------

# PARAMETROS DE REGRESSAO PARA CADA RESPOSTA
round(resumo$`Resp.Variable 1`$Regression,2)
round(resumo$`Resp.Variable 2`$Regression,2)

#---------------------------------------------------------------

# INTERVALOS DE CONFIANÇA
confint(fit)[1:10,]
confint(fit)[11:20,]

confint(fit)[21:25,]

#---------------------------------------------------------------

# ANOVA
anova(fit)

#---------------------------------------------------------------

# MANOVA
mc_manova(fit)

#---------------------------------------------------------------

# FUNÇÃO PARA PREDITOS

predito <- function(lock, p0156, p06, 
                    p23, p237, p4, 
                    p1, p15, p0){
  
  betas <- data.frame(beta_name = c('intercept', 
                                    'lock', 'p0156', 
                                    'p06', 'p23', 
                                    'p237', 'p4', 'p1',
                                    'p15', 'p0'),
                      estim_min = coef(fit, type = 'beta')$Estimates[1:10],
                      estim_max = coef(fit, type = 'beta')$Estimates[11:20])
  
  
  min <- exp(
    betas$estim_min[1] +           #intercept
      betas$estim_min[2]  *  lock  + #lock
      betas$estim_min[3]  *  p0156 + #p0156
      betas$estim_min[4]  *  p06   + #p06
      betas$estim_min[5]  *  p23   + #p23
      betas$estim_min[6]  *  p237  + #p237
      betas$estim_min[7]  *  p4    + #p4
      betas$estim_min[8]  *  p1    + #p1
      betas$estim_min[9]  *  p15   + #p15
      betas$estim_min[10] * p0)      #p0
  
  max <- exp(
    betas$estim_max[1] +            #intercept
      betas$estim_max[2]  *  lock  +  #lock
      betas$estim_max[3]  *  p0156 +  #p0156
      betas$estim_max[4]  *  p06   +  #p06
      betas$estim_max[5]  *  p23   +  #p23
      betas$estim_max[6]  *  p237  +  #p237
      betas$estim_max[7]  *  p4    +  #p4
      betas$estim_max[8]  *  p1    +  #p1
      betas$estim_max[9]  *  p15   +  #p15
      betas$estim_max[10]  *  p0)      #p0
  
  out <- data.frame(min_lat = min,
                    max_lat = max)
  
  return(out)  
}

#---------------------------------------------------------------

# PREDITOS TESTE

dados[sample(nrow(massa),1),c('lock',
                              'p0156',
                              'p06',
                              'p23',
                              'p237',
                              'p4',
                              'p1',
                              'p15',
                              'p0',
                              'min_lat',
                              'max_lat')]

predito(lock  =  0,
        p0156 =  2,
        p06   =  1,
        p23   =  1,
        p237  =  0,
        p4    =  0,
        p1    =  1,
        p15   =  0,
        p0    =  0)

#---------------------------------------------------------------