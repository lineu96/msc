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

confint(fit)[17:21,]

#---------------------------------------------------------------

# PARAMETROS DE REGRESSAO

beta_min <- data.frame(name = rownames(resumo$`Resp.Variable 1`$Regression),
                       exp_est = exp(round(resumo$`Resp.Variable 1`$Regression$Estimates,2)),
                       ic_min = as.vector(exp(confint(fit)[1:8,])[,1]),
                       ic_max = as.vector(exp(confint(fit)[1:8,])[,2]))

beta_max <- data.frame(name = rownames(resumo$`Resp.Variable 2`$Regression),
                       exp_est = exp(round(resumo$`Resp.Variable 2`$Regression$Estimates,2)),
                       ic_min = as.vector(exp(confint(fit)[9:16,])[,1]),
                       ic_max = as.vector(exp(confint(fit)[9:16,])[,2]))

beta_min[,2:4] <- round(beta_min[,2:4],2)
beta_max[,2:4] <- round(beta_max[,2:4],2)

beta_min
beta_max

#---------------------------------------------------------------

# INTERPRETAÇÃO

# Numericas

## p0156
paste0('Para cada unidade de aumento no uso da porta ', beta_min$name[3], 
       ' a media da latencia minima fica multiplicada por ', beta_min$exp_est[3], 
       ' mantendo fixos os valores das demais variáveis. Com 95% de confiança o intervalo entre ',
       beta_min$ic_min[3], ' e ', beta_min$ic_max[3], ' realmente contém o parâmetro.')

# Binarias

##p41
paste0('A media da latencia minima quando é usada a porta ', beta_min$name[7], 
       ' é ', beta_min$exp_est[7], ' vezes a media de quando ela não é usada, mantendo fixos os valores das demais variáveis. Com 95% de confiança o intervalo entre ',
       beta_min$ic_min[7], ' e ', beta_min$ic_max[7], ' realmente contém o parâmetro.')

#---------------------------------------------------------------

# ANOVA
anova(fit)

#---------------------------------------------------------------

# MANOVA
mc_manova(fit)

#---------------------------------------------------------------

# FUNÇÃO PARA PREDITOS

form.min_lat

predito <- function(lock, p0156, p06, 
                    p23, p237, p4, 
                    p1){
  
  betas <- data.frame(beta_name = c('intercept', 
                                    'lock', 'p0156', 
                                    'p06', 'p23', 
                                    'p237', 'p4', 'p1'),
                      estim_min = coef(fit, type = 'beta')$Estimates[1:8],
                      estim_max = coef(fit, type = 'beta')$Estimates[9:16])
  
  
  min <- exp(
    betas$estim_min[1] +           #intercept
      betas$estim_min[2]  *  lock  + #lock
      betas$estim_min[3]  *  p0156 + #p0156
      betas$estim_min[4]  *  p06   + #p06
      betas$estim_min[5]  *  p23   + #p23
      betas$estim_min[6]  *  p237  + #p237
      betas$estim_min[7]  *  p4    + #p4
      betas$estim_min[8]  *  p1      #p1
      )
  
  max <- exp(
    betas$estim_max[1] +            #intercept
      betas$estim_max[2]  *  lock  +  #lock
      betas$estim_max[3]  *  p0156 +  #p0156
      betas$estim_max[4]  *  p06   +  #p06
      betas$estim_max[5]  *  p23   +  #p23
      betas$estim_max[6]  *  p237  +  #p237
      betas$estim_max[7]  *  p4    +  #p4
      betas$estim_max[8]  *  p1       #p1
      )
  
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
                              'min_lat',
                              'max_lat')]

predito(lock  =  0,
        p0156 =  0,
        p06   =  1,
        p23   =  1,
        p237  =  1,
        p4    =  1,
        p1    =  0)

#---------------------------------------------------------------