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
confint(fit)[1:7,]
confint(fit)[8:14,]

confint(fit)[15:17,]

#---------------------------------------------------------------

# ANOVA
anova(fit)

#---------------------------------------------------------------

# MANOVA
mc_manova(fit)

#---------------------------------------------------------------

# FUNÇÃO PARA PREDITOS

predito <- function(p23, p4, p015, 
                    p5, p0, p01){
  
  betas <- data.frame(beta_name = c('intercept',
                                    'p23', 'p4', 'p015', 
                                    'p5', 'p0', 'p01'),
                      estim_min = coef(fit, type = 'beta')$Estimates[1:7],
                      estim_max = coef(fit, type = 'beta')$Estimates[8:14])
  
  
  min <- exp(
      betas$estim_min[1] +           #intercept
      betas$estim_min[2]  *  p23 +
      betas$estim_min[3]  *  p4 +
      betas$estim_min[4]  *  p015 +
      betas$estim_min[5]  *  p5 +
      betas$estim_min[6]  *  p0 +
      betas$estim_min[7]  *  p01
  )
  
  max <- exp(
    betas$estim_max[1] +           #intercept
    betas$estim_max[2]  *  p23 +
    betas$estim_max[3]  *  p4 +
    betas$estim_max[4]  *  p015 +
    betas$estim_max[5]  *  p5 +
    betas$estim_max[6]  *  p0 +
    betas$estim_max[7]  *  p01
  )
  
  out <- data.frame(min_lat = min,
                    max_lat = max)
  
  return(out)  
}

#---------------------------------------------------------------

# PREDITOS TESTE

dados[sample(nrow(massa),1),c('p23', 'p4', 'p015', 
                              'p5', 'p0', 'p01',
                              'min_lat',
                              'max_lat')]

predito(p23  = 0, 
        p4   = 0, 
        p015 = 0, 
        p5   = 1, 
        p0   = 0, 
        p01  = 1)

#---------------------------------------------------------------