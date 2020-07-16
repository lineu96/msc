#################################################################
# MANOVA VIA TESTE WALD TIPO III PARA PARAMETROS DE POTENCIA
#################################################################

mc_manova_power <- function(object){
  
  #----------------------------------------------------------------
  
  # Vetor power
  power <- coef(object, type = "power")[,c(1,2, 4)]
  
  #----------------------------------------------------------------
  
  # Número de powers
  n_power <- sum(as.vector(table(power$Response)))
  
  #----------------------------------------------------------------
  
  # Número de respostas
  n_resp <- length(as.vector(table(power$Response)))
  
  #----------------------------------------------------------------
  
  # Nomes dos parâmetros
  power_names <- as.vector(unique(power$Parameters))
  
  #----------------------------------------------------------------
  
  # vcov desconsiderando parametros de regressao e potencia
  vcov_power <- vcov(object)[power_names, power_names]
  
  #----------------------------------------------------------------
  
  # Índice que associa power a resposta
  p_var <- 0:((length(power_names)/n_resp)-1)
  
  #----------------------------------------------------------------
  
  # Matriz F para todos os parâmetros (Hypothesis matrix)
  F_all <- diag(length(p_var))
  
  #----------------------------------------------------------------
  
  # Matriz F por variável (Hypothesis matrix)
  F_par <- by(data = F_all, 
              INDICES = p_var, 
              FUN = as.matrix) 
  
  #----------------------------------------------------------------
  
  # Matriz G
  G <- diag(n_resp)
  
  #----------------------------------------------------------------
  
  # Matriz L
  
  L_par <- list()
  
  for (i in 1:length(F_par)) {
    L_par[[i]] <- kronecker(G, F_par[[i]])
  }
  
  
  #----------------------------------------------------------------
  
  ## Tabela
  
  W <- vector() # Vetor para a estatística de teste
  gl <- vector() # Vetor para graus de liberdade
  p_val <- vector() # Vetor para p-valor
  
  ### Estatística de teste:
  #### t(L*beta) x (L*vcov*t(L))^-1 x (L*beta) ~ Qui-quadrado(numero de parametros testados)
  
  for (i in 1:length(L_par)) {
    
    W[i] <- as.numeric((t(L_par[[i]]%*%power$Estimates)) %*% (solve(L_par[[i]]%*%vcov_power%*%t(L_par[[i]]))) %*% (L_par[[i]]%*%power$Estimates))
    gl[i] <- nrow(L_par[[i]])
    p_val[i] <- pchisq(W[i], df = gl[i], lower.tail = FALSE)
  }
  
  tabela <- data.frame(Variável = 'Power',
                       GL = gl,
                       W = round(W, 3),
                       P_valor = round(p_val, 3))
  
  #----------------------------------------------------------------
  
  return(tabela)
}

mc_manova_power(fit_jointP)
coef(fit_jointP, type = 'power')
