#################################################################
# FUNÇÕES ANOVA E MANOVA VIA TESTE WALD
#################################################################

#################################################################
# ANOVA VIA TESTE WALD
mc_anova <- function(object){
  
  # Vetor beta chapeu e indice de resposta
  beta <- coef(object, type = "beta")[,c(1, 4)] 
  
  #----------------------------------------------------------------
  
  # Número de betas por resposta
  n_beta <- as.vector(table(beta$Response)) 
  
  #----------------------------------------------------------------
  
  # Número de respostas
  n_resp <- length(n_beta) 
  
  #----------------------------------------------------------------
  
  # Lista vcov por resposta desconsiderando parametros de dispersao e potencia
  
  vcov_betas <- list()
  
  vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
  
  for (i in 2:n_resp) {
    vcov_betas[[i]] <- 
      vcov(object)[(cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i]), 
                       (cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i])] 
    
  }
  
  #----------------------------------------------------------------
  
  # Índice que associa beta a variável por resposta
  
  p_var <- list()
  
  for (i in 1:n_resp) {
    p_var[[i]] <- attr(object$list_X[[i]], "assign")
  }
  
  #----------------------------------------------------------------
  
  # Matriz L para todos os parâmetros (Hypothesis matrix), por resposta
  L_all <- list()
  
  for (i in 1:n_resp) {
    L_all[[i]] <- diag(length(p_var[[i]]))   
  }  
  
  #----------------------------------------------------------------
  
  # Matriz L por variável (Hypothesis matrix), por resposta
  
  L_par <- list()
  
  for (i in 1:n_resp) {
    L_par[[i]] <- by(data = L_all[[i]], 
                     INDICES = p_var[[i]], 
                     FUN = as.matrix)   
  }
  
  #----------------------------------------------------------------
  
  ## Tabela
  
  tabela <- list()
  W <- vector() # Vetor para a estatística de teste
  gl <- vector() # Vetor para graus de liberdade
  p_val <- vector() # Vetor para p-valor
  
  for (j in 1:n_resp) {
    for (i in 1:dim(L_par[[j]])) {
      W[i] <- as.numeric((t(L_par[[j]][[i]] %*% subset(beta, beta$Response == j)$Estimates)) %*% (solve(L_par[[j]][[i]]%*%vcov_betas[[j]]%*%t(L_par[[j]][[i]]))) %*% (L_par[[j]][[i]] %*% subset(beta, beta$Response == j)$Estimates))
      gl[i] <- nrow(L_par[[j]][[i]])
      p_val[i] <- pchisq(W[i], df = gl[i], lower.tail = FALSE)
      
    } 
    tabela[[j]] <- 
      data.frame(Variável = c("Intercept", 
                              attr(terms(object$linear_pred[[j]]), "term.labels")),
                 GL = gl,
                 W = round(W, 3),
                 P_valor = round(p_val, 3))
  }  

  return(tabela)  
}
#################################################################

#################################################################
# MANOVA VIA TESTE WALD TIPO III
#################################################################

mc_manova <- function(object){
  
  #----------------------------------------------------------------
  
  # Vetor beta chapeu
  beta <- coef(object, type = "beta")[,c(1, 4)] 
  
  #----------------------------------------------------------------
  
  # Número de betas
  n_beta <- sum(as.vector(table(beta$Response)))
  
  #----------------------------------------------------------------
  
  # Número de respostas
  n_resp <- length(as.vector(table(beta$Response)))
  
  #----------------------------------------------------------------
  
  # vcov desconsiderando parametros de dispersao e potencia
  vcov_betas <- vcov(object)[1:n_beta, 1:n_beta]
  
  #----------------------------------------------------------------
  
  # Índice que associa beta a variável
  p_var <- attr(object$list_X[[1]], "assign")
  
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
    
    W[i] <- as.numeric((t(L_par[[i]]%*%beta$Estimates)) %*% (solve(L_par[[i]]%*%vcov_betas%*%t(L_par[[i]]))) %*% (L_par[[i]]%*%beta$Estimates))
    gl[i] <- nrow(L_par[[i]])
    p_val[i] <- pchisq(W[i], df = gl[i], lower.tail = FALSE)
  }
  
  tabela <- data.frame(Variável = c("Intercept", 
                                    attr(terms(object$linear_pred[[1]]), "term.labels")),
                       GL = gl,
                       W = round(W, 3),
                       P_valor = round(p_val, 3))
  
  #----------------------------------------------------------------
  
  return(tabela)
}

#################################################################
# 
#################################################################
