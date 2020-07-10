#################################################################
# FUNÇÕES ANOVA E MANOVA VIA TESTE WALD
#################################################################

# ANOVA I    - X
# ANOVA II   - OK
# ANOVA III  - OK

# MANOVA I   - X
# MANOVA II  - OK
# MANOVA III - OK


#################################################################
# ANOVA VIA TESTE WALD TIPO III
#################################################################

mc_anova_III <- function(object){
  
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
  
  for (j in 1:n_resp) {
    
    W <- vector() # Vetor para a estatística de teste
    gl <- vector() # Vetor para graus de liberdade
    p_val <- vector() # Vetor para p-valor
    
    
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

#----------------------------------------------------------------

#################################################################
# ANOVA VIA TESTE WALD TIPO II
#################################################################

mc_anova_II <- function(object){
  
  #----------------------------------------------------------------
  
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
  expand <- list()
  
  for (i in 1:length(L_all)) {
    expand[[i]] <- by(data = L_all[[i]],
                      INDICES = p_var[[i]],
                      FUN = as.matrix)  
  }
  
  
  beta_names <- list()
  
  for (i in 1:length(L_all)) {
    beta_names[[i]] <- object$beta_names[[i]]  
  }
  
  
  testes <- list()
  
  for (i in 1:length(L_all)) {
    testes[[i]] <- data.frame(beta_names = beta_names[[i]],
                              interacao = stringr::str_detect(beta_names[[i]], ':'))  
  }
  
  
  for (i in 1:length(L_all)) {
    for (j in 1:(length(expand[[i]]))) {
      testes[[i]][,j+2] <- colSums(expand[[i]][[j]])
    }  
  }
  
  aux <- list()
  length(aux) <- n_resp
  
  for (k in 1:length(L_all)) {
    for (i in 3:ncol(testes[[k]])) {
      padrao <- as.vector(subset(testes[[k]], interacao == FALSE & testes[[k]][,i] == 1)$beta_names)
      
      x <- matrix(nrow = nrow(testes[[k]]), ncol = length(padrao))
      
      for (j in 1:nrow(testes[[k]])) {
        x[j,] <- sjmisc::str_contains(testes[[k]]$beta_names[j],
                                     pattern = padrao)
      }
      
      aux[[k]][[i]] <- list()
      
      aux[[k]][[i]] <- ifelse(rowSums(x) == 1, 1, testes[[k]][,i])
      
      as.vector(aux[[k]][[i]])
    }  
  }
  
  aux2 <- list()
  
  for (i in 1:length(aux)) {
    aux2[[i]] <- as.data.frame(do.call(cbind, aux[[i]]))  
  }
  
  
  p_varII <- aux2
  
  #----------------------------------------------------------------
  
  # Matriz L por variável (Hypothesis matrix), por resposta
  
  L_par <- list()
  length(L_par) <- n_resp
  
  
  for (j in 1:length(p_varII)) {
    for (i in 1:ncol(p_varII[[j]])) {
      L_par[[j]][[i]] <- by(data = L_all[[j]],
                            INDICES = p_varII[[j]][,i], 
                            FUN = as.matrix)$`1`
    }  
  }
  
  #----------------------------------------------------------------
  
  ## Tabela
  
  tabela <- list()
  
  for (j in 1:n_resp) {
    
    W <- vector() # Vetor para a estatística de teste
    gl <- vector() # Vetor para graus de liberdade
    p_val <- vector() # Vetor para p-valor
    
    for (i in 1:length(L_par[[j]])) {
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

#----------------------------------------------------------------

#################################################################
# MANOVA VIA TESTE WALD TIPO III
#################################################################

mc_manova_III <- function(object){
  
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

#----------------------------------------------------------------

#################################################################
# MANOVA VIA TESTE WALD TIPO II
#################################################################

mc_manova_II <- function(object){
  
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
  expand <- by(data = F_all,
               INDICES = p_var,
               FUN = as.matrix)
  
  beta_names <- object$beta_names[[1]]
  
  testes <- data.frame(beta_names,
                       interacao = stringr::str_detect(beta_names, ':'))
  
  for (i in 1:(length(expand))) {
    testes[,i+2] <- colSums(expand[[i]])
  }
  
  aux <- list()
  
  for (i in 3:ncol(testes)) {
    padrao <- as.vector(subset(testes, interacao == FALSE & testes[,i] == 1)$beta_names)
    
    x<-matrix(nrow = nrow(testes), ncol = length(padrao))
    
    for (j in 1:nrow(testes)) {
      x[j,] <- sjmisc::str_contains(testes$beta_names[j],
                                   pattern = padrao)
    }
    
    
    aux[[i]] <- ifelse(rowSums(x) == 1, 1, testes[,i])
    
    as.vector(aux[[i]])
  }
  
  aux2 <- as.data.frame(do.call(cbind, aux))
  
  p_varII <- aux2
  
  #cbind(beta_names,p_varII)
  
  F_par <- list()
  
  for (i in 1:ncol(p_varII)) {
    F_par[[i]] <- by(data = F_all,
                     INDICES = p_varII[,i], 
                     FUN = as.matrix)$`1`
  }
  
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

#----------------------------------------------------------------

#################################################################

mc_anova_II()
mc_anova_III()

mc_manova_II()
mc_manova_III()

#################################################################