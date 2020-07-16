
#################################################################
# ANOVA VIA TESTE WALD TIPO III PARA PARAMETROS DE DISPERSAO
#################################################################

mc_anova_disp <- function(object){
  
  # Vetor tau e indice de resposta
  tau <- coef(object, type = "tau")[,c(1,2, 4)] 
  
  #----------------------------------------------------------------
  
  # Número de taus por resposta
  n_tau <- as.vector(table(tau$Response)) 
  
  #----------------------------------------------------------------
  
  # Número de respostas
  n_resp <- length(n_tau) 
  
  #----------------------------------------------------------------
  
  # Lista vcov por resposta desconsiderando parametros de 
  # regressao e potencia
  
  vcov_taus <- list()
  
  padrao <- vector()
  
  for (j in 1:n_resp) {
    for (i in 1:length(row.names(vcov(object)))) {
      padrao[i] <- sjmisc::str_contains(rownames(vcov(object))[i],
                                        pattern = paste0('tau',j))
    }
    
    names <- data.frame(row_names = row.names(vcov(object)),
                        id = padrao)
    
    names2 <- as.vector(subset(names, id == TRUE)$row_names)
    
    vcov_taus[[j]] <- vcov(object)[names2, names2]  
  }
  
  #----------------------------------------------------------------
  
  # Índice que associa tau a matriz Z
  
  p_var <- list()
  
  for (i in 1:n_resp) {
    p_var[[i]] <- 0:(n_tau[i]-1)
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
      W[i] <- as.numeric((t(L_par[[j]][[i]] %*% subset(tau, tau$Response == j)$Estimates)) %*% (solve(L_par[[j]][[i]]%*%vcov_taus[[j]]%*%t(L_par[[j]][[i]]))) %*% (L_par[[j]][[i]] %*% subset(tau, tau$Response == j)$Estimates))
      gl[i] <- ifelse(is.null(nrow(L_par[[j]][[i]])) == TRUE,
                      1,nrow(L_par[[j]][[i]]))
      p_val[i] <- pchisq(W[i], df = gl[i], lower.tail = FALSE)
      
    } 
    tabela[[j]] <- 
      data.frame(Variável = paste0('tau',1:(n_tau[i])),
                 GL = gl,
                 W = round(W, 3),
                 P_valor = round(p_val, 3))
  }  
  
  return(tabela)  
}
