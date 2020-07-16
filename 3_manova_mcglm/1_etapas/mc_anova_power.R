
#################################################################
# ANOVA VIA TESTE WALD TIPO III PARA PARAMETROS DE POTENCIA
#################################################################

mc_anova_power <- function(object){
  
  # Vetor power e indice de resposta
  power <- coef(object, type = "power")[,c(1,2,4)] 
  
  #----------------------------------------------------------------
  
  # Número de powers por resposta
  n_power <- as.vector(table(power$Response)) 
  
  #----------------------------------------------------------------
  
  # Número de respostas
  n_resp <- length(n_power) 
  
  #----------------------------------------------------------------
  
  # Lista vcov por resposta desconsiderando parametros de 
  # regressao e dispersao
  
  vcov_power <- list()
  
  padrao <- vector()
  
  for (j in 1:n_resp) {
    for (i in 1:length(row.names(vcov(object)))) {
      padrao[i] <- sjmisc::str_contains(rownames(vcov(object))[i],
                                        pattern = paste0('power',j))
    }
    
    names <- data.frame(row_names = row.names(vcov(object)),
                        id = padrao)
    
    names2 <- as.vector(subset(names, id == TRUE)$row_names)
    
    vcov_power[[j]] <- vcov(object)[names2, names2]  
  }
  
  #----------------------------------------------------------------
  
  # Índice que associa tau a matriz Z
  
  p_var <- list()
  
  for (i in 1:n_resp) {
    p_var[[i]] <- 0:(n_power[i]-1)
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
      W[i] <- as.numeric((t(L_par[[j]][[i]] %*% subset(power, power$Response == j)$Estimates)) %*% (solve(L_par[[j]][[i]]%*%vcov_power[[j]]%*%t(L_par[[j]][[i]]))) %*% (L_par[[j]][[i]] %*% subset(power, power$Response == j)$Estimates))
      gl[i] <- ifelse(is.null(nrow(L_par[[j]][[i]])) == TRUE,
                      1,nrow(L_par[[j]][[i]]))
      p_val[i] <- pchisq(W[i], df = gl[i], lower.tail = FALSE)
      
    } 
    tabela[[j]] <- 
      data.frame(Variável = power_names[j],
                 GL = gl,
                 W = round(W, 3),
                 P_valor = round(p_val, 3))
  }  
  
  return(tabela)  
}


