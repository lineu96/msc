#' @title MANOVA type II table for mcglm objects via Wald test
#' @name mc_manova_II
#' @author Lineu Alberto Cavazani de Freitas, \email{lialcafre@@gmail.com}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION! BE CAREFUL!
#' Performs Wald tests to generate type-II multivariate 
#' analysis-of-variance tables for model objects 
#' produced by mcglm
#'
#' @param object an object of \code{mcglm} class.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#' @keywords internal
#' @return Type II MANOVA table for mcglm objects.
#'
#' @export

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
  
  p_varII <- as.data.frame(do.call(cbind, aux))
  
  
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
                       W = round(W, 4),
                       P_valor = round(p_val, 4))
  
  #----------------------------------------------------------------
  
  return(tabela)
}