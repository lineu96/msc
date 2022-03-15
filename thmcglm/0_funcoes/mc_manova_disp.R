#' @title MANOVA table for dispersion components
#' @name mc_manova_disp
#' @author Lineu Alberto Cavazani de Freitas, \email{lialcafre@@gmail.com}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION! BE CAREFUL!
#' Performs Wald tests to generate multivariate analysis-of-variance
#' tables of the significance for the dispersion components for 
#' model objects produced by mcglm.
#'
#' @param object an object of \code{mcglm} class.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#' @keywords internal
#' @return MANOVA table for dispersion components of mcglm objects.
#' @export

mc_manova_disp <- function(object, p_var, names){
  
  # Vetor tau
  tau <- coef(object, type = "tau")[,c(1,2,4)]
  
  #----------------------------------------------------------------
  
  # Número de taus
  n_tau <- sum(as.vector(table(tau$Response)))
  
  #----------------------------------------------------------------
  
  # Número de respostas
  n_resp <- length(as.vector(table(tau$Response)))
  
  #----------------------------------------------------------------
  
  # Nomes dos parâmetros
  tau_names <- as.vector(unique(tau$Parameters))
  
  #----------------------------------------------------------------
  
  # vcov desconsiderando parametros de regressao e potencia
  vcov_taus <- vcov(object)[tau_names, tau_names]
  
  #----------------------------------------------------------------
  
  # Índice que associa tau a matriz Z
  p_var <- p_var
  
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
    
    W[i] <- as.numeric((t(L_par[[i]]%*%tau$Estimates)) %*% (solve(L_par[[i]]%*%vcov_taus%*%t(L_par[[i]]))) %*% (L_par[[i]]%*%tau$Estimates))
    gl[i] <- nrow(L_par[[i]])
    p_val[i] <- pchisq(W[i], df = gl[i], lower.tail = FALSE)
  }
  
  tabela <- data.frame(Variável = names,
                       GL = gl,
                       W = round(W, 4),
                       P_valor = round(p_val, 4))
  
  #----------------------------------------------------------------
  
  return(tabela)
}