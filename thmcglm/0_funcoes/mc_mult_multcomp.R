#' @title Multiple comparisons test for all responses
#' @name mc_mult_multcomp
#' @author Lineu Alberto Cavazani de Freitas, \email{lialcafre@@gmail.com}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION! BE CAREFUL!
#' Performs a multiple comparisons test to compare diferences between 
#' treatment levels over all the responses for model objects produced by 
#' mcglm
#'
#' @param object an object of \code{mcglm} class.
#' @param effect A vector of variables. For each configuration of these the estimate will be calculated.
#' @param data dataframe.
#' @keywords internal
#' @return Table of multiple comparisons.
#'
#' @export

mc_mult_multcomp <- function(object, effect, data){
  
  # Obter a matriz de combinações lineares dos parâmetros dos 
  # modelos que resultam nas médias ajustadas (geralmente denotada por L)
  
  m_glm <- glm(formula = object$linear_pred[[1]], data = data)
  mm <- doBy::LE_matrix(m_glm, effect = effect)
  
  #-------------------------------------------------------------------  
  
  # Para testar os contrastes de uma média ajustada contra a outra deve-se 
  # subtrair as linhas da primeira matriz duas a duas (geralmente denotada 
  # por K)
  
  K1 <- apc(mm)
  K2 <- by(K1, INDICES = row.names(K1), FUN = as.matrix)
  
  #-------------------------------------------------------------------
  
  # Aplicando K no teste Wald
  
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
  
  # Matriz G
  G <- diag(n_resp)
  
  #----------------------------------------------------------------
  
  # Matriz L
  
  L_par <- list()
  
  for (i in 1:length(K2)) {
    L_par[[i]] <- kronecker(G, K2[[i]])
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
  
  tabela <- data.frame(Contrast = names(K2),
                       GL = gl,
                       W = round(W, 4),
                       P_valor = round(p.adjust(p_val, method = 'bonferroni'), 4))
  
  return(tabela)
}