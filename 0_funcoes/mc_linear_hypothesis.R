#' @title Test Linear Hypothesis
#' @name mc_linear_hypothesis
#' @author Lineu Alberto Cavazani de Freitas, \email{lialcafre@@gmail.com}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION! BE CAREFUL!
#' Performs Wald tests for testing a linear hypothesis for 
#' model objects produced by mcglm.
#'
#' @param object an object of \code{mcglm} class.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#' @keywords internal
#' @return MANOVA table for dispersion components of mcglm objects.
#' @export

mc_linear_hypothesis <- function(object, hypothesis){
  
  # Vetor beta chapeu
  coefs <- coef(object, type = c("beta", "tau", "power"))
  
  #----------------------------------------------------------------
  
  # Número de parametros
  n_coefs <- sum(as.vector(table(coefs$Response)))
  
  #----------------------------------------------------------------
  
  # Número de respostas
  n_resp <- length(as.vector(table(coefs$Response)))
  
  #----------------------------------------------------------------
  
  # vcov
  vcov_coefs <- vcov(object)[as.vector(coefs$Parameters),
                             as.vector(coefs$Parameters)]
  
  #----------------------------------------------------------------
  
  # Quebrando as strings recebidas como argumento (hipóteses)
  hypothesis2 <- stringr::str_split(hypothesis, 
                                    pattern = c('='), 
                                    simplify = T)
  
  #----------------------------------------------------------------
  
  # Gerando um data frame
  hypothesis3 <- as.data.frame(hypothesis2)
  names(hypothesis3) <- c('parameters', 'null_hyp')
  
  #----------------------------------------------------------------
  
  # Retirando espaços
  hypothesis3$parameters <- stringr::str_replace(hypothesis3$parameters, " ",  "")
  hypothesis3$null_hyp <- stringr::str_replace(hypothesis3$null_hyp, " ",  "")
  
  #----------------------------------------------------------------
  
  if(sum(hypothesis3$parameters %in% coefs$Parameters) != 
     length(hypothesis3$parameters)) stop("You specified hypothesis about parameters that does not exist in the model.")
  
  #----------------------------------------------------------------
  
  # Gerando a matriz L
  L_user <- matrix(nrow = nrow(hypothesis3), ncol = n_coefs)
  
  colnames(L_user) <- as.vector(coefs$Parameters)
  
  for (i in 1:nrow(L_user)) {
    L_user[i,] <- ifelse(colnames(L_user) == hypothesis3$parameters[i],
                         1,0)  
  }
  
  for (i in 1:nrow(L_user)) {
    L_user[i,] <- ifelse(colnames(L_user) == hypothesis3$null_hyp[i],
                         -1,L_user[i,])  
  }
  
  #----------------------------------------------------------------
  
  # Substitui strings por 0 (hipóteses de igualdade)
  hypothesis3$null_hyp <- ifelse(stringr::str_detect(hypothesis3$null_hyp, c('beta')) == T,0,hypothesis3$null_hyp)
  hypothesis3$null_hyp <- ifelse(stringr::str_detect(hypothesis3$null_hyp, c('tau')) == T,0,hypothesis3$null_hyp)
  hypothesis3$null_hyp <- ifelse(stringr::str_detect(hypothesis3$null_hyp, c('power')) == T,0,hypothesis3$null_hyp)
  
  #----------------------------------------------------------------
  
  # Converte valores da hipótese nula para numericas
  hypothesis3$null_hyp <- as.numeric(hypothesis3$null_hyp)
  
  #----------------------------------------------------------------
  
  # Efetua o teste
  
  W <- vector() # Vetor para a estatística de teste
  gl <- vector() # Vetor para graus de liberdade
  p_val <- vector() # Vetor para p-valor
  
  W <- as.numeric((t((L_user%*%coefs$Estimates) - hypothesis3$null_hyp)) %*% (solve(L_user%*%vcov_coefs%*%t(L_user))) %*% ((L_user%*%coefs$Estimates) - hypothesis3$null_hyp))
  gl <- nrow(L_user)
  p_val <- pchisq(W, df = gl, lower.tail = FALSE)
  
  tabela <- data.frame(Df = gl,
                       Chi = round(W, 4),
                       'Pr(>Chi)' = round(p_val, 4),
                       check.names = F)
  
  #----------------------------------------------------------------
  
  cat("Linear hypothesis test")
  cat("\n\n")
  cat("Hypothesis:")
  
  
  print_hyp <- as.data.frame(hypothesis)
  names(print_hyp) <- NULL
  print(print_hyp)
  
  cat("\n")
  
  cat("Results:\n")
  
  print(tabela)
  cat("\n")
  
  return(invisible(tabela))
  
}
