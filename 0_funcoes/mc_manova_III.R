#' @title MANOVA type III table for mcglm objects via Wald test
#' @name mc_manova_III
#' @author Lineu Alberto Cavazani de Freitas, \email{lialcafre@@gmail.com}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION! BE CAREFUL!
#' Performs Wald tests to generate type-III multivariate 
#' analysis-of-variance tables for model objects 
#' produced by mcglm
#'
#' @param object an object of \code{mcglm} class.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#' @keywords internal
#' @return Type III MANOVA table for mcglm objects.
#'
#' @export

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
  
  # ERROS E AVISOS
  
  preds <- c()
  
  for (i in 1:n_resp) {
    preds[i] <- sub(".*~", "",gsub(" ", "", as.character(object$linear_pred)[i]))
  }
  
  if(length(unique(preds)) != 1) stop("For MANOVA functions, the predictors must be the same for all outcomes.")
  
  if(n_resp == 1) warning("You are applying a MANOVA function to a univariate problem.")  
  
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
  
  tabela <- data.frame(Covariate = c("Intercept", 
                                     attr(terms(object$linear_pred[[1]]), "term.labels")),
                       Df = gl,
                       Chi = round(W, 4),
                       'Pr(>Chi)' = round(p_val, 4),
                       check.names = F)
  
  #----------------------------------------------------------------
  
  cat("MANOVA type III using Wald statistic for fixed effects\n\n")
  cat("Call: ")
  cat(paste0('~ ', preds[[1]]))
  cat("\n")
  print(tabela)
  
  return(invisible(tabela))
  
}
