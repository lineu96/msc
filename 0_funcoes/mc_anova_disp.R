#' @title Anova Tables for dispersion components
#' @name mc_anova_disp
#' @author Lineu Alberto Cavazani de Freitas, \email{lialcafre@@gmail.com}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION! BE CAREFUL!
#' Performs Wald tests to generate analysis-of-variance tables 
#' of the significance for the dispersion components by response 
#' variables for model objects produced by mcglm.
#'
#' @param object an object of \code{mcglm} class.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#' @keywords internal
#' @return ANOVA table for dispersion components of mcglm objects.
#'
#' @export

mc_anova_disp <- function(object, p_var, names){
  
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
    
    names2 <- data.frame(row_names = row.names(vcov(object)),
                         id = padrao)
    
    names2 <- as.vector(subset(names2, id == TRUE)$row_names)
    
    vcov_taus[[j]] <- vcov(object)[names2, names2]  
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
      data.frame(Dispersion = names[[j]],
                 Df = gl,
                 Chi = round(W, 4),
                 'Pr(>Chi)' = round(p_val, 4),
                 check.names = F)
  }  
  
  #----------------------------------------------------------------
  
  cat("ANOVA type III using Wald statistic for dispersion parameters\n\n")
  for (i in 1:n_resp) {
    cat("Call: ")
    print(object$linear_pred[[i]])
    cat("\n")
    print(tabela[[i]])
    cat("\n")
  }
  
  return(invisible(tabela)) 
}
