
#################################################################
# FUNÇÕES PARA TESTES DE HIPÓTESE GERAIS, ANOVAS, MANOVAS E 
# TESTES DE COMPARAÇÕES MÚLTIPLAS VIA TESTE WALD PARA OBJETOS 
# MCGLM
#################################################################

# ANOVA I               - OK
# ANOVA II              - OK
# ANOVA III             - OK
# ANOVA PARA DISPERSAO  - OK

# MANOVA I              - OK
# MANOVA II             - OK
# MANOVA III            - OK
# MANOVA PARA DISPERSAO - OK

# HIPÓTESES GERAIS      - OK

# MULTCOMP POR RESPOSTA - OK
# MULTCOMP MULTIVARIADO - OK

#################################################################

# mc_anova_I()
# mc_anova_II()
# mc_anova_III()
# mc_anova_disp()

# mc_manova_I()
# mc_manova_II()
# mc_manova_III()
# mc_manova_disp()

# mc_linear_hypothesis

# mc_multcomp
# mc_mult_multcomp

#################################################################
# ANOVA VIA TESTE WALD TIPO III
#################################################################

#' @title ANOVA type III table for mcglm objects via Wald test
#' @name mc_anova_III
#' @author Lineu Alberto Cavazani de Freitas, \email{lialcafre@@gmail.com}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION! BE CAREFUL!
#' Performs Wald tests to generate type-III analysis-of-variance 
#' tables per response for model objects produced by mcglm
#'
#' @param object an object of \code{mcglm} class.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#' @keywords internal
#' @return Type III ANOVA table for mcglm objects.
#'
#' @export

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
  
  #vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
  
  #for (i in 2:n_resp) {
  #  vcov_betas[[i]] <- 
  #    vcov(object)[(cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i]), 
  #                 (cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i])] 
  #  }
  
  if (n_resp == 1) {
    vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
  } else {
    vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
    for (i in 2:n_resp) {
      vcov_betas[[i]] <- 
        vcov(object)[(cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i]), 
                     (cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i])] 
      
    }
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
                 W = round(W, 4),
                 P_valor = round(p_val, 4))
  }  
  
  return(tabela)  
}

#----------------------------------------------------------------

#################################################################
# ANOVA VIA TESTE WALD TIPO II
#################################################################

#' @title ANOVA type II table for mcglm objects via Wald test
#' @name mc_anova_II
#' @author Lineu Alberto Cavazani de Freitas, \email{lialcafre@@gmail.com}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION! BE CAREFUL!
#' Performs Wald tests to generate type-II analysis-of-variance 
#' tables per response for model objects produced by mcglm
#'
#' @param object an object of \code{mcglm} class.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#' @keywords internal
#' @return Type II ANOVA table for mcglm objects.
#'
#' @export

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
  
  #vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
  
  #for (i in 2:n_resp) {
  #  vcov_betas[[i]] <- 
  #    vcov(object)[(cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i]), 
  #                 (cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i])] 
  #}
  
  if (n_resp == 1) {
    vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
  } else {
    vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
    for (i in 2:n_resp) {
      vcov_betas[[i]] <- 
        vcov(object)[(cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i]), 
                     (cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i])] 
      
    }
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
  
  # Índice que associa beta a variável por resposta para realização 
  # do teste tipo II
  
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
  
  p_varII <- list()
  
  for (i in 1:length(aux)) {
    p_varII[[i]] <- as.data.frame(do.call(cbind, aux[[i]]))  
  }
  
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
                 W = round(W, 4),
                 P_valor = round(p_val, 4))
  }
  
  return(tabela)  
}

#----------------------------------------------------------------

#################################################################
# ANOVA VIA TESTE WALD TIPO I
#################################################################

#' @title ANOVA pseudo-type I table for mcglm objects via Wald test
#' @name mc_anova_I
#' @author Lineu Alberto Cavazani de Freitas, \email{lialcafre@@gmail.com}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION! BE CAREFUL!
#' Performs Wald tests to generate pseudo-type-I analysis-of-variance 
#' tables per response for model objects produced by mcglm
#'
#' @param object an object of \code{mcglm} class.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#' @keywords internal
#' @return Type I ANOVA table for mcglm objects.
#'
#' @export

mc_anova_I <- function(object){
  
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
  
  #vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
  
  #for (i in 2:n_resp) {
  #  vcov_betas[[i]] <- 
  #    vcov(object)[(cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i]), 
  #                 (cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i])] 
  #}
  
  if (n_resp == 1) {
    vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
  } else {
    vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
    for (i in 2:n_resp) {
      vcov_betas[[i]] <- 
        vcov(object)[(cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i]), 
                     (cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i])] 
      
    }
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
  
  # Índice que associa beta a variável por resposta  para 
  # teste sequencial
  
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
  
  p_varII <- list()
  
  for (i in 1:n_resp) {
    p_varII[[i]] <- matrix(nrow = nrow(testes[[i]]),
                           ncol = ncol(testes[[i]])-2)  
  }
  
  
  for (j in 1:n_resp) {
    for (i in 3:(ncol(testes[[j]])-1)) {
      p_varII[[j]][,i-2] <- rowSums(testes[[j]][,i:ncol(testes[[j]])])
    }
    
    p_varII[[j]][,ncol(p_varII[[j]])] <- testes[[j]][,ncol(testes[[j]])]  
  }
  
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
                 W = round(W, 4),
                 P_valor = round(p_val, 4))
  }
  
  return(tabela)  
}

#----------------------------------------------------------------

#################################################################
# MANOVA VIA TESTE WALD TIPO III
#################################################################

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
                       W = round(W, 4),
                       P_valor = round(p_val, 4))
  
  #----------------------------------------------------------------
  
  return(tabela)
}

#----------------------------------------------------------------

#################################################################
# MANOVA VIA TESTE WALD TIPO II
#################################################################

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

#----------------------------------------------------------------

#################################################################
# MANOVA VIA TESTE WALD TIPO I
#################################################################

#' @title MANOVA pseudo-type I table for mcglm objects via Wald test
#' @name mc_manova_I
#' @author Lineu Alberto Cavazani de Freitas, \email{lialcafre@@gmail.com}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION! BE CAREFUL!
#' Performs Wald tests to generate pseudo-type-I multivariate 
#' analysis-of-variance tables for model objects 
#' produced by mcglm
#'
#' @param object an object of \code{mcglm} class.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#' @keywords internal
#' @return Type I MANOVA table for mcglm objects.
#'
#' @export

mc_manova_I <- function(object){
  
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
  
  aux <- matrix(nrow = nrow(testes),
                ncol = ncol(testes)-2)
  
  
  for (i in 3:(ncol(testes)-1)) {
    aux[,i-2] <- rowSums(testes[,i:ncol(testes)])
  }
  
  aux[,ncol(aux)] <- testes[,ncol(testes)]  
  
  p_varII <- aux
  
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
                       W = round(W, 4),
                       P_valor = round(p_val, 4))
  
  #----------------------------------------------------------------
  
  return(tabela)
}

#----------------------------------------------------------------

#################################################################
# ANOVA VIA TESTE WALD TIPO III PARA PARAMETROS DE DISPERSAO
#################################################################

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
  
  # Índice que associa tau a matriz Z
  
  #p_var <- list()
  
  #for (i in 1:n_resp) {
  #  p_var[[i]] <- 0:(n_tau[i]-1)
  #}  
  
  p_var <- p_var
  
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
      data.frame(Variável = names[[j]],
                 GL = gl,
                 W = round(W, 4),
                 P_valor = round(p_val, 4))
  }  
  
  return(tabela)  
}

#----------------------------------------------------------------

#################################################################
# MANOVA VIA TESTE WALD TIPO III PARA PARAMETROS DE DISPERSAO
#################################################################

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

#----------------------------------------------------------------

#################################################################
# HIPÓTESES LINEARES GERAIS: PARAMETRO VS VALOR POSTULADO
#################################################################

# mc_linear_hypothesis <- function(object, hypothesis){
#   
#   # Vetor beta chapeu
#   coefs <- coef(object, type = c("beta", "tau", "power"))
#   
#   #----------------------------------------------------------------
#   
#   # Número de parametros
#   n_coefs <- sum(as.vector(table(coefs$Response)))
#   
#   #----------------------------------------------------------------
#   
#   # Número de respostas
#   n_resp <- length(as.vector(table(coefs$Response)))
#   
#   #----------------------------------------------------------------
#   
#   # vcov
#   vcov_coefs <- vcov(object)[as.vector(coefs$Parameters),
#                              as.vector(coefs$Parameters)]
#   
#   #----------------------------------------------------------------
#   
#   # Matriz L para todos os parâmetros (Hypothesis matrix)
#   L_all <- diag(n_coefs)
#   
#   #----------------------------------------------------------------
#   
#   colnames(L_all) <- as.vector(coefs$Parameters)
#   row.names(L_all) <- as.vector(coefs$Parameters)
#   
#   #----------------------------------------------------------------
#   
#   # Quebrando as strings recebidas como argumento (hipóteses)
#   hypothesis2 <- stringr::str_split(hypothesis, 
#                                     pattern = c('='), 
#                                     simplify = T)
#   
#   #----------------------------------------------------------------
#   
#   # Gerando um data frame
#   hypothesis3 <- as.data.frame(hypothesis2)
#   names(hypothesis3) <- c('parameters', 'null_hyp')
#   
#   #----------------------------------------------------------------
#   
#   # Retirando espaços
#   hypothesis3$parameters <- stringr::str_replace(hypothesis3$parameters, " ",  "")
#   hypothesis3$null_hyp <- stringr::str_replace(hypothesis3$null_hyp, " ",  "")
#   
#   #----------------------------------------------------------------
#   
#   # Selecionando linhas da matriz L
#   L_user <- subset(L_all, rownames(L_all) %in% hypothesis3$parameters)
#   
#   #----------------------------------------------------------------
#   
#   # Verificando se há hipóteses de igualdade e modificando a matriz
#   # para fazer o teste de igualdade
#   
#   for (i in 1:nrow(L_user)) {
#     L_user[i,] <- ifelse(colnames(L_user) == hypothesis3$null_hyp[i],
#                          -1,L_user[i,])  
#   }
#   
#   #----------------------------------------------------------------
#   
#   # Substitui strings por 0 (hipóteses de igualdade)
#   hypothesis3$null_hyp <- ifelse(stringr::str_detect(hypothesis3$null_hyp, c('beta')) == T,0,hypothesis3$null_hyp)
#   hypothesis3$null_hyp <- ifelse(stringr::str_detect(hypothesis3$null_hyp, c('tau')) == T,0,hypothesis3$null_hyp)
#   hypothesis3$null_hyp <- ifelse(stringr::str_detect(hypothesis3$null_hyp, c('power')) == T,0,hypothesis3$null_hyp)
#   
#   #----------------------------------------------------------------
#   
#   # Converte valores da hipótese nula para numericas
#   hypothesis3$null_hyp <- as.numeric(hypothesis3$null_hyp)
#   
#   #----------------------------------------------------------------
#   
#   # Efetua o teste
#   
#   W <- vector() # Vetor para a estatística de teste
#   gl <- vector() # Vetor para graus de liberdade
#   p_val <- vector() # Vetor para p-valor
#   
#   W <- as.numeric((t((L_user%*%coefs$Estimates) - hypothesis3$null_hyp)) %*% (solve(L_user%*%vcov_coefs%*%t(L_user))) %*% ((L_user%*%coefs$Estimates) - hypothesis3$null_hyp))
#   gl <- nrow(L_user)
#   p_val <- pchisq(W, df = gl, lower.tail = FALSE)
#   
#   tabela <- data.frame(GL = gl,
#                        W = round(W, 4),
#                        P_valor = round(p_val, 4))
#   
#   return(tabela)
#   
# }

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
  
  tabela <- data.frame(GL = gl,
                       W = round(W, 4),
                       P_valor = round(p_val, 4))
  
  return(tabela)
  
}

#----------------------------------------------------------------

#################################################################
# TESTE DE COMPARAÇÕES MÚLTIPLAS PARA TODAS AS RESPOSTAS
#################################################################

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

#----------------------------------------------------------------

#################################################################
# TESTE DE COMPARAÇÕES MÚLTIPLAS PARA CADA RESPOSTA
#################################################################

#' @title Multiple comparisons test for each response
#' @name mc_multcomp
#' @author Lineu Alberto Cavazani de Freitas, \email{lialcafre@@gmail.com}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION! BE CAREFUL!
#' Performs a multiple comparisons test to compare diferences between 
#' treatment levels for each response for model objects produced by 
#' mcglm
#'
#' @param object an object of \code{mcglm} class.
#' @param effect A list of vector of variables. For each configuration of 
#' these the estimate will be calculated.
#' @param data dataframe.
#' @keywords internal
#' @return Table of multiple comparisons.
#'
#' @export

mc_multcomp <- function(object, effect, data){
  
  # Obter a matriz de combinações lineares dos parâmetros dos 
  # modelos que resultam nas médias ajustadas (geralmente denotada por L)
  #-------------------------------------------------------------------
  
  m_glm <- list()
  
  for (i in 1:length(object$linear_pred)) {
    m_glm[[i]] <- glm(formula = object$linear_pred[[i]], data = data)
  }
  
  mm <- list()
  
  for (i in 1:length(m_glm)) {
    mm[[i]] <- doBy::LE_matrix(m_glm[[i]], effect = effect[[i]])
  }
  
  #------------------------------------------------------------------- 
  
  # Para testar os contrastes de uma média ajustada contra a outra deve-se 
  # subtrair as linhas da primeira matriz duas a duas (geralmente denotada 
  # por K)
  
  K1 <- list()  
  
  for (i in 1:length(mm)) {
    K1[[i]] <- apc(mm[[i]])
  }
  
  K2 <- list()
  
  for (i in 1:length(mm)) {
    K2[[i]] <- by(K1[[i]], INDICES = row.names(K1[[i]]), FUN = as.matrix)
  }
  
  #-------------------------------------------------------------------
  
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
  
  #vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
  
  #for (i in 2:n_resp) {
  #  vcov_betas[[i]] <- 
  #    vcov(object)[(cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i]), 
  #                 (cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i])] 
  #  }
  
  if (n_resp == 1) {
    vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
  } else {
    vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
    for (i in 2:n_resp) {
      vcov_betas[[i]] <- 
        vcov(object)[(cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i]), 
                     (cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i])] 
      
    }
  }
  
  #----------------------------------------------------------------
  
  ## Tabela
  
  tabela <- list()
  
  for (j in 1:n_resp) {
    
    W <- vector() # Vetor para a estatística de teste
    gl <- vector() # Vetor para graus de liberdade
    p_val <- vector() # Vetor para p-valor
    
    
    for (i in 1:dim(K2[[j]])) {
      W[i] <- as.numeric((t(K2[[j]][[i]] %*% subset(beta, beta$Response == j)$Estimates)) %*% (solve(K2[[j]][[i]]%*%vcov_betas[[j]]%*%t(K2[[j]][[i]]))) %*% (K2[[j]][[i]] %*% subset(beta, beta$Response == j)$Estimates))
      gl[i] <- nrow(K2[[j]][[i]])
      p_val[i] <- pchisq(W[i], df = gl[i], lower.tail = FALSE)
      
    } 
    tabela[[j]] <- 
      data.frame(Contrast = names(K2[[j]]),
                 GL = gl,
                 W = round(W, 4),
                 P_valor = round(p.adjust(p_val, method = 'bonferroni'), 4))
  }  
  
  return(tabela)  
}

#----------------------------------------------------------------

#################################################################
# FUNÇÃO GERADORA DE CONTRASTES DE TUKEY (WALMES)
#################################################################

#' @name apc
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @export
#' @title Generate Matrix of All Pairwise Comparisons (Tukey contrasts)
#' @description This function takes a matrix where each line defines a
#'     linear function of the parameters to estimate a marginal mean
#'     (aka least squares mean) and return the matrix that define the
#'     contrasts among these means. All pairwise contrasts are returned
#'     (aka Tukey contrasts). The matrix with these contrasts can be
#'     passed to \code{\link[multcomp]{glht}()} to estimate them or used
#'     in explicit matricial calculus.
#' @param lfm a \eqn{k \times p} matrix where each line defines a linear
#'     function to estimate a lsmean. In general, these matrices are
#'     obtained by using \code{\link[doBy]{LSmatrix}()}.
#' @param lev a character vector with length equals to the numbers of
#'     lines of \code{lfm} matrix, (\eqn{k}). Default is \code{NULL} and
#'     the row names of code{lfm} is used. If row names is also
#'     \code{NULL}, incremental integer values are used to identify the
#'     comparisons.
#' @return a \eqn{K\times p} matrix with the linear functions that
#'     define all pairwise contrasts. \eqn{K} is \eqn{{k}\choose{2}}.
#' @seealso \code{\link{apmc}()}, \code{\link[doBy]{LSmatrix}()}.
#' @examples
#'
#' X <- diag(3)
#' rownames(X)
#' apc(X)
#'
#' rownames(X) <- letters[nrow(X):1]
#' apc(X)
#'
#' apc(X, lev = LETTERS[1:nrow(X)])
#'
#' # Objects from doBy::LSmatrix() have an "grid" attribute.
#' attr(X, "grid") <- data.frame(n = LETTERS[1:nrow(X)])
#' rownames(X) <- NULL
#' apc(X)
#'
apc <- function(lfm, lev = NULL) {
  nlev <- nrow(lfm)
  rn <- rownames(lfm)
  a <- attr(lfm, "grid")
  if (is.null(lev)) {
    if (!is.null(a)) {
      lev <- apply(a, 1, paste, collapse = ":")
    } else if (!is.null(rn)) {
      lev <- rn
    } else {
      lev <- as.character(1:nlev)
    }
  }
  cbn <- utils::combn(seq_along(lev), 2)
  M <- lfm[cbn[1, ], ] - lfm[cbn[2, ], ]
  if (is.vector(M)) {
    dim(M) <- c(1, length(M))
  }
  rownames(M) <- paste(lev[cbn[1, ]], lev[cbn[2, ]], sep = "-")
  return(M)
}

#----------------------------------------------------------------