####################################################################
# PSEUDO-ANÁLISE DE VARIÂNCIA UNIVARIADA PARA MÚLTIPLAS RESPOSTAS NO 
# MCGLM DO TIPO I
####################################################################

#----------------------------------------------------------------
library(mcglm)
library(Matrix)
#----------------------------------------------------------------

# LEITURA

dados <- read.csv2("dados_ovelhas.csv", 
                   header = T, 
                   sep = ";", 
                   dec = ',')

dados2 <- dados[,c(1,2,3,4,
                   14,15,17,21,
                   7,9,10,11)]

dados2$momento <- ordered(dados2$tempo, 
                          levels=c("Antes", "Durante", "Depois"))

#----------------------------------------------------------------

# PREDITOR

#form.ncorpo <- ncorpo ~ sessao + tempo + linhagem
#form.ncorpo2 <- ncorpo ~ sessao + tempo + linhagem
#form.ncorpo3 <- ncorpo ~ sessao + tempo + linhagem

form.ncorpo <- ncorpo ~ sessao
form.ncorpo2 <- ncorpo ~ sessao + tempo
form.ncorpo3 <- ncorpo ~ sessao + tempo + linhagem
form.ncorpo4 <- ncorpo ~ (sessao + tempo + linhagem)^2

#----------------------------------------------------------------

# MATRIX LINEAR PREDICTOR

Z0 <- mc_id(dados2) # Identidade

Z1 <- mc_mixed(~0 + factor(animal), data = dados2) # Animal

dados2$comb <- paste(dados2$animal, dados2$sessao)
Z2 <- mc_mixed(~0 + comb, data = dados2) # Animal Momento

#----------------------------------------------------------------

# AJUSTE DO MODELO

fit_jointP <- 
  mcglm(linear_pred = c(form.ncorpo,
                        form.ncorpo2,
                        form.ncorpo3,
                        form.ncorpo4),
        matrix_pred = list(c(Z0, Z1, Z2),
                           c(Z0, Z1, Z2),
                           c(Z0, Z1, Z2),
                           c(Z0, Z1, Z2)),
        link = c("log","log","log","log"),
        variance = c("poisson_tweedie",
                     "poisson_tweedie",
                     "poisson_tweedie",
                     "poisson_tweedie"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 0.1,
                                 max_iter = 20,
                                 tol = 1e-01),
        power_fixed = c(F, F, F, F),
        data = dados2)

#----------------------------------------------------------------

# RESUMO DO MODELO

summary(fit_jointP)

#################################################################
# TABELA DE ANÁLISE DE VARIÂNCIA VIA TESTE WALD
#################################################################

## Elementos necessários para obtenção da estatística de teste

#----------------------------------------------------------------

# Vetor beta chapeu e indice de resposta
beta <- coef(fit_jointP, type = "beta")[,c(1, 4)]

#----------------------------------------------------------------

# Número de betas por resposta
n_beta <- as.vector(table(beta$Response))

#----------------------------------------------------------------

# Número de respostas
n_resp <- length(n_beta) 

#----------------------------------------------------------------

# Lista vcov por resposta desconsiderando parametros de dispersao e potencia

vcov_betas <- list()

vcov_betas[[1]] <- vcov(fit_jointP)[1:n_beta[1], 1:n_beta[1]]

for (i in 2:n_resp) {
  vcov_betas[[i]] <- 
    vcov(fit_jointP)[(cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i]), 
                     (cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i])] 
  
}


#----------------------------------------------------------------

# Índice que associa beta a variável por resposta

p_var <- list()

for (i in 1:n_resp) {
  p_var[[i]] <- attr(fit_jointP$list_X[[i]], "assign")
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
  beta_names[[i]] <- fit_jointP$beta_names[[i]]  
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

############################################################################

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
W <- vector() # Vetor para a estatística de teste
gl <- vector() # Vetor para graus de liberdade
p_val <- vector() # Vetor para p-valor

for (j in 1:n_resp) {
  for (i in 1:length(L_par[[j]])) {
    W[i] <- as.numeric((t(L_par[[j]][[i]] %*% subset(beta, beta$Response == j)$Estimates)) %*% (solve(L_par[[j]][[i]]%*%vcov_betas[[j]]%*%t(L_par[[j]][[i]]))) %*% (L_par[[j]][[i]] %*% subset(beta, beta$Response == j)$Estimates))
    gl[i] <- nrow(L_par[[j]][[i]])
    p_val[i] <- pchisq(W[i], df = gl[i], lower.tail = FALSE)
    
  } 
  tabela[[j]] <- 
    data.frame(Variável = c("Intercept", 
                            attr(terms(fit_jointP$linear_pred[[j]]), "term.labels")),
               GL = gl,
               W = round(W, 3),
               P_valor = round(p_val, 3))
}


#------------------------------------------------------------------

anova_pc <- anova(fit_jointP)

tabela[[4]]
anova_pc[[4]]

#------------------------------------------------------------------