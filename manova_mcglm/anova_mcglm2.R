####################################################################
# ANÁLISE DE VARIÂNCIA UNIVARIADA PARA MÚLTIPLAS RESPOSTAS NO MCGLM
####################################################################

#----------------------------------------------------------------
library(mcglm)
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
W <- vector() # Vetor para a estatística de teste
gl <- vector() # Vetor para graus de liberdade
p_val <- vector() # Vetor para p-valor

for (j in 1:n_resp) {
  for (i in 1:dim(L_par[[j]])) {
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

#----------------------------------------------------------------

tabela

anova(fit_jointP)

#----------------------------------------------------------------