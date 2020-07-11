####################################################################
# ANÁLISE DE VARIÂNCIA MULTIVARIADA NO MCGLM DO TIPO II
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

form.ncorpo  <- ncorpo  ~ (sessao + tempo + linhagem)^2
form.ncorpo2 <- ncabeca ~ (sessao + tempo + linhagem)^2
form.ncorpo3 <- norelha ~ (sessao + tempo + linhagem)^2

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
                        form.ncorpo3),
        matrix_pred = list(c(Z0, Z1, Z2),
                           c(Z0, Z1, Z2),
                           c(Z0, Z1, Z2)),
        link = c("log","log","log"),
        variance = c("poisson_tweedie",
                     "poisson_tweedie",
                     "poisson_tweedie"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 0.1,
                                 max_iter = 20,
                                 tol = 1e-01),
        power_fixed = c(F, F, F),
        data = dados2)

#----------------------------------------------------------------

# RESUMO DO MODELO

summary(fit_jointP)

#################################################################
# TABELA DE ANÁLISE DE VARIÂNCIA MULTIVARIADA VIA TESTE WALD
#################################################################

## Elementos necessários para obtenção da estatística de teste

#----------------------------------------------------------------

# Vetor beta chapeu
beta <- coef(fit_jointP, type = "beta")[,c(1, 4)] 

#----------------------------------------------------------------

# Número de betas
n_beta <- sum(as.vector(table(beta$Response)))

#----------------------------------------------------------------

# Número de respostas
n_resp <- length(as.vector(table(beta$Response)))

#----------------------------------------------------------------

# vcov desconsiderando parametros de dispersao e potencia
vcov_betas <- vcov(fit_jointP)[1:n_beta, 1:n_beta]

#----------------------------------------------------------------

# Indice que associa parametro a variavel
p_var <- attr(fit_jointP$list_X[[1]], "assign")

#----------------------------------------------------------------

# Matriz F para todos os parâmetros (Hypothesis matrix)
F_all <- diag(length(p_var))

#----------------------------------------------------------------

# Matriz F por variável (Hypothesis matrix)

expand <- by(data = F_all,
             INDICES = p_var,
             FUN = as.matrix)

beta_names <- fit_jointP$beta_names[[1]]

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

aux2 <- as.data.frame(do.call(cbind, aux))

p_varII <- aux2

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
                                  attr(terms(fit_jointP$linear_pred[[1]]), "term.labels")),
                     GL = gl,
                     W = round(W, 3),
                     P_valor = round(p_val, 3))

#----------------------------------------------------------------

tabela
mc_manova(fit_jointP)

#----------------------------------------------------------------