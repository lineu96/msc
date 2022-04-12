
#----------------------------------------------------------------

# bibliotecas necessárias

library(Matrix)
library(mcglm)

load("NORTARA_FUN.RData")

#----------------------------------------------------------------
# minhas funções
source('functions.R')
#----------------------------------------------------------------

source("funcoes_simula/simula_uni_long_normal.R")
source("funcoes_simula/simula_uni_long_pois_binom.R")
source("funcoes_simula/simula_tri_long_normal.R")
source("funcoes_simula/simula_tri_long_pois_binom.R")

#----------------------------------------------------------------

sample_size = 100
n_datasets = 2
n_rep = 5
taus = c(0.5,0.5)
n_distances = 20

#----------------------------------------------------------------

## Matrix linear predictor
UM <- rep(1, n_rep)
Z0 <- Diagonal(n_rep, 1)
Z1 <- UM%*%t(UM)

#----------------------------------------------------------------

Omega <- mc_matrix_linear_predictor(tau = taus, 
                                    Z = list(Z0, Z1))

Omega1 <- as.matrix(Omega, n_rep, n_rep)
Omega2 <- as.matrix(Omega, n_rep, n_rep)
Omega3 <- as.matrix(Omega, n_rep, n_rep)

chol_1 <- chol(Omega1)
chol_2 <- chol(Omega2)
chol_3 <- chol(Omega3)

BB <- bdiag(chol_1, chol_2, chol_3)

Sigma_b <- Matrix(c(1.0,  0.75, 0.5,
                    0.75,  1.0, 0.25,
                    0.5,  0.25, 1.0), 
                  3, 3)

I <- Diagonal(5, 1)
C <- t(BB)%*%kronecker(Sigma_b, I)%*%BB
C <- as.matrix(C, (n_rep*3), (n_rep*3))

#----------------------------------------------------------------

## Marginais

invcdfnames <- c(rep('qnorm', n_rep),
                 rep('qpois', n_rep),
                 rep('qbinom', n_rep))

paramslists <- list(
  m1 = list(mean = 5),
  m2 = list(mean = 5),
  m3 = list(mean = 5),
  m4 = list(mean = 5),
  m5 = list(mean = 5),
  m6 = list(lambda = 10),
  m7 = list(lambda = 10),
  m8 = list(lambda = 10),
  m9 = list(lambda = 10),
  m10 = list(lambda = 10),
  m11 = list(p = 0.6, size = 1),
  m12 = list(p = 0.6, size = 1),
  m13 = list(p = 0.6, size = 1),
  m14 = list(p = 0.6, size = 1),
  m15 = list(p = 0.6, size = 1)
)

#----------------------------------------------------------------

# lista para armazenar os conjuntos de dados
datasets <- list()

# geração dos conjuntos de dados

for (i in 1:(n_datasets)) {
  
  
  data_temp <- genNORTARA(n = sample_size, 
                          cor_matrix = C, 
                          paramslists = paramslists, 
                          invcdfnames = invcdfnames)
  
  y1 <- c(t(data_temp[,1:5]))
  y2 <- c(t(data_temp[,6:10]))
  y3 <- c(t(data_temp[,11:15]))
  
  data <- data.frame(y1 = y1,
                     y2 = y2,
                     y3 = y3,
                     id = rep(1:sample_size, 
                              each = n_rep))
  
  datasets[[i]] <- data.frame(y1 = y1,
                              y2 = y2,
                              y3 = y3,
                              id = rep(1:sample_size, 
                                       each = n_rep))
  
}

#----------------------------------------------------------------