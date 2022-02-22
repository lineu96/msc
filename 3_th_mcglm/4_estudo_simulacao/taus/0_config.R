#----------------------------------------------------------------

# ESTUDO DE SIMULAÇÃO

#----------------------------------------------------------------

# 500 data sets
# 4 tamanhos amostrais: 50, 100, 250, 500 e 1000
# 3 distribuições: normal, poisson, bernoulli (binomial n=1)

#----------------------------------------------------------------
  
# Taus = 0.5,0.5

#----------------------------------------------------------------
  
# 20 pontos: faz-se um decrécimo de tau/20 na hipótese

# Obtem a distância euclideana do vetor original para o modificado. 

# Ao final divide o vetor pelo seu sd para independente dos betas 
# iniciais as distancias estarem sempre na mesma escala

#----------------------------------------------------------------
  
# Univariado normal
# Univariado poisson
# Univariado bernouli

# Trivariado normal
# Trivariado poisson
# Trivariado bernouli

# Trivariado normal, poisson, bernouli

#----------------------------------------------------------------

# bibliotecas necessárias

library(mcglm)
library(Matrix)
library(NORTARA)

#----------------------------------------------------------------
# minhas funções
source('~/msc/3_th_mcglm/0_funcoes/functions.R')
#----------------------------------------------------------------

source("~/msc/3_th_mcglm/4_estudo_simulacao/taus/funcoes_simula/simula_uni_long_normal.R")
source("~/msc/3_th_mcglm/4_estudo_simulacao/taus/funcoes_simula/simula_uni_long_pois_binom.R")

#----------------------------------------------------------------

# Parâmetros

n_datasets = 500
n_rep = 5
n_distances = 20
taus = c(0.5,0.5)

#----------------------------------------------------------------