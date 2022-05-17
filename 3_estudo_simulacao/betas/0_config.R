#----------------------------------------------------------------

# ESTUDO DE SIMULAÇÃO

#----------------------------------------------------------------

# 500 data sets
# 4 tamanhos amostrais: 50, 100, 250, 500 e 1000
# 3 distribuições: normal, poisson, bernoulli (binomial n=1)

#----------------------------------------------------------------
  
# Betas normal = 5,0,0,0 (cv = 20%)
# Betas poisson = 2.3,0,0,0 (contagens próximas de 10)
# Betas bernoulli = 0.5,0,0,0 (p próximo de 0.6)

#----------------------------------------------------------------
  
# 20 pontos: faz-se um decrécimo em beta0 e distribui esse 
# decrécimo nos demais betas. 

# Obtem a distância euclideana do vetor original para o modificado. 

# Ao final padroniza o vetor para que independente dos betas 
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

#----------------------------------------------------------------
# minhas funções
source('~/msc/0_funcoes/functions.R')
#----------------------------------------------------------------

source("~/msc/3_estudo_simulacao/betas/funcoes_simula/simula_uni.R")
source("~/msc/3_estudo_simulacao/betas/funcoes_simula/simula_tri_normal.R")
source("~/msc/3_estudo_simulacao/betas/funcoes_simula/simula_tri_pois_binom.R")
source("~/msc/3_estudo_simulacao/betas/funcoes_simula/simula_tri.R")

#----------------------------------------------------------------

# Parâmetros

n_datasets = 500
n_treatment = 4
n_distances = 20

betas_normal = c(5,0,0,0)
betas_poisson = c(2.3,0,0,0)
betas_bernoulli = c(0.5,0,0,0)

decrease_normal = 0.15
decrease_poisson = 0.05
decrease_bernoulli = 0.25

#----------------------------------------------------------------