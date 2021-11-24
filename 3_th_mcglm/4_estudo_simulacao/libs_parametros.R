
#----------------------------------------------------------------
# bibliotecas necessárias

library(mcglm)
library(Matrix)
library(tidyverse)
library(simglm)

#----------------------------------------------------------------
# minhas funções
source('~/msc/3_th_mcglm/0_funcoes/functions.R')
#----------------------------------------------------------------

# parâmetros para testar funções

## prototipo 1

sample_size = 25      # tamanho das amostras
n_datasets = 10        # numero de conjuntos de dados
variance_error = 1.5   # variabilidade da amostra simulada
betas = c(5,0,0,0)    # valores dos parametros de regressao
dif_effects = 0.5    # decréscimo em beta 0 e distribuição nos demais betas
outcome_type = NULL 

## prototipo 2
sample_size = 25      # tamanho das amostras
n_datasets = 10 # numero de conjuntos de dados
variance_error = 1.5   # variabilidade da amostra simulada
betas_iniciais = c(5,0,0,0) # valores iniciais dos parametros de regressao
dif_effects = 0.5 # decréscimo em beta 0 e distribuição nos demais betas
outcome_type = NULL 

#----------------------------------------------------------------