#----------------------------------------------------------------

# Protótipo estudo de simulação teste wald para mcglm

# Varia o modelo, testa mesma hipótese

#----------------------------------------------------------------

library(mcglm)
library(Matrix)
library(tidyverse)
library(simglm)

#----------------------------------------------------------------

source('~/msc/3_th_mcglm/0_funcoes/functions.R')

#----------------------------------------------------------------

prototipo2_lm <- function(sample_size = 25,      # tamanho das amostras
                          n_datasets = 100, # numero de conjuntos de dados
                          variance_error = 1.5,   # variabilidade da amostra simulada
                          betas_iniciais = c(5,0,0,0), # valores iniciais dos parametros de regressao
                          dif_effects = 0.1, # decréscimo em beta 0 e distribuição nos demais betas
                          outcome_type = NULL   # tipo de resposta simulada (NULL para normal,
                          # 'logistic', 'poisson'
){

  betas <- list() # lista para armazenar os betas para gerar datasets
  betas[[1]] <- betas_iniciais # primeiro elemento igual aos betas iniciais
  
  dists <- vector() # vetor para armazenar as distancias
  dists[1] <- 0 # distancia inicial 0 
  
  hyp_betas <- betas_iniciais # valor inicial para distribuição de efeitos
  
  #----------------------------------------------------------------
  
  # obtenção dos betas para simular
  
  for (i in 2:(5/dif_effects)) {
    
    hyp_betas[1] <- hyp_betas[1] - dif_effects
    hyp_betas[c(2,3,4)] <- hyp_betas[c(2,3,4)] + dif_effects/3  
    
    betas[[i]] <- hyp_betas
    
    dists[[i]] <- dist(rbind(betas_iniciais, hyp_betas), method = "euclidean")
  }
  
  #----------------------------------------------------------------
  
  # argumentos para simulação 
  
  sim_arguments <- list()
  
  for(i in 1:length(betas)){
    
    sim_arguments[[i]] <- list(
      formula = y ~ x,
      fixed = list(x = list(var_type = 'factor', 
                            levels = c('A', 'B', 'C', 'D'))),
      error = list(variance = variance_error),
      sample_size = sample_size,
      reg_weights = betas[[i]],
      outcome_type = outcome_type )
  }
  
  
  #----------------------------------------------------------------
  
  # geração dos conjuntos de dados
  
  datasets <- list()
  
  for (j in 1:length(betas)) {
    
    datasets[[j]] <- list()
    length(datasets[[j]]) <- n_datasets
    
    for (i in 1:n_datasets) {
      
      dados <- simulate_fixed(data = NULL, sim_arguments[[j]]) %>%
        simulate_error(sim_arguments[[j]]) %>%
        generate_response(sim_arguments[[j]])
      
      datasets[[j]][[i]] <- dados[c('x','y')]
    }  
  }
  
  #----------------------------------------------------------------
  
  # elementos mcglm
  
  form <- y ~ x # preditor
  
  Z0 <- mc_id(datasets[[1]][[1]]) # matriz identidade para o preditor matricial
  
  #----------------------------------------------------------------
  
  # ajuste de um modelo por conjunto de dados
  
  models <- list()
  
  for (j in 1:length(betas)) {
    
    models[[j]] <- list()
    length(models[[j]]) <- n_datasets
    
    for (i in 1:n_datasets) {
      
      fit <- 
        mcglm(linear_pred = c(form),
              matrix_pred = list(c(Z0)),
              link = c("identity"),
              variance = c("constant"),
              data = datasets[[j]][[i]])
      
      models[[j]][[i]] <- fit
      print(paste(j,i))
      
    }  
  }
  
  #----------------------------------------------------------------
  
  # hipótese a ser testada para cada modelo
  
  hypothesis <- paste(c('beta10',
                        'beta11',
                        'beta12',
                        'beta13'), '=', betas_iniciais)
  
  
  #----------------------------------------------------------------
  
  # obtenção do p-valor para a cada hipotese em cada dataset
  # armazena diferentes betas simulados na linha, modelo na coluna
  
  p_test <- matrix(nrow = length(betas), 
                   ncol = n_datasets)
  
  for (j in 1:length(betas)) {
    for (i in 1:n_datasets) {
      
      p_test[j,i] <-  mc_linear_hypothesis(object =  models[[j]][[i]], 
                                           hypothesis = hypothesis)$P_valor
      
    }  
  }
  
  #----------------------------------------------------------------
  
  # converte resultado para dataframe
  
  p_test <- as.data.frame(p_test)
  
  #----------------------------------------------------------------
  
  # acrescenta info de distancia
  
  p_test$dist <- dists  
  
  #----------------------------------------------------------------
  
  return(p_test)
  
}

