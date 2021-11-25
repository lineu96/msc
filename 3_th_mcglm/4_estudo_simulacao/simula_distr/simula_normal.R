#----------------------------------------------------------------

# Protótipo estudo de simulação teste wald para mcglm

# Simula de um modelo, varia hipótese

# Distribuição normal

#----------------------------------------------------------------

simula_normal <- function(sample_size = 25,      # tamanho das amostras
                          n_datasets = 10,        # numero de conjuntos de dados
                          n_trat = 4, # número de tratamentos
                          betas = c(5,0,0,0),    # valores dos parametros de regressao
                          dif_effects = 0.5)    # decréscimo em beta 0 e distribuição nos demais betas
                          
{
  
  # geração dos conjuntos de dados
  
  trat <- gl(n_trat, sample_size/n_trat)
  
  datasets <- list()
  
  for (i in 1:n_datasets) {
    
    X <- model.matrix(~ trat)
    
    mu <- X%*%betas
    
    y <- rnorm(sample_size, mean = mu, sd = 1)
    
    datasets[[i]] <- data.frame(y = y,
                        x = trat)
  }
  
  
  #----------------------------------------------------------------
  
  # elementos mcglm
  
  form <- y ~ x # preditor
  
  Z0 <- mc_id(datasets[[1]]) # matriz identidade para o preditor matricial
  
  #----------------------------------------------------------------
  
  # ajuste de um modelo por conjunto de dados
  
  models <- list()
  
  for (i in 1:n_datasets) {
    fit <- 
      mcglm(linear_pred = c(form),
            matrix_pred = list(c(Z0)),
            link = c("identity"),
            variance = c("constant"),
            data = datasets[[i]])
    
    models[[i]] <- fit
    print(i)
  }
  
  #----------------------------------------------------------------
  
  dists <- vector() # vetor para armazenar as distancias
  dists[1] <- 0 # distancia inicial 0 
  
  hyp_betas <- betas # vetor inicial para distribuir os efeitos
  
  hypothesis <- list() # vetor para armazenar as hipoteses
  
  # hipotese inicial
  hypothesis[[1]] <- paste(c('beta10',
                             'beta11',
                             'beta12',
                             'beta13'), '=', betas)
  
  #----------------------------------------------------------------
  
  # obtenção das hipoteses para função mc_linear_hypothesis
  # e distancias dos valores de betas inicialmente simulados
  
  for (i in 2:(5/dif_effects)) {
    
    hyp_betas[1] <- hyp_betas[1] - dif_effects
    hyp_betas[c(2,3,4)] <- hyp_betas[c(2,3,4)] + dif_effects/3  
    
    hypothesis[[i]] <- paste(c('beta10',
                               'beta11',
                               'beta12',
                               'beta13'), '=', hyp_betas)
    
    dists[[i]] <- dist(rbind(betas, hyp_betas), method = "euclidean")
  }
  
  #----------------------------------------------------------------
  
  # obtenção do p-valor para a cada hipotese em cada dataset
  # armazena hipotese na linha, modelo na coluna
  
  p_test <- matrix(nrow = length(hypothesis), 
                   ncol = length(models))
  
  
  for (i in 1:length(models)) {
    for (j in 1:length(hypothesis)) {
      p_test[j,i] <- mc_linear_hypothesis(object =  models[[i]], 
                                          hypothesis = hypothesis[[j]])$P_valor
    }
  }
  
  #----------------------------------------------------------------
  
  # converte resultado para dataframe
  p_test <- as.data.frame(p_test)
  
  #----------------------------------------------------------------
  
  # acrescenta info de distancia
  p_test$dist <- dists
  
  #----------------------------------------------------------------
  
  # obtendo percentual de rejeição
  
  rej <- ifelse(p_test[,1:(ncol(p_test)-1)] < 0.05, 1, 0)
  
  df_final <- data.frame(dist = p_test$dist,
                         rej = (rowSums(rej)/(ncol(p_test)-1)*100))
  
  #----------------------------------------------------------------
  
  return(df_final)
}

#----------------------------------------------------------------
