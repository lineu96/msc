simula_tri_normal <- function(sample_size = 100,
                              n_treatment = 4,
                              betas = c(5,0,0,0),
                              n_datasets = 100,
                              n_distances = 20,
                              distribution = 'normal'){
  
  #---------------------------------------------------
  
  # tratamentos
  trat <- gl(n_treatment, sample_size/n_treatment)
  
  # matriz do modelo
  X <- model.matrix(~ trat)
  
  #---------------------------------------------------
  
  ## Matriz de correlação alvo
  
  cor_matrix <- matrix(c(1.0,  0.75, 0.5,
                         0.75,  1.0, 0.25,
                         0.5,  0.25, 1.0
  ),3,3)
  
  #---------------------------------------------------
  
  # lista para armazenar os conjuntos de dados
  datasets <- list()
  
  #---------------------------------------------------
  
  # Gerando as respostas
  
  for (i in 1:n_datasets) {
    
    # Argumentos das marginais
    
    mu <- X%*%betas
    
    mu <- c(y1 = mu[1], y2 = mu[1], y3 = mu[1])
    
    y <- as.data.frame(mvtnorm::rmvnorm(n_datasets, 
                                        mean = mu, 
                                        sigma = cor_matrix))
    
    datasets[[i]] <- data.frame(y,
                                x = trat)
    
  }
  
  #---------------------------------------------------
  
  # Comparando correlação alvo e correlação dos dados
  
  # cor_matrix
  # 
  # for (i in 1:n_datasets) {
  #   print(cor(datasets[[i]][,1:3]))
  # }
  
  #---------------------------------------------------
  # elementos mcglm
  
  link <- c("identity")
  variance <- c("constant")
  
  form1 = y1~x
  form2 = y2~x
  form3 = y3~x
  Z0 <- mc_id(datasets[[1]])
  
  #----------------------------------------------------------------
  
  # ajuste de um modelo por conjunto de dados
  
  models <- list()
  
  for (i in 1:n_datasets) {
    fit <- 
      mcglm(linear_pred = c(form1, form2, form3),
            matrix_pred = list(Z0,Z0,Z0),
            link = c(link, link, link), 
            variance = c(variance, variance, variance),
            data = datasets[[i]])
    
    models[[i]] <- fit
    print(i)
  }
  
  #----------------------------------------------------------------
  
  # obtenção das hipoteses para função mc_linear_hypothesis
  # e distancias dos valores de betas inicialmente simulados
  
  dists <- vector() # vetor para armazenar as distancias
  dists[1] <- 0 # distancia inicial 0 
  
  hyp_betas <- betas # vetor inicial para distribuir os efeitos
  
  hypothesis <- list() # vetor para armazenar as hipoteses
  
  # hipotese inicial
  hypothesis[[1]] <- paste(coef(models[[1]], type = 'beta')$Parameters,
                           '=', 
                           rep(betas, 3))
  
  # obtenção das distâncias e hipóteses a serem testadas
  for (i in 2:n_distances) {
    
    hyp_betas[1] <- hyp_betas[1] - (betas[1]/n_distances)
    hyp_betas[2:length(betas)] <- hyp_betas[2:length(betas)] + (betas[1]/n_distances)/(n_treatment-1)
    
    hypothesis[[i]] <- paste(coef(models[[1]], type = 'beta')$Parameters,
                             '=',
                             rep(hyp_betas, 3))
    
    dists[[i]] <- dist(rbind(betas, hyp_betas), method = "euclidean")
  }
  
  #----------------------------------------------------------------
  # Dividindo as distâncias pelo desvio padrão das distâncias para
  # independente dos betas elas estarem no mesmo intervalo
  
  dists <- dists/sd(dists)
  
  #----------------------------------------------------------------
  
  # Armazenando estimativas dos parametros e vcov das estimativas
  
  parameters <- data.frame(Parameters = coef(models[[1]])$Parameters,
                           Type = coef(models[[1]])$Type)
  
  for (i in 1:n_datasets) {
    parameters[,i+2] <- coef(models[[i]])$Estimates
  }
  
  vcovs <- list()
  
  for (i in 1:n_datasets) {
    vcovs[[i]] <- vcov(models[[i]])  
  }
  
  #----------------------------------------------------------------
  
  # obtenção do p-valor para a cada hipotese em cada dataset
  # armazena hipotese na linha, modelo na coluna
  
  p_test <- matrix(nrow = length(hypothesis), 
                   ncol = length(models))
  
  
  for (i in 1:length(models)) {
    for (j in 1:length(hypothesis)) {
      p_test[j,i] <- try(mc_linear_hypothesis(object =  models[[i]], 
                                              hypothesis = hypothesis[[j]])$P_valor)
    }
  }
  
  #----------------------------------------------------------------
  
  # converte resultado para dataframe
  p_test <- as.data.frame(p_test)
  
  # caso tenha falhado, converte para NA
  for (i in 1:ncol(p_test)) {
    p_test[,i] <- as.numeric(p_test[,i])    
  }
  
  #----------------------------------------------------------------
  
  # acrescenta info de distancia
  p_test$dist <- dists
  
  #----------------------------------------------------------------
  
  # obtém percentual de rejeição
  
  rej <- ifelse(p_test[,1:(ncol(p_test)-1)] < 0.05, 1, 0)
  
  df_final <- data.frame(dist = p_test$dist,
                         rej = (rowSums(rej, na.rm = T)/
                                  (ncol(p_test[ , colSums(is.na(p_test)) == 0])-1)*100))
  
  df_final$distribution <- paste('tri', distribution)
  df_final$sample_size <- sample_size
  df_final$n_datasets <- (ncol(p_test[ , colSums(is.na(p_test)) == 0])-1)
  
  #----------------------------------------------------------------
  
  # retorna dataframe com o percentual de rejeição para cada hipótese
  return(list(hypothesis = hypothesis, 
              parameters = parameters, 
              vcovs = vcovs, 
              p_test = p_test, 
              df_final = df_final))
  
}
