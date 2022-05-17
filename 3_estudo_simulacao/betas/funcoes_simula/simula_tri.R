simula_tri <- function(sample_size = 50,
                       n_treatment = 4,
                       betas_normal = c(5,0,0,0),
                       betas_poisson = c(2.3,0,0,0),
                       betas_bernoulli = c(0.5,0,0,0),
                       n_datasets = 500,
                       n_distances = 20,
                       decrease_normal = 0.15,
                       decrease_poisson = 0.05,
                       decrease_bernoulli = 0.25){

# sample_size = 50
# n_treatment = 4
# betas_normal = c(5,0,0,0)
# betas_poisson = c(2.3,0,0,0)
# betas_bernoulli = c(0.5,0,0,0)
# n_datasets = 15
# n_distances = 20
# decrease_normal = 0.15
# decrease_poisson = 0.05
# decrease_bernoulli = 0.25

  #---------------------------------------------------
  
  # tratamentos
  trat <- gl(n_treatment, sample_size/n_treatment)
  
  # matriz do modelo
  X <- model.matrix(~ trat)
  
  #---------------------------------------------------
  
  # Elementos NORTARA
  
  ## Marginais
  
  invcdfnames <- c("qnorm","qpois", "qbinom")
  
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
  
  # Geração das amostras
  temp <- list()
  
  for(i in 1:sample_size) {
    
    # Argumentos das marginais
    mu <- X%*%betas_normal
    lambda <- exp(X%*%betas_poisson)
    p <- exp(X%*%betas_bernoulli)/(1 + exp(X%*%betas_bernoulli))
    
    paramslists <- list(
      m1 = list(mean = mu[i], sd = 1),
      m2 = list(lambda = lambda[i]),
      m3 = list(p = p[i], size = 1)
    )
    
    
    temp[[i]] <- NORTARA::genNORTARA(n = n_datasets+5, 
                                     cor_matrix = cor_matrix, 
                                     invcdfnames = invcdfnames, 
                                     paramslists = paramslists)
    print(i)
  }
  
  
  datasets <- list()
  
  for(j in 1:(n_datasets+5)) {
    datasets[[j]] <- matrix(NA, ncol = 3, nrow = sample_size)
    for(i in 1:sample_size) {
      datasets[[j]][i,] <- temp[[i]][j,]
    }
  }
  
  
  for (i in 1:(n_datasets+5)) {
    datasets[[i]] <- as.data.frame(datasets[[i]])
    names(datasets[[i]]) <- c('y1', 'y2', 'y3')
    datasets[[i]]$x <- trat  
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
  
  # McGLM
  
  form1 = y1~x
  form2 = y2~x
  form3 = y3~x
  
  link_normal <- "identity"
  variance_normal <- "constant"
  
  link_poisson <- "log"
  variance_poisson <- "tweedie"
  
  link_bernoulli <- "logit"
  variance_bernoulli <- "binomialP"
  
  Z0 <- mc_id(datasets[[1]])
  
  #----------------------------------------------------------------
  
  # ajuste de um modelo por conjunto de dados
  
  models <- list()
  
  if (sample_size < 100) {
    ca <- list(#verbose = T,
      tuning = 1,
      max_iter = 100,
      tol = 0.5) 
    
  } else {
    ca <- list() 
  }
  
  for (i in 1:(n_datasets+5)) {
    fit <- 
      try(mcglm(linear_pred = c(form1, form2, form3),
            matrix_pred = list(Z0,Z0,Z0),
            link = c(link_normal, link_poisson, link_bernoulli), 
            variance = c(variance_normal, variance_poisson, variance_bernoulli),
            data = datasets[[i]],
            control_algorithm = ca))
    
    models[[i]] <- fit
    print(i)
  }
  
  #----------------------------------------------------------------
  
  # obtenção das hipoteses para função mc_linear_hypothesis
  # e distancias dos valores de betas inicialmente simulados
  
  dists <- vector() # vetor para armazenar as distancias
  dists[1] <- 0 # distancia inicial 0 
  
  hyp_betas_normal <- betas_normal # vetor inicial para distribuir os efeitos
  hyp_betas_poisson <- betas_poisson # vetor inicial para distribuir os efeitos
  hyp_betas_bernoulli <- betas_bernoulli # vetor inicial para distribuir os efeitos
  
  hypothesis <- list() # vetor para armazenar as hipoteses
  
  # hipotese inicial
  hypothesis[[1]] <- paste(coef(models[[1]], type = 'beta')$Parameters,
                           '=', 
                           c(betas_normal, 
                             betas_poisson, 
                             betas_bernoulli))
  
  # obtenção das distâncias e hipóteses a serem testadas
  for (i in 2:n_distances) {
    
    hyp_betas_normal[1] <- hyp_betas_normal[1] - decrease_normal
    hyp_betas_normal[2:length(betas_normal)] <- hyp_betas_normal[2:length(betas_normal)] + (decrease_normal/(n_treatment-1))
    
    hyp_betas_poisson[1] <- hyp_betas_poisson[1] - decrease_poisson
    hyp_betas_poisson[2:length(betas_poisson)] <- hyp_betas_poisson[2:length(betas_poisson)] + (decrease_poisson/(n_treatment-1))
    
    hyp_betas_bernoulli[1] <- hyp_betas_bernoulli[1] - decrease_bernoulli
    hyp_betas_bernoulli[2:length(betas_bernoulli)] <- hyp_betas_bernoulli[2:length(betas_bernoulli)] + (decrease_bernoulli/(n_treatment-1))
    
    hypothesis[[i]] <- paste(coef(models[[1]], type = 'beta')$Parameters,
                             '=',
                             c(hyp_betas_normal,
                               hyp_betas_poisson,
                               hyp_betas_bernoulli))
    
    dists[[i]] <- dist(rbind(betas_normal, 
                             hyp_betas_normal), 
                       method = "euclidean")
  }
  
  #----------------------------------------------------------------
  # Dividindo as distâncias pelo desvio padrão das distâncias para
  # independente dos betas elas estarem no mesmo intervalo
  
  dists <- (dists - min(dists)) / diff(range(dists))
  
  #----------------------------------------------------------------
  
  # Armazenando estimativas dos parametros e vcov das estimativas
  
  parameters <- data.frame(Parameters = coef(models[[1]])$Parameters,
                           Type = coef(models[[1]])$Type)
  
  for (i in 1:(n_datasets+5)) {
    parameters[,i+2] <- coef(models[[i]])$Estimates
  }
  
  vcovs <- list()
  
  for (i in 1:(n_datasets+5)) {
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
                                              hypothesis = hypothesis[[j]])$`Pr(>Chi)`)
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
  
  # obtém percentual de rejeição
  
  rej <- ifelse(p_test[,1:(ncol(p_test))] < 0.05, 1, 0)
  
  index_problems <- names(which(colSums(is.na(p_test)) > 0))
  
  rej2 <- rej[,!(colnames(rej) %in% index_problems)][,1:n_datasets]
  
  df_final <- data.frame(dist = dists,
                         rej = ((rowSums(rej2))/ncol(rej2))*100)
  
  df_final$distribution <- "normal/poisson/bernoulli"
  df_final$sample_size <- sample_size
  df_final$n_datasets <- ncol(rej2)
  
  #----------------------------------------------------------------
  
  # retorna dataframe com o percentual de rejeição para cada hipótese
  
  results <-list(hypothesis = hypothesis, 
                 parameters = parameters, 
                 vcovs = vcovs, 
                 p_test = p_test, 
                 index_problems = index_problems,
                 df_final = df_final)
  
    return(results)
  }
  
  #----------------------------------------------------------------
