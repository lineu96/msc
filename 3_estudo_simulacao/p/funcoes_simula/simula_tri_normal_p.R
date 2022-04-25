# simula_tri_normal <- function(sample_size = 100,
#                               n_treatment = 2,
#                               betas = c(3,2),
#                               n_datasets = 10,
#                               n_distances = 20,
#                               distribution = 'normal'){

source("~/msc/0_funcoes/mc_linear_hypothesis.R")

sample_size = 1000
n_treatment = 2
betas = c(3,2)
n_datasets = 500
n_distances = 20
distribution = 'normal'

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
  
  for (i in 1:(n_datasets+500)) {
    
    # Argumentos das marginais
    
    mu <- X%*%betas
    
    mu <- matrix(c(mu, mu, mu),ncol = 3)
    
    y <- t(apply(mu, 1, 
                 function(m) mvtnorm::rmvnorm(1, mean=m, 
                                              sigma=cor_matrix)))
    
    datasets[[i]] <- data.frame(y,
                                x = trat)
    
    names(datasets[[i]]) <- c('y1', 'y2', 'y3', 'x')
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
  variance <- c("tweedie")
  
  form1 = y1~x
  form2 = y2~x
  form3 = y3~x
  Z0 <- mc_id(datasets[[1]])
  
  #----------------------------------------------------------------
  
  # ajuste de um modelo por conjunto de dados
  
  models <- list()
  
  control_initial <- list()
  
  control_initial$regression[[1]] <- c(3,2)
  control_initial$regression[[2]] <- c(3,2)
  control_initial$regression[[3]] <- c(3,2)
  
  control_initial$power[[1]] <- 0.5
  control_initial$power[[2]] <- 0.5
  control_initial$power[[3]] <- 0.5
  
  control_initial$tau[[1]] <- 0.5
  control_initial$tau[[2]] <- 0.5
  control_initial$tau[[3]] <- 0.5
  
  control_initial$rho <- c(0,0,0)
  
  for (i in 1:(n_datasets+500)) {
    fit <- 
      try(mcglm(linear_pred = c(form1, form2, form3),
            matrix_pred = list(Z0,Z0,Z0),
            link = c(link, link, link), 
            variance = c(variance, variance, variance),
            data = datasets[[i]],
            power_fixed = c(F,F,F),
            control_initial = control_initial))
    
    models[[i]] <- fit
    print(i)
  }
  
  #----------------------------------------------------------------
  
  # obtenção das hipoteses para função mc_linear_hypothesis
  # e distancias dos valores de betas inicialmente simulados
  
  dists <- vector() # vetor para armazenar as distancias
  dists[1] <- 0 # distancia inicial 0 
  
  p_init <- switch(distribution,
                   "normal" = c(0,0,0),
                   "binomial" = c(1,1,1),
                   "poisson" = c(1,1,1))
  
  hyp_p <- p_init  # vetor inicial para distribuir os efeitos
  
  hypothesis <- list() # vetor para armazenar as hipoteses
  
  # hipotese inicial
  hypothesis[[1]] <- paste(coef(models[[1]], type = 'power')$Parameters,
                           '=', 
                           p_init)
  
  # obtenção das distâncias e hipóteses a serem testadas
  for (i in 2:n_distances) {
    
    hyp_p <- hyp_p + (2/n_distances)
    
    hypothesis[[i]] <- paste(coef(models[[1]], type = 'power')$Parameters,
                             '=',
                             hyp_p)
    
    dists[[i]] <- dist(rbind(p_init, hyp_p), method = "euclidean")
  }
  
  #----------------------------------------------------------------
  # Dividindo as distâncias pelo desvio padrão das distâncias para
  # independente dos betas elas estarem no mesmo intervalo
  
  dists <- dists/sd(dists)
  
  #----------------------------------------------------------------
  
  # Armazenando estimativas dos parametros e vcov das estimativas
  
  parameters <- data.frame(Parameters = coef(models[[1]])$Parameters,
                           Type = coef(models[[1]])$Type)
  
  for (i in 1:(n_datasets+500)) {
    parameters[,i+2] <- try(coef(models[[i]])$Estimates)
  }
  
  vcovs <- list()
  
  for (i in 1:(n_datasets+500)) {
    vcovs[[i]] <- try(vcov(models[[i]]))
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
  
  # obtém percentual de rejeição
  
  rej <- ifelse(p_test[,1:(ncol(p_test))] < 0.05, 1, 0)
  
  index_problems <- names(which(colSums(is.na(p_test)) > 0))
  
  length(index_problems)
  
  rej2 <- rej[,!(colnames(rej) %in% index_problems)][,1:n_datasets]
  
  df_final <- data.frame(dist = dists,
                         rej = ((rowSums(rej2))/ncol(rej2))*100)
  
  df_final$distribution <- paste('tri', distribution)
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
  
  
  
  normal_tri_n1000 = results
  save(normal_tri_n1000, file = 'normal_tri_n1000.Rdata')
  rm(list = ls())
  
  
  #  return(results)
  #}
  
  #----------------------------------------------------------------