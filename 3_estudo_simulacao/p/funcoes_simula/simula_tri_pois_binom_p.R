
#simula_tri_pois_binom <- function(sample_size = 50,
#                                  n_treatment = 2,
#                                  betas = c(1.3,0.7),
#                                  n_datasets = 5,
#                                  n_distances = 20,
#                                  distribution = 'poisson'){

source("~/msc/0_funcoes/mc_linear_hypothesis.R")

sample_size = 1000
n_treatment = 2
betas = c(0.35,0.15)
n_datasets = 500
n_distances = 20
distribution = 'binomial'

  #---------------------------------------------------
  
  # tratamentos
  trat <- gl(n_treatment, sample_size/n_treatment)
  
  # matriz do modelo
  X <- model.matrix(~ trat)

  cor_matrix <- matrix(c(1.0 , 0.75,0.5,
                         0.75, 1.0 ,0.25,
                         0.5 , 0.25 ,1.0
  ), 3,3)

  #---------------------------------------------------
  
  # Gerando as respostas
  
  # Elementos NORTARA
  
  ## Marginais
  switch(distribution,
         "poisson" = {
           invcdfnames <- c("qpois","qpois", "qpois")
           link <- "log"
           variance <- "tweedie"
           
           # lambdas por resposta
           lambda <- exp(X%*%betas)
           
         },
         
         "binomial" = {
           invcdfnames <- c("qbinom","qbinom", "qbinom")
           link <- "logit"
           variance <- "binomialP"
           
           p <- exp(X%*%betas)/(1 + exp(X%*%betas))
         }
  )
  
  # Geração das amostras
  temp <- list()
  
  for(i in 1:sample_size) {
    
    switch(distribution,
           "poisson" = {
             lambda <- exp(X%*%betas)
             
             paramslists <- list(
               m1 = list(lambda = lambda[i]),
               m2 = list(lambda = lambda[i]),
               m3 = list(lambda = lambda[i])
             )
           },
           
           "binomial" = {
             p <- exp(X%*%betas)/(1 + exp(X%*%betas))
             
             paramslists <- list(
               m1 = list(p = p[i], size = 1),
               m2 = list(p = p[i], size = 1),
               m3 = list(p = p[i], size = 1)
             )
           }
    )
    
    
    temp[[i]] <- NORTARA::genNORTARA(n = n_datasets+9000, 
                                     cor_matrix = cor_matrix, 
                                     invcdfnames = invcdfnames, 
                                     paramslists = paramslists)
    print(i)
  }
  
  
  datasets <- list()
  
  for(j in 1:(n_datasets+9000)) {
    datasets[[j]] <- matrix(NA, ncol = 3, nrow = sample_size)
    for(i in 1:sample_size) {
      datasets[[j]][i,] <- temp[[i]][j,]
    }
  }
  
  
  for (i in 1:(n_datasets+9000)) {
    datasets[[i]] <- as.data.frame(datasets[[i]])
    names(datasets[[i]]) <- c('y1', 'y2', 'y3')
    datasets[[i]]$x <- trat  
  }
  

  #---------------------------------------------------
  # elementos mcglm
  
  # McGLM
  
  form1 = y1~x
  form2 = y2~x
  form3 = y3~x
  Z0 <- mc_id(datasets[[1]])
  
  #----------------------------------------------------------------
  
  # ajuste de um modelo por conjunto de dados
  
  models <- list()
  
  switch(distribution,
         
         "poisson" = {
           
           for (i in 1:(n_datasets+9000)) {
             fit <- 
               try(mcglm(linear_pred = c(form1, form2, form3),
                         matrix_pred = list(Z0,Z0,Z0),
                         link = c(link, link, link), 
                         variance = c(variance, variance, variance),
                         data = datasets[[i]],
                         power_fixed = c(F,F,F)))
             
             models[[i]] <- fit
             print(i)
           }
         },
         
         "binomial" = {
           
           if (sample_size < 100) {
             ca <- list(#verbose = T,
               tuning = 1,
               max_iter = 100,
               tol = 0.5) 
             
           } else {
             ca <- list() 
           }
           
           for (i in 1:(n_datasets+9000)) {
             fit <- 
               try(mcglm(linear_pred = c(form1, form2, form3),
                         matrix_pred = list(Z0,Z0,Z0),
                         link = c(link, link, link), 
                         variance = c(variance, variance, variance),
                         Ntrial = list(1,1,1),
                         data = datasets[[i]],
                         control_algorithm = ca,
                         power_fixed = c(F,F,F)))
             
             models[[i]] <- fit
             print(i)
           }
         }
  )
  
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
  
  for (i in 1:(n_datasets+9000)) {
    parameters[,i+2] <- try(coef(models[[i]])$Estimates)
  }
  
  vcovs <- list()
  
  for (i in 1:(n_datasets+9000)) {
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
  
  length(models)-length(index_problems)
  
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
  
  
  
  binomial_tri_n1000 = results
  save(binomial_tri_n1000, file = 'binomial_tri_n1000.Rdata')
  rm(list = ls())
  
  
  #  return(results)
  #}
  
  #----------------------------------------------------------------