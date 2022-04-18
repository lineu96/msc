simula_uni <- function(sample_size = 100,
                       n_datasets = 500,
                       n_treatment = 2,
                       betas = c(0.35,0.15),
                       n_distances = 20,
                       distribution = 'binomial')
  
{
  
  # tratamentos
  trat <- gl(n_treatment, sample_size/n_treatment)
  
  # matriz do modelo
  X <- model.matrix(~ trat)
  
  # lista para armazenar os conjuntos de dados
  datasets <- list()
  
  # geração dos conjuntos de dados
  
  # switch para simular de diferentes distribuições
  # (normal, poisson, binomial n=10 ou beta)
  # e definir função de ligação e variância para 
  # ajuste dos modelos (depende da distribuição)
  
  switch(distribution,
         
         "normal" = {
           
           link <- c("identity")
           variance <- c("tweedie")
           
           for (i in 1:(n_datasets+150)) {
             
             mu <- X%*%betas
             
             y <- rnorm(sample_size, mean = mu, sd = 1)
             
             datasets[[i]] <- data.frame(y = y,
                                         x = trat)
           }
         },
         
         "poisson" = {
           
           link <- c("log")
           variance <- c("tweedie")
           
           for (i in 1:(n_datasets+150)) {
             
             lambda <- exp(X%*%betas)
             
             y <- rpois(sample_size, 
                        lambda = lambda)
             
             datasets[[i]] <- data.frame(y = y,
                                         x = trat)
           }
         },
         
         "binomial" = {
           
           link <-  "logit" 
           variance  <-  "binomialP"
           
           for (i in 1:(n_datasets+150)) {
             
             p <- exp(X%*%betas)/(1 + exp(X%*%betas))
             
             y <- rbinom(sample_size, 
                         p = p,
                         size = 1)
             
             datasets[[i]] <- data.frame(y = y,
                                         x = trat)
           }
         },
         
         "beta" = {
           
           link <- "logit"
           
           variance <- "binomialP"
           
           for (i in 1:(n_datasets+150)) {
             
             p <- exp(X%*%betas)/(1 + exp(X%*%betas))
             
             y <- gamlss.dist::rBE(sample_size, 
                                   mu = p, 
                                   sigma = 0.2)
             
             datasets[[i]] <- data.frame(y = y,
                                         x = trat)
           }
         }
  )
  
  
  #----------------------------------------------------------------
  
  # elementos mcglm
  
  # caso seja binomial a resposta precisa ser declarada como razão
  # y/Ntrial ~x
  
  switch(distribution,
         "binomial" = {form = y/1~x},
         {form = y~x}
  )
  
  # preditor matricial
  Z0 <- mc_id(datasets[[1]]) # matriz identidade para o preditor matricial
  
  #----------------------------------------------------------------
  
  # ajuste de um modelo por conjunto de dados

  models <- list()
  
  switch(distribution,
         "binomial" = {
           
           if (sample_size < 100) {
             ca <- list(#verbose = T,
               tuning = 1,
               max_iter = 100,
               tol = 0.5) 
             
           } else {
             ca <- list() 
           }
           
           for (i in 1:(n_datasets+150)) {
             fit <- 
               try(mcglm(linear_pred = c(form),
                     matrix_pred = list(c(Z0)),
                     link = link, 
                     variance = variance,
                     Ntrial = list(1),
                     data = datasets[[i]],
                     control_algorithm = ca,
                     power_fixed = F))
             
             models[[i]] <- fit
             print(i)
           }
         },
         {
           for (i in 1:(n_datasets+150)) {
             fit <- 
               try(mcglm(linear_pred = c(form),
                     matrix_pred = list(c(Z0)),
                     link = link,
                     variance = variance,
                     data = datasets[[i]],
                     power_fixed = F))
             
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
                   "normal" = 0,
                   "binomial" = 1,
                   "poisson" = 1
  )
  
  hyp_p <- p_init # vetor inicial para distribuir os efeitos
  
  hypothesis <- list() # vetor para armazenar as hipoteses
  
  # hipotese inicial
  hypothesis[[1]] <- paste(coef(models[[1]], type = 'power')$Parameters,
                           '=', 
                           p_init)
  
  # obtenção das distâncias e hipóteses a serem testadas
  for (i in 2:n_distances) {
    
    hyp_p[1] <- hyp_p[1] + (2/n_distances)
    
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
  
  for (i in 1:(n_datasets+150)) {
    parameters[,i+2] <- try(coef(models[[i]])$Estimates)
  }
  
  vcovs <- list()
  
  for (i in 1:(n_datasets+150)) {
    vcovs[[i]] <- try(vcov(models[[i]])  )
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
  
  # acrescenta info de distancia
  #p_test$dist <- dists
  
  #----------------------------------------------------------------
  
  # obtém percentual de rejeição
  
  rej <- ifelse(p_test[,1:(ncol(p_test))] < 0.05, 1, 0)
  
  index_problems <- names(which(colSums(is.na(p_test)) > 0))
  
  rej2 <- rej[,!(colnames(rej) %in% index_problems)][,1:n_datasets]
  
  df_final <- data.frame(dist = dists,
                         rej = ((rowSums(rej2))/ncol(rej2))*100)
  
  
  
  #df_final <- data.frame(dist = p_test$dist,
  #                       rej = (rowSums(rej, na.rm = T)/
  #                                (ncol(p_test[ , colSums(is.na(p_test)) == 0])-1)*100))
  
  df_final$distribution <- paste('uni', distribution)
  df_final$sample_size <- sample_size
  #df_final$n_datasets <- (ncol(p_test[ , colSums(is.na(p_test)) == 0])-1)
  df_final$n_datasets <- ncol(rej2)
  
  #----------------------------------------------------------------
  
  # retorna dataframe com o percentual de rejeição para cada hipótese
  return(list(hypothesis = hypothesis, 
              parameters = parameters, 
              vcovs = vcovs, 
              p_test = p_test, 
              index_problems = index_problems,
              df_final = df_final))
}

#----------------------------------------------------------------

