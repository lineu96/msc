simula_uni_long <- function(sample_size = 100,
                            n_datasets = 100,
                            n_rep = 5,
                            taus = c(1,0),
                            n_distances = 20,
                            distribution = 'normal')
  
{
  
  #sample_size = 100
  #n_datasets = 10
  #n_rep = 5
  #taus = c(1,0)
  #n_distances = 20
  #distribution = 'poisson'
  
  switch(distribution,
         "poisson" = {
           link <- "log"
           variance <- "tweedie"
         },
         
         "bernoulli" = {
           link <- "logit"
           variance <- "binomialP"
         },
         
         "normal" = {
           link <- "identity"
           variance <- "constant"
         }
  )
  
  # lista para armazenar os conjuntos de dados
  datasets <- list()
  
  # geração dos conjuntos de dados
  
  for (i in 1:(n_datasets)) {
   
    switch(distribution,
           "poisson" = {
             y <- rpois(sample_size*n_rep, 10)
             datasets[[i]] <- data.frame(y = y, 
                                         id = rep(1:sample_size, 
                                                  each = n_rep))
           },
           
           "bernoulli" = {
             y <- rbinom(sample_size*n_rep, 1,0.6)
             datasets[[i]] <- data.frame(y = y, 
                                         id = rep(1:sample_size, 
                                                  each = n_rep))
           },
           
           "normal" = {
             y <- rnorm(sample_size*n_rep, 5,1)
             datasets[[i]] <- data.frame(y = y, 
                                         id = rep(1:sample_size, 
                                                  each = n_rep))
           }
    ) 
    
    print(i)
  }
  
  #----------------------------------------------------------------
  
  # elementos mcglm
  
  # caso seja binomial a resposta precisa ser declarada como razão
  # y/Ntrial ~x
  
  form = y~1
  
  # preditor matricial
  Z0 <- mc_id(datasets[[1]]) # matriz identidade para o preditor matricial
  Z1 <- mc_mixed(~0 + as.factor(id), data = datasets[[1]])
  
  #----------------------------------------------------------------
  
  # ajuste de um modelo por conjunto de dados
  
  models <- list()
  
  switch(distribution,
         "poisson" = {
           for (i in 1:n_datasets) {
             fit <- 
               mcglm(linear_pred = c(form),
                     matrix_pred = list(c(Z0, Z1)),
                     link = link, 
                     variance = variance,
                     data = datasets[[i]])
             
             models[[i]] <- fit
             print(i)
           }
         },
         
         "bernoulli" = {
           for (i in 1:n_datasets) {
             fit <- 
               mcglm(linear_pred = c(form),
                     matrix_pred = list(c(Z0, Z1)),
                     link = link, 
                     variance = variance,
                     Ntrial = list(1),
                     data = datasets[[i]])
             
             models[[i]] <- fit
             print(i)
           }
         },
         
         "normal" = {
           for (i in 1:n_datasets) {
             fit <- 
               mcglm(linear_pred = c(form),
                     matrix_pred = list(c(Z0, Z1)),
                     link = link, 
                     variance = variance,
                     data = datasets[[i]])
             
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
  
  hyp_taus <- taus # vetor inicial para distribuir os efeitos
  
  hypothesis <- list() # vetor para armazenar as hipoteses
  
  # hipotese inicial
  hypothesis[[1]] <- paste(coef(models[[1]], type = 'tau')$Parameters,
                           '=', 
                           taus)
  
  # obtenção das distâncias e hipóteses a serem testadas
  for (i in 2:n_distances) {
    
    hyp_taus[1] <- hyp_taus[1] - 0.02
    hyp_taus[2] <- hyp_taus[2] + 0.02
    
    hypothesis[[i]] <- paste(coef(models[[1]], type = 'tau')$Parameters,
                             '=',
                             hyp_taus)
    
    dists[[i]] <- dist(rbind(taus, hyp_taus), method = "euclidean")
  }
  #----------------------------------------------------------------
  # Dividindo as distâncias pelo desvio padrão das distâncias para
  # independente dos betas elas estarem no mesmo intervalo
  
  dists <- (dists - min(dists)) / diff(range(dists))
  
  #----------------------------------------------------------------
  
  # Armazenando estimativas dos parametros e vcov das estimativas
  
  parameters <- data.frame(Parameters = coef(models[[1]])$Parameters,
                           Type = coef(models[[1]])$Type)
  
  for (i in 1:(n_datasets)) {
    parameters[,i+2] <- coef(models[[i]])$Estimates
  }
  
  vcovs <- list()
  
  for (i in 1:(n_datasets)) {
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
  
  df_final$distribution <- paste('uni', distribution)
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