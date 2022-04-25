# simula_uni_long_normal <- function(sample_size = 100,
#                                    n_datasets = 50,
#                                    n_rep = 5,
#                                    taus = c(0.5,0.5),
#                                    n_distances = 20,
#                                    distribution = 'normal')
#   
# {
  
sample_size = 100
n_datasets = 50
n_rep = 5
taus = c(0.5,0.5)
n_distances = 20
distribution = 'normal'

  ## Matrix linear predictor
  UM <- rep(1, n_rep)
  Z0 <- Diagonal(n_rep, 1)
  Z1 <- UM%*%t(UM)
  
  Omega <- mc_matrix_linear_predictor(tau = taus, 
                                      Z = list(Z0, Z1))
  
  Omega <- as.matrix(Omega, n_rep, n_rep)
  
  # lista para armazenar os conjuntos de dados
  datasets <- list()
  
  # geração dos conjuntos de dados
  
  for (i in 1:(n_datasets)) {
    
    mu <- c(y1 = 5, 
            y2 = 5, 
            y3 = 5,
            y4 = 5,
            y5 = 5)
    
    data_temp <- as.data.frame(mvtnorm::rmvnorm(sample_size, 
                                                mean = mu, 
                                                sigma = Omega))
    
    y <- c(t(data_temp))
    
    datasets[[i]] <- data.frame(y = y, 
                                id = rep(1:sample_size, 
                                         each = n_rep))
    
  }
  
  
  #----------------------------------------------------------------
  
  # elementos mcglm
  
  form = y~1
  link <- c("identity")
  variance <- c("constant")
  
  # preditor matricial
  Z0 <- mc_id(datasets[[1]]) # matriz identidade para o preditor matricial
  Z1 <- mc_mixed(~0 + as.factor(id), data = datasets[[1]])
  
  #----------------------------------------------------------------
  
  # ajuste de um modelo por conjunto de dados
  
  models <- list()
  
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
    
    hyp_taus <- hyp_taus - (taus/n_distances)
    
    hypothesis[[i]] <- paste(coef(models[[1]], type = 'tau')$Parameters,
                             '=',
                             hyp_taus)
    
    dists[[i]] <- dist(rbind(taus, hyp_taus), method = "euclidean")
  }
  
  #----------------------------------------------------------------
  # Dividindo as distâncias pelo desvio padrão das distâncias para
  # independente dos betas elas estarem no mesmo intervalo
  
  dists <- dists/sd(dists)
  
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
  
  
  #  return(results)
  #}
  
  #----------------------------------------------------------------