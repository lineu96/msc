simula_tri_long_normal <- function(sample_size = 250,
                                       n_datasets = 100,
                                       n_rep = 5,
                                       taus = c(0.5,0.5),
                                       n_distances = 20,
                                       distribution = 'normal')
  
{
  
  ## Matrix linear predictor
  UM <- rep(1, n_rep)
  Z0 <- Diagonal(n_rep, 1)
  Z1 <- UM%*%t(UM)
  
  Omega <- mc_matrix_linear_predictor(tau = taus, 
                                      Z = list(Z0, Z1))
  
  Omega1 <- as.matrix(Omega, n_rep, n_rep)
  Omega2 <- as.matrix(Omega, n_rep, n_rep)
  Omega3 <- as.matrix(Omega, n_rep, n_rep)
  
  chol_1 <- chol(Omega1)
  chol_2 <- chol(Omega2)
  chol_3 <- chol(Omega3)
  
  BB <- bdiag(chol_1, chol_2, chol_3)
  
  Sigma_b <- Matrix(c(1.0,  0.75, 0.5,
                      0.75,  1.0, 0.25,
                      0.5,  0.25, 1.0), 
                    3, 3)
  
  I <- Diagonal(5, 1)
  C <- t(BB)%*%kronecker(Sigma_b, I)%*%BB
  C <- as.matrix(C, (n_rep*3), (n_rep*3))
  
  ## Marginais
  mu <- c(y1 = 5, 
          y2 = 5, 
          y3 = 5,
          y4 = 5,
          y5 = 5,
          y6 = 5, 
          y7 = 5, 
          y8 = 5,
          y9 = 5,
          y10 = 5,
          y11 = 5, 
          y12 = 5, 
          y13 = 5,
          y14 = 5,
          y15 = 5)

  # lista para armazenar os conjuntos de dados
  datasets <- list()
  
  # geração dos conjuntos de dados
  
  for (i in 1:(n_datasets)) {
    
    data_temp <- as.data.frame(mvtnorm::rmvnorm(sample_size, 
                                                mean = mu, 
                                                sigma = C))
    
    y1 <- c(t(data_temp[,1:5]))
    y2 <- c(t(data_temp[,6:10]))
    y3 <- c(t(data_temp[,11:15]))
    
    data <- data.frame(y1 = y1,
                       y2 = y2,
                       y3 = y3,
                       id = rep(1:sample_size, 
                                  each = n_rep))
    
    datasets[[i]] <- data.frame(y1 = y1,
                                y2 = y2,
                                y3 = y3,
                                id = rep(1:sample_size, 
                                         each = n_rep))
    
  }
  
  
  #----------------------------------------------------------------
  
  # elementos mcglm
  
  # caso seja binomial a resposta precisa ser declarada como razão
  # y/Ntrial ~x
  
  form1 = y1~1
  form2 = y2~1
  form3 = y3~1
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
      mcglm(linear_pred = c(form1,
                            form2,
                            form3),
            matrix_pred = list(c(Z0, Z1),
                               c(Z0, Z1),
                               c(Z0, Z1)),
            link = c(link,link,link), 
            variance = c(variance,variance,variance),
            data = datasets[[i]],
            control_algorithm = list(#verbose = T,
              tuning = 1,
              max_iter = 100,
              tol = 0.05))
    
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
                                              hypothesis = hypothesis[[j]])$P_valor)
    }
  }
  
  #----------------------------------------------------------------
  
  # converte resultado para dataframe
  p_test <- as.data.frame(p_test)
  
  #----------------------------------------------------------------
  
  # obtém percentual de rejeição
  
  rej <- ifelse(p_test[,1:(ncol(p_test))] < 0.05, 1, 0)
  
  df_final <- data.frame(dist = dists,
                         rej = ((rowSums(rej))/ncol(rej))*100)
  
  df_final$distribution <- paste('tri', distribution)
  df_final$sample_size <- sample_size
  df_final$n_datasets <- ncol(rej)
  
  #----------------------------------------------------------------
  
  # retorna dataframe com o percentual de rejeição para cada hipótese
  return(list(hypothesis = hypothesis, 
              parameters = parameters, 
              vcovs = vcovs, 
              p_test = p_test,
              df_final = df_final))
}

#----------------------------------------------------------------

