
simula_tri <- function(sample_size = 50,
                       n_treatment = 4,
                       betas_normal = c(5,0,0,0),
                       betas_poisson = c(2.3,0,0,0),
                       betas_binomial = c(0.5,0,0,0),
                       n_datasets = 500,
                       n_distances = 20){
  
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
  
  for (i in 1:n_datasets) {
    
    # Argumentos das marginais
    mu <- X%*%betas_normal
    lambda <- exp(X%*%betas_poisson)
    p <- exp(X%*%betas_binomial)/(1 + exp(X%*%betas_binomial))
    
    paramslists <- list(
      m1 = list(mean = mu, sd = 1),
      m2 = list(lambda = lambda),
      m3 = list(p = p, size = 1)
    )
    
    y <- NORTARA::genNORTARA(sample_size,
                             cor_matrix,
                             invcdfnames,
                             paramslists)
    
    datasets[[i]] <- data.frame(y = y,
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
  
  # McGLM
  
  form1 = y1~x
  form2 = y2~x
  form3 = y3~x
  
  link_normal <- "identity"
  variance_normal <- "constant"
  
  link_poisson <- "log"
  variance_poisson <- "tweedie"
  
  link_binomial <- "logit"
  variance_binomial <- "binomialP"
  
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
  
  for (i in 1:n_datasets) {
    fit <- 
      mcglm(linear_pred = c(form1, form2, form3),
            matrix_pred = list(Z0,Z0,Z0),
            link = c(link_normal, link_poisson, link_binomial), 
            variance = c(variance_normal, variance_poisson, variance_binomial),
            data = datasets[[i]],
            control_algorithm = ca)
    
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
  hyp_betas_binomial <- betas_binomial # vetor inicial para distribuir os efeitos
  
  hypothesis <- list() # vetor para armazenar as hipoteses
  
  # hipotese inicial
  hypothesis[[1]] <- paste(coef(models[[1]], type = 'beta')$Parameters,
                           '=', 
                           c(betas_normal, 
                             betas_poisson, 
                             betas_binomial))
  
  # obtenção das distâncias e hipóteses a serem testadas
  for (i in 2:n_distances) {
    
    hyp_betas_normal[1] <- hyp_betas_normal[1] - (betas_normal[1]/n_distances)
    hyp_betas_normal[2:length(betas_normal)] <- hyp_betas_normal[2:length(betas_normal)] + (betas_normal[1]/n_distances)/(n_treatment-1)
    
    hyp_betas_poisson[1] <- hyp_betas_poisson[1] - (betas_poisson[1]/n_distances)
    hyp_betas_poisson[2:length(betas_poisson)] <- hyp_betas_poisson[2:length(betas_poisson)] + (betas_poisson[1]/n_distances)/(n_treatment-1)
    
    hyp_betas_binomial[1] <- hyp_betas_binomial[1] - (betas_binomial[1]/n_distances)
    hyp_betas_binomial[2:length(betas_binomial)] <- hyp_betas_binomial[2:length(betas_binomial)] + (betas_binomial[1]/n_distances)/(n_treatment-1)
    
    hypothesis[[i]] <- paste(coef(models[[1]], type = 'beta')$Parameters,
                             '=',
                             c(hyp_betas_normal,
                               hyp_betas_poisson,
                               hyp_betas_binomial))
    
    dists[[i]] <- dist(rbind(betas_normal, 
                             hyp_betas_normal), 
                       method = "euclidean")
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
  
  df_final$distribution <- 'normal/poisson/binomial'
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
