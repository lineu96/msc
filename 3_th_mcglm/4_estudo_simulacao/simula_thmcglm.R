#----------------------------------------------------------------
# Função para estudo de simulação teste wald para mcglm
#----------------------------------------------------------------
# Simula de um modelo, varia hipótese
#----------------------------------------------------------------

# Considera um modelo em que há uma variável explicativa categórica

# Simula conjuntos de dados para o caso em que beta0 é igual a um 
# valor e os demais betas são 0

# Testa a hipótese de que os betas são iguais aos betas simulados

# Altera a hipótese: faz um decréscimo em beta 0, distribui esse
# decrécimo igualmente entre os demais betas

# Repete este procedimento algumas vezes

# Para cada uma das vezes toma a distância euclideana do vetor de 
# betas original para o modificado

# Para cada ponto avalia quantas vezes houve rejeição da hipótese
# nula

# O resultado gera uma curva que mostra a partir de que distância
# o teste funciona perfeitamente

#----------------------------------------------------------------

# Argumentos

# sample_size - tamanho das amostras
# n_datasets - numero de conjuntos de dados
# n_treatment - número de tratamentos
# betas - valores dos parametros de regressao
# n_distances - número de distâncias (vai definir decréscimo em beta 0
#                               para distribuição nos demais betas)
# distribution - distribuição (normal, poisson, binomial n=1, beta)

#----------------------------------------------------------------

simula <- function(sample_size = 50,
                           n_datasets = 200,
                           n_treatment = 4,
                           betas = c(0.5,0,0,0),
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
           variance <- c("constant")
           
           for (i in 1:n_datasets) {
             
             mu <- X%*%betas
             
             y <- rnorm(sample_size, mean = mu, sd = 1)
             
             datasets[[i]] <- data.frame(y = y,
                                         x = trat)
           }
         },
         
         "poisson" = {
           
           link <- c("log")
           variance <- c("tweedie")
           
           for (i in 1:n_datasets) {
             
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
           
           for (i in 1:n_datasets) {
             
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
           
           for (i in 1:n_datasets) {
             
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
           for (i in 1:n_datasets) {
             fit <- 
               mcglm(linear_pred = c(form),
                     matrix_pred = list(c(Z0)),
                     link = link, 
                     variance = variance,
                     Ntrial = list(1),
                     data = datasets[[i]])
             
             models[[i]] <- fit
             print(i)
           }
         },
         {
           for (i in 1:n_datasets) {
             fit <- 
               mcglm(linear_pred = c(form),
                     matrix_pred = list(c(Z0)),
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
  
  hyp_betas <- betas # vetor inicial para distribuir os efeitos
  
  hypothesis <- list() # vetor para armazenar as hipoteses
  
  # hipotese inicial
  hypothesis[[1]] <- paste(coef(models[[1]], type = 'beta')$Parameters,
                           '=', 
                           betas)
  
  # obtenção das distâncias e hipóteses a serem testadas
  for (i in 2:n_distances) {
    
    hyp_betas[1] <- hyp_betas[1] - (betas[1]/n_distances)
    hyp_betas[2:length(betas)] <- hyp_betas[2:length(betas)] + (betas[1]/n_distances)/(n_treatment-1)
    
    hypothesis[[i]] <- paste(coef(models[[1]], type = 'beta')$Parameters,
                             '=',
                             hyp_betas)
    
    dists[[i]] <- dist(rbind(betas, hyp_betas), method = "euclidean")
  }
  
  #----------------------------------------------------------------
  # Dividindo as distâncias pelo desvio padrão das distâncias para
  # independente dos betas elas estarem no mesmo intervalo
  
  dists <- dists/sd(dists)
  
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
  
  df_final$distribution <- distribution
  df_final$sample_size <- sample_size
  df_final$n_datasets <- (ncol(p_test[ , colSums(is.na(p_test)) == 0])-1)
  
  #----------------------------------------------------------------
  
  # retorna dataframe com o percentual de rejeição para cada hipótese
  return(df_final)
}

#----------------------------------------------------------------

