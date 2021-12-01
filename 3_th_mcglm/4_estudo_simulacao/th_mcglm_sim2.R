#----------------------------------------------------------------
# Função para estudo de simulação teste wald para mcglm
#----------------------------------------------------------------
# Varia o modelo, testa mesma hipótese
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
# distribution - distribuição (normal, poisson, binomial n=10, beta)

#----------------------------------------------------------------

th_mcglm_sim2 <- function(sample_size = 25,
                         n_datasets = 10,
                         n_treatment = 4,
                         betas_iniciais = c(5,0,0,0),
                         n_distances = 20,
                         distribution = 'binomial')
  
{

  
  betas <- list() # lista para armazenar os betas para gerar datasets
  betas[[1]] <- betas_iniciais # primeiro elemento igual aos betas iniciais
  
  dists <- vector() # vetor para armazenar as distancias
  dists[1] <- 0 # distancia inicial 0 
  
  hyp_betas <- betas_iniciais # valor inicial para distribuição de efeitos
  
  #----------------------------------------------------------------
  
  # obtenção dos betas para simular
  
  for (i in 2:n_distances) {
    
    hyp_betas[1] <- hyp_betas[1] - (betas_iniciais[1]/n_distances)
    hyp_betas[2:length(betas_iniciais)] <- hyp_betas[2:length(betas_iniciais)] + (betas_iniciais[1]/n_distances)/(n_treatment-1)
    
    betas[[i]] <- hyp_betas
    
    dists[[i]] <- dist(rbind(betas_iniciais, hyp_betas), method = "euclidean")
  } 
  
  #----------------------------------------------------------------
  
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
           
           for (j in 1:length(betas)) {
             
             datasets[[j]] <- list()
             length(datasets[[j]]) <- n_datasets
             
             for (i in 1:n_datasets) {
               
               mu <- X%*%betas[[j]]
               
               y <- rnorm(sample_size, mean = mu, sd = 1)
               
               datasets[[j]][[i]] <- data.frame(y = y,
                                                x = trat)
             }  
           }
         },
         
         "poisson" = {
           
           link <- c("log")
           variance <- c("tweedie")
           
           for (j in 1:length(betas)) {
             
             datasets[[j]] <- list()
             length(datasets[[j]]) <- n_datasets
             
             for (i in 1:n_datasets) {
               
               lambda <- exp(X%*%betas[[j]])
               
               y <- rpois(sample_size, 
                          lambda = lambda)
               
               datasets[[j]][[i]] <- data.frame(y = y,
                                                x = trat)
             }  
           }
         },
         
         "binomial" = {
           
           link <-  "logit" 
           variance  <-  "binomialP"
           
           for (j in 1:length(betas)) {
             
             datasets[[j]] <- list()
             length(datasets[[j]]) <- n_datasets
             
             for (i in 1:n_datasets) {
               
               p <- exp(X%*%betas[[j]])/(1 + exp(X%*%betas[[j]]))
               
               y <- rbinom(sample_size, 
                           p = p,
                           size = 10)
               
               datasets[[j]][[i]] <- data.frame(y = y,
                                                x = trat)
             }  
           }
         },
         
         "beta" = {
           
           link <- "logit"
           
           variance <- "binomialP"
           
           for (j in 1:length(betas)) {
             
             datasets[[j]] <- list()
             length(datasets[[j]]) <- n_datasets
             
             for (i in 1:n_datasets) {
               
               p <- exp(X%*%betas[[j]])/(1 + exp(X%*%betas[[j]]))
               
               y <- gamlss.dist::rBE(sample_size, 
                                     mu = p, 
                                     sigma = 0.2)
               
               datasets[[j]][[i]] <- data.frame(y = y,
                                                x = trat)
             }  
           }
         }
  )
  
  #----------------------------------------------------------------
  
  # elementos mcglm
  
  # caso seja binomial a resposta precisa ser declarada como razão
  # y/Ntrial ~x
  
  switch(distribution,
         "binomial" = {form = y/10~x},
         {form = y~x}
  )
  
  # preditor matricial
  Z0 <- mc_id(datasets[[1]][[1]]) # matriz identidade para o preditor matricial
  
  #----------------------------------------------------------------
  
  models <- list()
  
  switch(distribution,
         "binomial" = {
           for (j in 1:length(betas)) {
             
             models[[j]] <- list()
             length(models[[j]]) <- n_datasets
             
             for (i in 1:n_datasets) {
               
               fit <- 
                 mcglm(linear_pred = c(form),
                       matrix_pred = list(c(Z0)),
                       link = link,
                       variance = variance,
                       Ntrial = list(10),
                       data = datasets[[j]][[i]])
               
               models[[j]][[i]] <- fit
               print(paste(j,i))
               
             }  
           }
         },
         {
           for (j in 1:length(betas)) {
             
             models[[j]] <- list()
             length(models[[j]]) <- n_datasets
             
             for (i in 1:n_datasets) {
               
               fit <- 
                 mcglm(linear_pred = c(form),
                       matrix_pred = list(c(Z0)),
                       link = link,
                       variance = variance,
                       data = datasets[[j]][[i]])
               
               models[[j]][[i]] <- fit
               print(paste(j,i))
               
             }  
           }
         }
  )
  
  #----------------------------------------------------------------

  hypothesis <- paste(coef(models[[1]][[1]], type = 'beta')$Parameters,
                           '=',
                           betas_iniciais)
  
  #----------------------------------------------------------------
  
  p_test <- matrix(nrow = length(betas), 
                   ncol = n_datasets)
  
  for (j in 1:length(betas)) {
    for (i in 1:n_datasets) {
      
      p_test[j,i] <-  mc_linear_hypothesis(object =  models[[j]][[i]], 
                                           hypothesis = hypothesis)$P_valor
      
    }  
  }
  
  #----------------------------------------------------------------
  
  # converte resultado para dataframe
  p_test <- as.data.frame(p_test)
  
  #----------------------------------------------------------------
  
  # acrescenta info de distancia
  p_test$dist <- dists
  
  #----------------------------------------------------------------
  
  # obtém percentual de rejeição
  
  rej <- ifelse(p_test[,1:(ncol(p_test)-1)] < 0.05, 1, 0)
  
  df_final <- data.frame(dist = p_test$dist,
                         rej = (rowSums(rej)/(ncol(p_test)-1)*100))
  
  #----------------------------------------------------------------
  
  # retorna dataframe com o percentual de rejeição para cada hipótese
  return(df_final)
}

#----------------------------------------------------------------
