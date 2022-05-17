
#----------------------------------------------------------------

# elementos mcglm

form1 = y1~1
form2 = y2~1
form3 = y3/1~1

link_normal <- "identity"
variance_normal <- "constant"

link_poisson <- "log"
variance_poisson <- "tweedie"

link_binomial <- "logit"
variance_binomial <- "binomialP"

# preditor matricial
Z0 <- mc_id(datasets[[1]]) # matriz identidade para o preditor matricial
Z1 <- mc_mixed(~0 + as.factor(id), data = datasets[[1]])

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
    mcglm(linear_pred = c(form1, 
                          form2, 
                          form3),
          matrix_pred = list(c(Z0, Z1),
                             c(Z0, Z1),
                             c(Z0, Z1)),
          link = c(link_normal, 
                   link_poisson, 
                   link_binomial), 
          variance = c(variance_normal, 
                       variance_poisson, 
                       variance_binomial),
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

df_final$distribution <- 'normal/poisson/binomial'
df_final$sample_size <- sample_size
df_final$n_datasets <- ncol(rej)

#----------------------------------------------------------------

# retorna dataframe com o percentual de rejeição para cada hipótese
return(list(hypothesis = hypothesis, 
            parameters = parameters, 
            vcovs = vcovs, 
            p_test = p_test,
            df_final = df_final))

#----------------------------------------------------------------

save(tri_n50, file = 'resultados_tri_n50.RData')

#----------------------------------------------------------------