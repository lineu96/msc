#---------------------------------------------------------------
# BOOTSTRAP PARA AVALIAÇÃO DOS RESÍDUOS
#---------------------------------------------------------------

# Lista para armazenar resíduos de cada reamostra

res_pred_boot <- list()

# Reamostras
for (i in 1:1100) {
  
  print(i)
  
  # Índices
  index <- sample(nrow(dados2), replace = T)
  
  # Dados
  data_boot <- dados2[index,]
  
  # Ajuste
  try(
    
    fit_boot <- 
      mcglm(linear_pred = c(form.min_lat,
                            form.max_lat),
            matrix_pred = list(c(Z0),
                               c(Z0)),
            link = c("log", "log"),
            variance = c("poisson_tweedie", "poisson_tweedie"), 
            control_algorithm = 
              list(#verbose = T, 
              tuning = 0.1,
              max_iter = 6000,
              tol = 1e-1),
            power_fixed = c(F,F),
            data = data_boot)
    
    )
  
  # Resíduos
  
  ## chol(vcov) inversa
  chol_inv <- Matrix::chol(fit_boot$inv_C)
  
  ## Resíduos empilhados
  residuos <- as.numeric(residuals(fit_boot, type = 'raw'))
  
  ## Produto matricial
  pearson <- as.numeric(chol_inv%*%residuos)
  
  ## Preditos
  preditos <- fit$fitted
  
  ## Dataframe
  res_pred_boot[[i]] <- data.frame(index = rep(1:nrow(data_boot),2),
                                   resp = c(rep('min_lat', nrow(data_boot)),
                                            rep('max_lat', nrow(data_boot))),
                                   observado = c(dados2$min_lat,
                                                 dados2$max_lat),
                                   preditos = preditos,
                                   pearson = pearson,
                                   raw = residuos
  )  
}

# Gráfico

x11()

par(mfrow = c(1,2))

# Latencia minima

temp <- subset(res_pred_boot[[1]], resp == 'min_lat')

for (i in 2:length(res_pred_boot)) {
  temp <- rbind(temp,
                subset(res_pred_boot[[i]], resp == 'min_lat'))
  
}

plot(pearson~preditos, subset(res_pred, resp == 'min_lat'), 
     pch=20, xlab = 'Preditos', ylab = 'Pearson', main = 'Min. Lat.',
     col = 'white', las =T, ylim = c(-3,6.2))

points(pearson~preditos, 
       temp, 
       col='#BDBDBD', 
       pch = 15) 

points(pearson~preditos, subset(res_pred, resp == 'min_lat'), pch=20)

abline(h = quantile(temp$pearson, probs = c(0.025, 0.975)),
       lwd = 2, lty = 2, col = 2)

# Latencia maxima

temp2 <- subset(res_pred_boot[[1]], resp == 'max_lat')

for (i in 2:length(res_pred_boot)) {
  temp2 <- rbind(temp2,
                 subset(res_pred_boot[[i]], resp == 'max_lat'))
  
}

plot(pearson~preditos, subset(res_pred, resp == 'max_lat'), 
     pch=20, xlab = 'Preditos', ylab = 'Pearson', main = 'Max. Lat.',
     col = 'white', las =T, ylim = c(-3,6.2))

points(pearson~preditos, 
       temp2, 
       col='#BDBDBD', 
       pch = 15) 

points(pearson~preditos, subset(res_pred, resp == 'max_lat'), pch=20)

abline(h = quantile(temp2$pearson, probs = c(0.025, 0.975)),
       lwd = 2, lty = 2, col = 2)
