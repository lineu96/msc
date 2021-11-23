# analises

grafico <- function(df){
  
  #par(mfrow = c(1,3))
  par(mfrow = c(1,2))
  
  # p-valor para cada dataset em cada distancia
  #plot(df$dist, df[,1],type = 'l', col = 1,
  #     #xlim = c(0,3),
  #     ylim = c(0,1),
  #     xlab = 'Distancia',
  #     ylab = 'p-valor',
  #     main = 'P-valor para cada \n dataset em cada \nhipotese')
  
  #for (i in 1:(ncol(df)-1)) {
  #  lines(df$dist, df[,i], col = i)  
  #}
  
  #----------------------------------------------------------------
  
  # percentual de rejeições/nao rejeições para cada distancia
  n_rej <- ifelse(df[,1:(ncol(df)-1)] > 0.05, 1, 0)
  rej <- ifelse(df[,1:(ncol(df)-1)] < 0.05, 1, 0)
  
  df_final <- data.frame(dist = df$dist,
                         rej = (rowSums(rej)/(ncol(df)-1))*100,
                         n_rej = (rowSums(n_rej)/(ncol(df)-1))*100)
  
  # a partir de que distancia o teste acerta
  # subset(df_final, rej == 0 & n_rej == 100)[1,]
  
  # hipotese associada a esta distancia
  # hypothesis[134]
  
  # graficos
  plot(rej~dist, df_final, type = 'l', lwd = 2,
       xlab = 'Distancia', ylab = 'Rejeições',
       ylim = c(0,100),
       main = '% Rejeição para \n cada distância')
  
  abline(v=subset(df_final, rej == 100 & n_rej == 0)[1,]$dist, 
         lty = 2, col = 2, pch = 3)
  
  plot(n_rej~dist, df_final, type = 'l', lwd = 2, 
       xlab = 'Distancia', ylab = 'Ñ rejeicoes',
       ylim = c(0,100),
       main = '% Não rejeição para \n cada distância')
  
  abline(v=subset(df_final, rej == 100 & n_rej == 0)[1,]$dist,
         lty = 2, col = 2, pch = 3)
  
}

#----------------------------------------------------------------