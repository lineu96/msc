#----------------------------------------------------------------

# Análise gráfica do percentual de rejeições para cada distância

#----------------------------------------------------------------

grafico2 <- function(df, main){
  
  # percentual de rejeições para cada distancia
  rej <- ifelse(df[,1:(ncol(df)-1)] < 0.05, 1, 0)
  
  df_final <- data.frame(dist = df$dist,
                         rej = (rowSums(rej)/(ncol(df)-1))*100)
  
  plot(rej~dist, df_final, type = 'o', lwd = 2, pch = 19,
       xlab = 'Distância', ylab = 'Rejeições',
       ylim = c(0,100),
       main = main)
  
  abline(v=subset(df_final, rej == 100)[1,]$dist, 
         lty = 2, col = 2, pch = 3)
  
  abline(h=5, 
         lty = 2, col = 2, pch = 3)
  
}

#----------------------------------------------------------------