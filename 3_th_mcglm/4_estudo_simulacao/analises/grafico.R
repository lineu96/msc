#----------------------------------------------------------------

# Análise gráfica do percentual de rejeições para cada distância

#----------------------------------------------------------------

grafico <- function(df, main){
  
  plot(rej~dist, df, type = 'o', lwd = 2, pch = 19,
       xlab = 'Distância', ylab = 'Rejeições',
       ylim = c(0,100),
       main = main)
  
  abline(v=subset(df, rej == 100)[1,]$dist, 
         lty = 2, col = 2, pch = 3)
  
  abline(h=5, 
         lty = 2, col = 2, pch = 3)
  
}

#----------------------------------------------------------------