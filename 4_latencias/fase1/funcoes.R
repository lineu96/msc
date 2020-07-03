#-------------------------------------------------------------------
# Análise TG Bruno Tissei
#-------------------------------------------------------------------

# FUNÇÕES ÚTEIS
#-------------------------------------------------------------------

# Tabelas de frequencia
tb_freq <- function(vetor){
  aux <- factor(vetor, 
                levels = 0:max(vetor))
  
  freq <- table(aux)
  freq_r <- round(prop.table(freq),3) # frequência relativa
  
  table <- data.frame(Niveis = names(freq),
                      Frequencia = as.vector(freq),
                      Freq_relativa = as.vector(freq_r))
  
  return(table)
  
}

#-------------------------------------------------------------------

# Medidas de posição
posic <- function(vetor){
  data.frame( 
    Mínimo = quantile(vetor)[1],
    Quartil_1 = quantile(vetor)[2],
    Média = round(mean(vetor),2),
    Mediana = quantile(vetor)[3],
    Moda = names(sort(table(vetor), 
                      decreasing = TRUE)[1]),
    Quartil_3 = quantile(vetor)[4],
    Máximo = quantile(vetor)[5]
  )
}

#-------------------------------------------------------------------

# Medidas de dispersão
disp <- function(vetor){
  data.frame(Amplitude = diff(range(vetor)),
             Variância = round(var(vetor), 2),
             Desvio_padrão = round(sd(vetor), 2),
             Coef_variação = paste0(round(100*sd(vetor)/mean(vetor), 2), "%"))
}

#-------------------------------------------------------------------

# Gráfico de setores
setores <- function(table, title = 'title'){
  
  tb <- as.data.frame(table)
  
  tb <- tb %>% 
    arrange(desc(Var1)) %>%
    mutate(prop = Freq / sum(tb$Freq) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  
  ggplot(tb, aes(x="", y=prop, fill=Var1)) +
    geom_bar(stat="identity", 
             width=1, 
             color="white",
             alpha = 0.8) +
    coord_polar("y", start=0) +
    ggtitle(title)+
    theme_bw() + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.title = element_blank())+
    
    geom_text(aes(y = ypos, label = Freq), 
              color = "white", size=6) +
    scale_fill_brewer(palette="Set1")
  
}

#-------------------------------------------------------------------

# Histogramas e densidades
barras <- function(df,var,n_col){
  
  # g1 <- ggplot(data = df, 
  #              aes_string(x = var))+
  #   geom_density(col = 1,
  #                fill = 1,
  #                alpha = 0.6) + 
  #   theme_bw() +
  #   xlab('')+ ylab('Densidade') + ggtitle('Com 0') +
  #   theme_bw()
  # 
  # g2 <- ggplot(data = subset(df, df[,n_col] > 0), 
  #              aes_string(x = var))+
  #   geom_density(col = 1,
  #                fill = 1,
  #                alpha = 0.6) + 
  #   
  #   theme_bw() + ggtitle('Sem 0') +
  #   xlab('')+ ylab('') +theme_bw()
  
  
  g3 <- ggplot(data = df, 
               aes_string(x = var))+
    geom_bar(col = 1,
                   fill = 1,
                   alpha = 0.6) +
    ggtitle('Com 0') +
    xlab('Valores')+ ylab('Frequência') + theme_bw()
  
  g4 <- ggplot(data = subset(df, df[,n_col] > 0), 
               aes_string(x = var))+
    geom_bar(col = 1,
                   fill = 1,
                   alpha = 0.6) +
    ggtitle('Sem 0') +
    xlab('Valores') + ylab('') +theme_bw()
  
  library(gridExtra)
  grid.arrange(#g1,g2,
               g3,g4,
               nrow=1,ncol=2, 
               top = var
  )  
  
}

#-------------------------------------------------------------------

# Gráficos de dispersão

scat <- function(porta){
  
  g1 <-   ggplot(data = dados, 
                 mapping = aes_string(x=porta, 
                                      y='min_lat')) +
    geom_point() + 
    #geom_smooth(se=F, 
    #            linetype = 
    #              'longdash', 
    #            col = '#2EFE64') + 
    geom_smooth(method = 'lm', 
                se = F, 
                col= 2)+ theme_bw()+
    theme(legend.title = element_blank())+
    xlab(porta) + ylab('Latência mínima') + ggtitle('')  
  
  g2 <-   ggplot(data = dados, 
                 mapping = aes_string(x=porta, 
                                      y='max_lat')) +
    geom_point() + 
    #geom_smooth(se=F, 
    #            linetype = 
    #              'longdash', 
    #            col = '#2EFE64') + 
    geom_smooth(method = 'lm', 
                se = F, 
                col= 2)+ theme_bw()+
    theme(legend.title = element_blank())+
    xlab(porta) + ylab('Latência máxima') + ggtitle('')  
  
  
  
  ggarrange(g1,g2,
            nrow = 1, ncol = 2, 
            common.legend = TRUE, 
            legend="bottom",
            #labels = paste0(porta, ' em função das latências'),
            font.label = list(size = 15))
  
}

#-------------------------------------------------------------------