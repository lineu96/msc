#-------------------------------------------------------------------
# Análise TG Bruno Tissei
#-------------------------------------------------------------------

# GRÁFICOS - ANÁLISE BI E MULTIVARIADAS
# LATENCIAS EM FUNÇÃO DE PORTAS
# SUBSET DE LATENCIAS
#-------------------------------------------------------------------

scat2 <- function(porta){

  dados_min <- subset(dados, dados$min_lat < 21)
  nrow(dados) - nrow(dados_min)
  
  dados_max <- subset(dados, dados$max_lat < 40)
  nrow(dados) - nrow(dados_max)
  
  g1 <-   ggplot(data = dados_min, 
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
  
  g2 <-   ggplot(data = dados_max, 
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



scat2('p0156')
scat2('p06')
scat2('p23')
scat2('p237')
scat2('p4')
scat2('p1')
scat2('p15')
scat2('p015')
scat2('p5')
scat2('p0')
scat2('p05')
scat2('p01')
scat2('p6')


# Correlograma

numericas1 <- dados_min[,c(7:17,20,21)]

names(numericas1)[c(12,13)] <- c('min', 'max')

cor1 <- cor(numericas1, method = 'pearson')

corrplot::corrplot.mixed(cor1, 
                         upper = 'ellipse',
                         tl.pos = "d"#,
                         #diag = 'n'
)


numericas2 <- dados_max[,c(7:16,20,21)]

names(numericas2)[c(11,12)] <- c('min', 'max')

cor2 <- cor(numericas2, method = 'pearson')

corrplot::corrplot.mixed(cor2, 
                         upper = 'ellipse',
                         tl.pos = "d"#,
                         #diag = 'n'
)
