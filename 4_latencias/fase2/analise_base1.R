#---------------------------------------------------------------
library(tidyverse)
#---------------------------------------------------------------

# Leitura
dados <- read.csv2('latencias_1.csv', header = T, sep = ',')

#---------------------------------------------------------------

# Seleção das variaveis de interesse
dados <- select(dados, n, inst, lock, segment,
                p0156, p06, p23,p237, p4, p1, p15, p0, p05,
                min_lat, max_lat )

#---------------------------------------------------------------

# Transformando portas em binária
dados$p0 <- ifelse(dados$p0 > 0, 1,0)
dados$p237 <- ifelse(dados$p237 > 0, 1,0)

#---------------------------------------------------------------


# Transformando segment em binária
dados$segment <- ifelse(dados$segment > 0, 1,0)

#---------------------------------------------------------------

# Divisão da base

# latência mínima entre 0 e 25 &
# latência máxima entre 0 e 45

indices <- subset(dados, dados$min_lat<=25 & dados$max_lat<=45)$n

massa <- subset(dados, dados$n %in% indices)
analista <- subset(dados, !(dados$n %in% indices))

analista

#---------------------------------------------------------------

# Conferindo tamanhos
nrow(massa)
nrow(analista)

(nrow(massa) + nrow(analista)) == nrow(dados)

#---------------------------------------------------------------

# Função para geração de tabela de frequência

tb_freq <- function(vetor){
  
  tabela <- as.data.frame(table(vetor)) # frequência absoluta
  
  tabela$Freq_relativa <- round(prop.table(tabela$Freq),3) # frequência relativa
  
  names(tabela)[1] <- 'Niveis'
  
  return(tabela)
}

#---------------------------------------------------------------

# Tabelas de frequencia para as portas

pander::pander(tb_freq(massa$lock))
#pander::pander(tb_freq(massa$segment)) SO 0 NO SUBSET
pander::pander(tb_freq(massa$p0156))
pander::pander(tb_freq(massa$p06))
pander::pander(tb_freq(massa$p23))
pander::pander(tb_freq(massa$p237))
pander::pander(tb_freq(massa$p4))
pander::pander(tb_freq(massa$p1))
pander::pander(tb_freq(massa$p15))
pander::pander(tb_freq(massa$p0))
#pander::pander(tb_freq(massa$p05)) SO 0 NO SUBSET

#---------------------------------------------------------------

# Função para geração de tabela de frequência por classes

tb_freq2 <- function(vetor){
  h <- hist(vetor, plot = FALSE) #histograma
  
  breaks <- h$breaks #armazenando os breaks do histograma 
  
  Classes <- cut(vetor, breaks = breaks, 
                 include.lowest = TRUE, right = TRUE) #gerando classes
  
  tabela <- as.data.frame(table(Classes)) #gerando tabela com faixas
  
  tabela$Freq_relativa <- round(prop.table(tabela$Freq),3) # frequência relativa
  
  return(tabela)
}

#---------------------------------------------------------------

# Tabelas de frequencia para as latências

pander::pander(tb_freq2(massa$min_lat))
pander::pander(tb_freq2(massa$max_lat))

#---------------------------------------------------------------

# Função para tabela de medidas de posição

posicao <- function(vetor){
  posicao <- data.frame(Minimo = quantile(vetor)[1],
                        Quartil_1 = quantile(vetor)[2],
                        Media = round(mean(vetor), 3),
                        Mediana = quantile(vetor)[3],
                        Moda = names(sort(table(vetor), 
                                          decreasing = TRUE)[1]),
                        Quartil_3 = quantile(vetor)[4],
                        Maximo = quantile(vetor)[5])
  
  row.names(posicao) <- NULL
  
  return(posicao)  
  
}

#---------------------------------------------------------------

# Medidas de posição de todas as variáveis

m1 <- rbind(posicao(massa$lock),
            #posicao(massa$segment),
            posicao(massa$p0156),
            posicao(massa$p06),
            posicao(massa$p23),
            posicao(massa$p237),
            posicao(massa$p4),
            posicao(massa$p1),
            posicao(massa$p15),
            posicao(massa$p0),
            #posicao(massa$p05),
            posicao(massa$min_lat),
            posicao(massa$max_lat))

row.names(m1) <- c("lock", "p0156", "p06", "p23","p237", "p4", 
                   "p1", "p15","p0",
                   "min_lat","max_lat")

pander::pander(m1)

#---------------------------------------------------------------

# Função para tabela de medidas de dispersão

dispersao <- function(vetor){
  dispersao <- data.frame(Amplitude = diff(range(vetor)),
                          Variancia = round(var(vetor), 3),
                          Desvio_padrao = round(sd(vetor), 3))
  
  row.names(dispersao) <- NULL
  
  return(dispersao)  
  
}

#---------------------------------------------------------------

# Medidas de dispersão de todas as variáveis

m2 <- rbind(#dispersao(massa$lock),
            #dispersao(massa$segment),
            dispersao(massa$p0156),
            dispersao(massa$p06),
            dispersao(massa$p23),
            dispersao(massa$p237),
            dispersao(massa$p4),
            dispersao(massa$p1),
            dispersao(massa$p15),
            #dispersao(massa$p0),
            #dispersao(massa$p05),
            dispersao(massa$min_lat),
            dispersao(massa$max_lat))

row.names(m2) <- c(#"lock", 
                   "p0156", "p06", "p23", "p237", "p4", 
                   "p1", "p15",#"p0",
                   "min_lat","max_lat")

pander::pander(m2)

#---------------------------------------------------------------

# Gráficos de barras para todas as variáveis

b_lock <- ggplot(data = massa, 
                 mapping = aes(x = factor(lock))) + 
  geom_bar(alpha = 0.7) + theme_bw() +
  xlab('lock') + ylab('Contagem')

#b_segment <- ggplot(data = massa, 
#                    mapping = aes(x = factor(segment))) + 
#  geom_bar(alpha = 0.7) + theme_bw() +
#  xlab('segment') + ylab('Contagem')

b_0156 <- ggplot(data = massa, 
                 mapping = aes(x = factor(p0156))) + 
  geom_bar(alpha = 0.7) + theme_bw() +
  xlab('Porta 0156') + ylab('Contagem')

b_06 <- ggplot(data = massa, 
               mapping = aes(x = factor(p06))) + 
  geom_bar(alpha = 0.7) + theme_bw() +
  xlab('Porta 06') + ylab('Contagem')

b_23 <- ggplot(data = massa, 
               mapping = aes(x = factor(p23))) + 
  geom_bar(alpha = 0.7) + theme_bw() +
  xlab('Porta 23') + ylab('Contagem')

b_237 <- ggplot(data = massa, 
               mapping = aes(x = factor(p237))) + 
  geom_bar(alpha = 0.7) + theme_bw() +
  xlab('Porta 237') + ylab('Contagem')

b_4 <- ggplot(data = massa, 
              mapping = aes(x = factor(p4))) + 
  geom_bar(alpha = 0.7) + theme_bw() +
  xlab('Porta 4') + ylab('Contagem')

b_1 <- ggplot(data = massa, 
              mapping = aes(x = factor(p1))) + 
  geom_bar(alpha = 0.7) + theme_bw() +
  xlab('Porta 1') + ylab('Contagem')

b_15 <- ggplot(data = massa, 
               mapping = aes(x = factor(p15))) + 
  geom_bar(alpha = 0.7) + theme_bw() +
  xlab('Porta 15') + ylab('Contagem')

b_0 <- ggplot(data = massa, 
              mapping = aes(x = factor(p0))) + 
  geom_bar(alpha = 0.7) + theme_bw() +
  xlab('Porta 0') + ylab('Contagem')

#b_05 <- ggplot(data = massa, 
#               mapping = aes(x = factor(p05))) + 
#  geom_bar(alpha = 0.7) + theme_bw() +
#  xlab('Porta 05') + ylab('Contagem')

b_min <- ggplot(data = massa, 
                mapping = aes(x = factor(min_lat))) + 
  geom_bar(alpha = 0.7) + theme_bw() +
  xlab('Latência mínima') + ylab('Contagem')

b_max <- ggplot(data = massa, 
                mapping = aes(x = factor(max_lat))) + 
  geom_bar(alpha = 0.7) + theme_bw() +
  xlab('Latência máxima') + ylab('Contagem')


ggpubr::ggarrange(b_lock,
                  #b_segment,
                  b_0156,
                  b_06,
                  b_23,
                  b_237,
                  b_4,
                  b_1,
                  b_15,
                  b_0,
                  #b_05,
                  b_min,
                  b_max, 
                  nrow = 6,
                  ncol = 2)

#---------------------------------------------------------------

# Função para análise bivariada, latencias ~ variaveis

tab_bi <- function(latencia, variavel){
  
  tabela <- as.data.frame(table(variavel)) # frequência absoluta
  
  tabela$Latencia_media <- tapply(latencia,
                                  variavel,
                                  mean)
  
  tabela$Desvio_padrao <- tapply(latencia,
                                 variavel,
                                 sd)
  
  names(tabela)[c(1,2)] <- c('Niveis', 'n')
  
  tabela$Latencia_media <- round(tabela$Latencia_media, 3)
  tabela$Desvio_padrao <- round(tabela$Desvio_padrao, 3)
  
  tabela <- tabela[,c(1,3,4,2)]
  
  return(tabela)
}

#---------------------------------------------------------------

# Latencia minima em função de cada variavel

pander::pander(tab_bi(massa$min_lat,massa$lock))
#pander::pander(tab_bi(massa$min_lat,massa$segment)) SO 0 NO SUBSET
pander::pander(tab_bi(massa$min_lat,massa$p0156))
pander::pander(tab_bi(massa$min_lat,massa$p06))
pander::pander(tab_bi(massa$min_lat,massa$p23))
pander::pander(tab_bi(massa$min_lat,massa$p237))
pander::pander(tab_bi(massa$min_lat,massa$p4))
pander::pander(tab_bi(massa$min_lat,massa$p1))
pander::pander(tab_bi(massa$min_lat,massa$p15))
pander::pander(tab_bi(massa$min_lat,massa$p0))
#pander::pander(tab_bi(massa$min_lat,massa$p05)) SO 0 NO SUBSET


#---------------------------------------------------------------

# Latencia maxima em função de cada variavel

pander::pander(tab_bi(massa$max_lat,massa$lock))
#pander::pander(tab_bi(massa$max_lat,massa$segment)) SO 0 NO SUBSET
pander::pander(tab_bi(massa$max_lat,massa$p0156))
pander::pander(tab_bi(massa$max_lat,massa$p06))
pander::pander(tab_bi(massa$max_lat,massa$p23))
pander::pander(tab_bi(massa$max_lat,massa$p237))
pander::pander(tab_bi(massa$max_lat,massa$p4))
pander::pander(tab_bi(massa$max_lat,massa$p1))
pander::pander(tab_bi(massa$max_lat,massa$p15))
pander::pander(tab_bi(massa$max_lat,massa$p0))
#pander::pander(tab_bi(massa$max_lat,massa$p05)) SO 0 NO SUBSET

#---------------------------------------------------------------

# Função para gráficos de dispersão das latencias

scat1 <- function(porta){
  
  g1 <-   ggplot(data = massa, 
                 mapping = aes_string(x=porta, 
                                      y='min_lat')) +
    geom_point() + 
    #geom_smooth(se=F, 
    #            linetype = 
    #              'longdash', 
    #            col = '#2EFE64') + 
    geom_smooth(method = 'lm', 
                se = T, 
                col= 2)+ theme_bw()+
    theme(legend.title = element_blank())+
    xlab(porta) + ylab('Latência mínima') + ggtitle('')  
  
  g2 <-   ggplot(data = massa, 
                 mapping = aes_string(x=porta, 
                                      y='max_lat')) +
    geom_point() + 
    #geom_smooth(se=F, 
    #            linetype = 
    #              'longdash', 
    #            col = '#2EFE64') + 
    geom_smooth(method = 'lm', 
                se = T, 
                col= 2)+ theme_bw()+
    theme(legend.title = element_blank())+
    xlab(porta) + ylab('Latência máxima') + ggtitle('')  
  
  
  
  ggpubr::ggarrange(g1,g2,
                    nrow = 1, ncol = 2, 
                    common.legend = TRUE, 
                    legend="bottom",
                    #labels = paste0(porta, ' em função das latências'),
                    font.label = list(size = 15))
  
}

#---------------------------------------------------------------

# Gráficos de dispersão

g1 <-   ggplot(data = massa, 
               mapping = aes(x=factor(lock),
                             y=min_lat)) +
  geom_boxplot(fill = 1, alpha = 0.3) + 
  theme(legend.title = element_blank())+
  xlab('lock') + ylab('Latência mínima') + ggtitle('') +
  theme_bw()

g2 <-   ggplot(data = massa, 
               mapping = aes(x=factor(lock), 
                             y=max_lat)) +
  geom_boxplot(fill = 1, alpha = 0.3) + 
  theme(legend.title = element_blank())+
  xlab('lock') + ylab('Latência máxima') + ggtitle('')   +
  theme_bw()

ggpubr::ggarrange(g1,g2,
                  nrow = 1, ncol = 2, 
                  common.legend = TRUE, 
                  legend="bottom",
                  #labels = paste0(porta, ' em função das latências'),
                  font.label = list(size = 15))

#scat1(massa$segment)
scat1('p0156')
scat1('p06')
scat1('p23')

g1 <-   ggplot(data = massa, 
               mapping = aes(x=factor(p237),
                             y=min_lat)) +
  geom_boxplot(fill = 1, alpha = 0.3) + 
  theme(legend.title = element_blank())+
  xlab('p237') + ylab('Latência mínima') + ggtitle('') +
  theme_bw()

g2 <-   ggplot(data = massa, 
               mapping = aes(x=factor(p237), 
                             y=max_lat)) +
  geom_boxplot(fill = 1, alpha = 0.3) + 
  theme(legend.title = element_blank())+
  xlab('p237') + ylab('Latência máxima') + ggtitle('')   +
  theme_bw()

ggpubr::ggarrange(g1,g2,
                  nrow = 1, ncol = 2, 
                  common.legend = TRUE, 
                  legend="bottom",
                  #labels = paste0(porta, ' em função das latências'),
                  font.label = list(size = 15))


scat1('p4')
scat1('p1')
scat1('p15')

g1 <-   ggplot(data = massa, 
               mapping = aes(x=factor(p0),
                             y=min_lat)) +
  geom_boxplot(fill = 1, alpha = 0.3) + 
  theme(legend.title = element_blank())+
  xlab('p0') + ylab('Latência mínima') + ggtitle('') +
  theme_bw()

g2 <-   ggplot(data = massa, 
               mapping = aes(x=factor(p0), 
                             y=max_lat)) +
  geom_boxplot(fill = 1, alpha = 0.3) + 
  theme(legend.title = element_blank())+
  xlab('p0') + ylab('Latência máxima') + ggtitle('')   +
  theme_bw()

ggpubr::ggarrange(g1,g2,
                  nrow = 1, ncol = 2, 
                  common.legend = TRUE, 
                  legend="bottom",
                  #labels = paste0(porta, ' em função das latências'),
                  font.label = list(size = 15))

#scat1('p05')

#---------------------------------------------------------------

# Uma latencia em função da outra

g1 <- ggplot(data = massa, 
       mapping = aes(x=min_lat, 
                    y=max_lat)) +
  geom_point() + 
  geom_smooth(se=T, 
              linetype = 
                'longdash', 
              col = 4) + 
  geom_smooth(method = 'lm', 
              se = T, 
              col= 2)+ theme_bw()+
  theme(legend.title = element_blank())+
  xlab('Latência mínima') + ylab('Latência máxima') + ggtitle('')  

g2 <- ggplot(data = massa, 
       mapping = aes(x=max_lat, 
                     y=min_lat)) +
  geom_point() + 
  geom_smooth(se=T, 
              linetype = 
                'longdash', 
              col = 4) + 
  geom_smooth(method = 'lm', 
              se = T, 
              col= 2)+ theme_bw()+
  theme(legend.title = element_blank())+
  xlab('Latência máxima') + ylab('Latência mínima') + ggtitle('')  

ggpubr::ggarrange(g1,g2,
                  nrow = 1, ncol = 2, 
                  common.legend = TRUE, 
                  legend="bottom",
                  #labels = paste0(porta, ' em função das latências'),
                  font.label = list(size = 15))


#---------------------------------------------------------------

# Correlograma para numéricas

numericas <- select(massa, 
                    p0156, p06, p23, p4, p1, p15,
                    min_lat, max_lat)

names(numericas)[c(7,8)] <- c('min', 'max')

cor1 <- cor(numericas, method = 'pearson')

corrplot::corrplot.mixed(cor1, 
                         upper = 'ellipse',
                         tl.pos = "d"#,
                         #diag = 'n'
                         )

#---------------------------------------------------------------
