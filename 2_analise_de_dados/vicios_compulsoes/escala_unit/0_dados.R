#---------------------------------------------------------------
# DADOS
#---------------------------------------------------------------

# Diretório dos dados e biblioteca

library(mcglm)
library(Matrix)
library(tidyverse)

#---------------------------------------------------------------

# Leitura dos dados

load("~/msc/2_analise_de_dados/vicios_compulsoes/escala_unit/dados_dissertacao.RData")

#id - fator identificador de paciente
#grupo - fator identiricador de grupo (Placebo, Probiótico)
#momento - fator identificador de momento (T0 (preop), T1 (3 meses), T2 (1 ano))
#YFAS - número de sintomas que caracterizam vício, varia de 0 a 7
#BES - escore que caracteriza compulsão, varia de 0 a 46
#YFAS_taxa - proporção de sintomas de caracterizam vício
#BES_taxa - proporção do escore que caracteriza compulsão

#---------------------------------------------------------------
# EXPLORATÓRIA
#---------------------------------------------------------------

desc <- dados_dissertacao[,c('grupo','momento',
                             'YFAS_taxa','BES_taxa')] %>%
  group_by(grupo, momento) %>%
  summarise(n_yale = length(YFAS_taxa),
            media_yale = round(mean(YFAS_taxa),2),
            sd_yale = round(sd(YFAS_taxa),2),
            n_ecap = length(BES_taxa),
            media_ecap = round(mean(BES_taxa),2), 
            sd_ecap = round(sd(BES_taxa),2))

desc

#---------------------------------------------------------------

a <- ggplot(dados_dissertacao, aes(x = YFAS_taxa)) +
  geom_histogram(col = 1, fill='white',
                 breaks = hist(dados_dissertacao$YFAS_taxa, 
                               plot = F)$breaks) +
  xlab('YFAS') +
  ylab('Frequência') +
  xlim(c(0,1))+
  theme_bw() +
  ggtitle('a')

b <- ggplot(data = dados_dissertacao, 
            mapping = aes_string(x='grupo', y='YFAS_taxa')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Grupo') + 
  ylab('YFAS') + 
  ylim(c(0,1))+
  ggtitle('b')

c <- ggplot(data = dados_dissertacao, 
            mapping = aes_string(x='momento', y='YFAS_taxa')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Momento') + 
  ylab('YFAS') + 
  ylim(c(0,1))+
  ggtitle('c')

d <- ggplot(dados_dissertacao, aes(x = BES_taxa)) +
  geom_histogram(col = 1, fill='white',
                 breaks = hist(dados_dissertacao$BES_taxa, 
                               plot = F)$breaks) +
  xlab('BES') +
  ylab('Frequência') +
  xlim(c(0,1))+
  theme_bw() +
  ggtitle('d')

e <- ggplot(data = dados_dissertacao, 
            mapping = aes_string(x='grupo', y='BES_taxa')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Grupo') +
  ylab('BES') + 
  ylim(c(0,1))+
  ggtitle('e')

f <- ggplot(data = dados_dissertacao, 
            mapping = aes_string(x='momento', y='BES_taxa')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Momento') + 
  ylab('BES') + 
  ylim(c(0,1))+
  ggtitle('f')

g <- ggpubr::ggarrange(a,b,c,d,e,f,
                       nrow = 2, ncol = 3)

#---------------------------------------------------------------

ggsave(filename='descritiva.pdf', 
       plot=g, device="pdf", 
       path=getwd(),
       dpi=500, 
       height = 6, 
       width = 8)

#---------------------------------------------------------------