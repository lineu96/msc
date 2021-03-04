#---------------------------------------------------------------
# DADOS
#---------------------------------------------------------------

# Diretório dos dados e biblioteca

library(mcglm)
library(Matrix)
library(tidyverse)

#---------------------------------------------------------------

# Leitura e tratamento dos dados

dados <- read.csv2("~/Dropbox/Aulas-Acessorias/10 Ligia Carlos/0 dados/dados.csv", sep = ',')

# Retirando linhas com dados ausentes

dados2 <- na.omit(dados)

# Tratamento para formato longo

imc <- dados %>% gather(key = 'momento_imc', 
                        value = 'imc', imc1:imc4)
imc <- imc[,c(1,18,19)]
imc$chave <- 1:nrow(imc)

yale <- dados %>% gather(key = 'momento_yale', 
                         value = 'yale', yale1:yale4)
yale <- yale[,c(1,18,19)]
yale$chave <- 1:nrow(yale)

ecap <- dados %>% gather(key = 'momento_ecap', 
                         value = 'ecap', ecap1:ecap4)
ecap <- ecap[,c(1,18,19)]
ecap$chave <- 1:nrow(ecap)

dados3 <- dados %>% gather(key = 'momento', 
                           value = 'peso', peso1:peso4)
dados3 <- dados3[,c(1,2,3,4,5,6,7,8,18,19)]
dados3$chave <- 1:nrow(dados3)

dados3 <- left_join(dados3, imc)
dados3 <- left_join(dados3, yale)
dados3 <- left_join(dados3, ecap)

#dados3[,c(9,12,14,16)]

dados3$momento <- as.factor(dados3$momento)
levels(dados3$momento) <- c('Pré op.','90 dias','365 dias')

dados3 <- dados3[,-c(12,14,16)]

#names(dados3)

dados4 <- na.omit(dados3)

# nrow(dados3)
# nrow(dados4)

# RESPOSTAS NA ESCALA UNITARIA

dados4$yale2 <- dados4$yale/8
dados4$ecap2 <- dados4$ecap/46

#---------------------------------------------------------------
# EXPLORATÓRIA
#---------------------------------------------------------------

a <- ggplot(dados4, aes(x = yale2)) +
  geom_bar(col = 1, fill='white') +
  xlab('Count') +
  ylab('Frequency') +
  theme_bw() +
  #scale_y_continuous(breaks = round(seq(0, 120, length.out = 3), 2)) +
  ggtitle('a')

b <- ggplot(data = dados4, 
            mapping = aes_string(x='grupo', y='yale2')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('Grupo') + ylab('Yale') + ggtitle('b')

c <- ggplot(data = dados4, 
            mapping = aes_string(x='momento', y='yale2')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('Momento') + ylab('Yale') + ggtitle('c')

d <- ggplot(dados4, aes(x = ecap2)) +
  geom_bar(col = 1, fill='white') +
  xlab('Count') +
  ylab('Frequency') +
  theme_bw() +
  #scale_y_continuous(breaks = round(seq(0, 120, length.out = 3), 2)) +
  ggtitle('d')

e <- ggplot(data = dados4, 
            mapping = aes_string(x='grupo', y='ecap2')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('Grupo') + ylab('Yale') + ggtitle('e')

f <- ggplot(data = dados4, 
            mapping = aes_string(x='momento', y='ecap2')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('Momento') + ylab('Yale') + ggtitle('f')

x11()
ggpubr::ggarrange(a,b,c,d,e,f,
                  nrow = 2, ncol = 3)

#---------------------------------------------------------------