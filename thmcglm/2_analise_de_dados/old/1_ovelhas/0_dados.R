#---------------------------------------------------------------
# DADOS
#---------------------------------------------------------------

# Diretório dos dados e biblioteca

setwd("~/msc/3_th_mcglm/3_analise_de_dados/1_ovelhas")

library(mcglm)
library(Matrix)
library(tidyverse)
library(ggpubr)

#---------------------------------------------------------------

# Leitura e tratamento dos dados

load("data_rbb.RData")

#---------------------------------------------------------------

# Exploratória

data_rbb2 <- 
  data_rbb %>% 
  gather(key = "response", value = "value", count, prop)

data_rbb2$moment <- ordered(data_rbb2$moment, 
                            levels=c("Pre", "During", "Post"))

count_exp <- subset(data_rbb2, response == 'count')
prop_exp  <- subset(data_rbb2, response == 'prop')

## Count
g1 <- ggplot(count_exp, aes(x = value)) +
  geom_histogram(col = 1, fill='white') +
  xlab('Count') +
  ylab('Frequency') +
  theme_bw() +
  scale_y_continuous(breaks = round(seq(0, 120, length.out = 3), 2)) +
  ggtitle('a')

g2 <- ggplot(count_exp, aes(x = lineage, y = value)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill='white') +
  xlab('Lineage') +
  ylab('Count') +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_color_discrete(guide = F)+
  ggtitle('b')+
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")

g3 <- ggplot(count_exp, aes(x = session, y = value)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill='white') +
  xlab('Session') +
  ylab('') +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_color_discrete(guide = F)+
  ggtitle('c')+
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")

g4<-
  ggplot(count_exp, aes(x=moment, y=value)) +  
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill='white')+ 
  xlab('Moment')+ ylab('') + 
  theme_bw()+
  theme(legend.title=element_blank(),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  + scale_color_discrete(guide = F)+
  ggtitle('d')+
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")

#g <- ggarrange(g1, g2, g3, g4,
#               ncol = 4, nrow = 1)

#g

## Prop

g5 <- ggplot(prop_exp, aes(x = value)) +
  geom_histogram(col = 1, fill='white') +
  xlab('Proportion') +
  ylab('Frequency') +
  theme_bw() +
  scale_y_continuous(breaks = round(seq(0, 120, length.out = 3), 2))+
  ggtitle('e')

g6 <- ggplot(prop_exp, aes(x = lineage, y = value)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill='white') +
  xlab('Lineage') +
  ylab('Proportion') +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_color_discrete(guide = F)+
  ggtitle('f')+
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")

g7 <- ggplot(prop_exp, aes(x = session, y = value)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill='white') +
  xlab('Session') +
  ylab('') +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_color_discrete(guide = F)+
  ggtitle('g')+
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")

g8<-
  ggplot(prop_exp, aes(x=moment, y=value)) +  
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill='white')+ 
  xlab('Moment')+ ylab('') + 
  theme_bw()+
  theme(legend.title=element_blank(),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  + scale_color_discrete(guide = F)+
  ggtitle('h')+
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")

#g <- ggarrange(g5, g6, g7, g8,
#               ncol = 4, nrow = 1)

#g

#---------------------------------------------------------------
x11()

ggpubr::ggarrange(g1,g2,g3,g4,
                  g5,g6,g7,g8,
                  nrow = 2, ncol = 4)
#---------------------------------------------------------------