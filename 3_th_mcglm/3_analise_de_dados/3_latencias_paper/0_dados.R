#---------------------------------------------------------------
# DADOS
#---------------------------------------------------------------

# Diretório dos dados e biblioteca

library(mcglm)
library(Matrix)
library(tidyverse)

#---------------------------------------------------------------

# Leitura e tratamento dos dados

dados <- read.csv2('latencias_1_NOVA.csv',  header = T,  sep = ',')

names(dados) <- c('n',  'code',  'inst',  'op0',  'op1',  'op2',  
                  'lock',  'set_rex',  'segment',  'mem_xchg',  
                  'p0156',  'p06',  'p23',  'p237',  'p4',  'p1',  
                  'p15',  'p015',  'p5',  'p0',  'p05',  'p01',  
                  'p6',  'min_lat',  'max_lat',  'count')

## Seleção das variaveis de interesse
dados <- select(dados,  n,  inst,  lock,  segment, 
                p0156,  p06,  p23, p237,  p4,  p1,  p15,  p0,  p05, 
                min_lat,  max_lat,  count)

## Transformando portas em binária
dados$lock <- as.factor(ifelse(dados$lock > 0,  1, 0))

dados$p0156 <- as.factor(ifelse(dados$p0156 > 0,  1, 0))
dados$p06 <- as.factor(ifelse(dados$p06 > 0,  1, 0))
dados$p23 <- as.factor(ifelse(dados$p23 > 0,  1, 0))

dados$p0 <- as.factor(ifelse(dados$p0 > 0,  1, 0))
dados$p237 <- as.factor(ifelse(dados$p237 > 0,  1, 0))
dados$p4 <- as.factor(ifelse(dados$p4 > 0,  1, 0))
dados$p1 <- as.factor(ifelse(dados$p1 > 0,  1, 0))
dados$p15 <- as.factor(ifelse(dados$p15 > 0,  1, 0))
dados$segment <- ifelse(dados$segment > 0,  1, 0)

## Selecionando instruções

exclusao <-c('CMPXCHG8B',
             'CMPXCHG8B_LOCK',
             'DIV_NOREX',
             'DIV_REX',
             'ENTER',
             'ENTER_W',
             'IDIV_NOREX',
             'IDIV_REX',
             'MOVSB',
             'MOVSD',
             'MOVSW',
             'SGDT',
             'SIDT',
             'SLDT',
             'STOSB',
             'STOSD',
             'STOSW',
             'MOV_DR',
             'VERR',
             'BTC_LOCK',
             'BTR_LOCK',
             'BTS_LOCK',
             'LAR',
             'LSL',
             'VERW',
             'XCHG_NOREX',
             'XCHG_REX',
             'POP',
             'DIV', 
             'IDIV',
             'PUSH',
             'XCHG',
             'BTC',
             'BTR',
             'BTS',
             'MOV',
             'RCL',
             'RCR'
             )

#barplot(table(dados$max_lat))
dados2 <- subset(dados, !(dados$inst %in% exclusao))

nrow(dados)-nrow(dados2)

#---------------------------------------------------------------
# EXPLORATÓRIA
#---------------------------------------------------------------

a <- ggplot(dados2, aes(x = min_lat)) +
  geom_histogram(col = 1, fill='white') +
  xlab('Count') +
  ylab('Frequency') +
  theme_bw() +
  scale_y_continuous(breaks = round(seq(0, 120, length.out = 3), 2)) +
  ggtitle('a')

b <- ggplot(data = dados2, 
            mapping = aes_string(x='lock', y='min_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('Lock') + ylab('Min. Lat.') + ggtitle('b')

c <- ggplot(data = dados2, 
            mapping = aes_string(x='p0156', y='min_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('p0156') + ylab('') + ggtitle('c')

d <- ggplot(data = dados2, 
            mapping = aes_string(x='p06', y='min_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('p06') + ylab('') + ggtitle('d')

e <- ggplot(data = dados2, 
            mapping = aes_string(x='p23', y='min_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('p23') + ylab('') + ggtitle('e')

f <- ggplot(data = dados2, 
            mapping = aes_string(x='p237', y='min_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('p237') + ylab('') + ggtitle('f')

g <- ggplot(data = dados2, 
            mapping = aes_string(x='p4', y='min_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('p4') + ylab('') + ggtitle('g')

h <- ggplot(data = dados2, 
            mapping = aes_string(x='p1', y='min_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('p1') + ylab('') + ggtitle('h')


i <- ggplot(dados2, aes(x = max_lat)) +
  geom_histogram(col = 1, fill='white') +
  xlab('Count') +
  ylab('Frequency') +
  theme_bw() +
  scale_y_continuous(breaks = round(seq(0, 120, length.out = 3), 2)) +
  ggtitle('i')

j <- ggplot(data = dados2, 
            mapping = aes_string(x='lock', y='max_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('Lock') + ylab('Max. Lat.') + ggtitle('j')

k <- ggplot(data = dados2, 
            mapping = aes_string(x='p0156', y='max_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('p0156') + ylab('') + ggtitle('k')

l <- ggplot(data = dados2, 
            mapping = aes_string(x='p06', y='max_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('p06') + ylab('') + ggtitle('l')

m <- ggplot(data = dados2, 
            mapping = aes_string(x='p23', y='max_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('p23') + ylab('') + ggtitle('m')

n <- ggplot(data = dados2, 
            mapping = aes_string(x='p237', y='max_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('p237') + ylab('') + ggtitle('n')

o <- ggplot(data = dados2, 
            mapping = aes_string(x='p4', y='max_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('p4') + ylab('') + ggtitle('o')

p <- ggplot(data = dados2, 
            mapping = aes_string(x='p1', y='max_lat')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+
  xlab('p1') + ylab('') + ggtitle('p')

x11()
ggpubr::ggarrange(a,b,c,d,e,f,g,h,
                  i,j,k,l,m,n,o,p,
                  nrow = 2, ncol = 8)

#---------------------------------------------------------------