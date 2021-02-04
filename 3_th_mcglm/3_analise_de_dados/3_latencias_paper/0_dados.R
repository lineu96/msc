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

