#---------------------------------------------------------------
# DADOS
#---------------------------------------------------------------

# Diretório dos dados e biblioteca

setwd("~/msc/4_latencias/fase2")

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

#---------------------------------------------------------------

# Divisão da base

# latência mínima entre 0 e 25 &
# latência máxima entre 0 e 45

indices <- subset(dados,  dados$min_lat<=25 & 
                     dados$max_lat<=45 | 
                     dados$inst == 'DIV' | 
                     dados$inst == 'IDIV')$n

# mal_ajustados dos ajustes univariados

# mal_ajustados <- sort(unique(c(MAUS_MIN, MAUS_MAX)))

mal_ajustados <- c(85, 173, 174, 175, 191, 192, 193, 209, 210, 211,
                   214, 335, 409, 410, 411, 412, 413, 414, 420, 421,
                   422, 423, 424, 425, 485, 486, 487, 533, 534, 535,
                   536, 537, 538, 539, 540, 541, 542, 543, 544, 545,
                   574, 575, 576, 577, 635, 636, 638, 640, 641, 642,
                   644, 645, 646, 647, 648, 649, 656, 657, 658, 665,
                   666, 677, 678, 683, 684, 685, 692, 693, 842, 845,
                   848, 851, 854, 857, 860, 863, 866, 869, 872, 875,
                   878, 881, 884, 887, 888, 967, 969, 970, 971, 972,
                   1075, 1076, 1077, 1078, 1079)

massa <- subset(dados,  dados$n %in% indices & !(dados$n %in% mal_ajustados))

analista <- subset(dados,  !(dados$n %in% indices) | (dados$n %in% mal_ajustados))

nrow(dados)
nrow(massa) + nrow(analista)

#---------------------------------------------------------------