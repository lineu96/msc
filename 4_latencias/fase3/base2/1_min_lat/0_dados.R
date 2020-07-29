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

dados <- read.csv2('latencias_2_NOVA.csv', header = T, sep = ',')

names(dados) <- c('n', 'code', 'inst', 'extension', 'op0',
                  'op1', 'op2', 'op3', 'p0156', 'p06', 
                  'p23', 'p237', 'p4', 'p1', 'p15', 
                  'p015', 'p5', 'p0', 'p05', 'p01', 
                  'p6', 'min_lat', 'max_lat', 'count')

#Transformando portas em binárias
dados$p23 <- as.factor(ifelse(dados$p23 > 0, 1,0))
dados$p4 <- as.factor(ifelse(dados$p4 > 0, 1,0))
dados$p015 <- as.factor(ifelse(dados$p015 > 0, 1,0))
dados$p5 <- as.factor(ifelse(dados$p5 > 0, 1,0))
dados$p0 <- as.factor(ifelse(dados$p0 > 0, 1,0))
dados$p01 <- as.factor(ifelse(dados$p01 > 0, 1,0))
dados$p237 <- as.factor(ifelse(dados$p237 > 0, 1,0))

#---------------------------------------------------------------

# Divisão da base

# latência mínima entre 0 e 15 &
# latência máxima entre 0 e 20

indices <- subset(dados, dados$min_lat<=15 & dados$max_lat<=24)$n

massa <- subset(dados, dados$n %in% indices)
analista <- subset(dados, !(dados$n %in% indices))

#---------------------------------------------------------------