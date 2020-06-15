#-------------------------------------------------------------------
# Análise TG Bruno Tissei
#-------------------------------------------------------------------


# CONTEXTO
#-------------------------------------------------------------------

# Intrução entra no processador e é decodificada.

# Uma macroinstrução é quebrada em diversas micro

# Ao todo existem 2700 macroinstruções

# Não sabemos o que é feito na micro

# As micro são destinadas para portas que podem ter uma ou mais 
# unidades de execução (a mesma porta pode ser capaz de realizar
# mais de um tipo de tarefa), e unidades de execução diferentes
# podem realizar a mesma tarefa

# Macro gera micros, micros são executadas em diferentes lugares

# PROBLEMA: quanto tempo uma macro leva para ser executada?
# Dependa das micro

# Em determinadas unidades as instruções levam sempre o mesmo
# tempo para serem executadas. 

# É feito um trabalho de inferência para verificar quais portas
# são usadas

# Dada a instrução e as portas, qual foi a latência
#-------------------------------------------------------------------

# VARIÁVEIS

# NÚMERO DA INSTRUÇÃO
# N

# NOME DA INSTRUÇÃO
# Instruction

# VARIÁVEIS INDICADORAS
# LOCK
# SET-REX
# SEGMENT
# MEM_XCHG

# PORTAS (quantas vezes foi usada)
# p0156
# p06
# p23
# p237
# p4
# p1
# p15
# p015
# p5
# p0
# p05
# p01
# p6

# Min Latency	
# Max Latency

#-------------------------------------------------------------------

library(tidyverse)
library(gridExtra)
library(ggpubr)

source('funcoes.R')

#-------------------------------------------------------------------

# DADOS
#-------------------------------------------------------------------

dados <- read.csv2('tissei.csv', header = T, sep = ',')

str(dados)

names(dados) <- c('n', 'inst', 'lock', 'set_rex', 'segment',
                  'mem_xchg', 'p0156', 'p06', 'p23', 'p237', 
                  'p4', 'p1', 'p15', 'p015', 'p5', 'p0', 
                  'p05', 'p01', 'p6', 'min_lat', 'max_lat')

#-------------------------------------------------------------------
# n vai de 1 até 1127 (mesmo numero de linhas)
#-------------------------------------------------------------------

# 299 instruções (as com mesmo nome passam por diferentes portas)
instructions <- as.data.frame(sort(table(dados$inst), decreasing = T))
names(instructions) <- c('inst', 'freq')

nrow(instructions) 

#-------------------------------------------------------------------

# Instruções com padrão '_'
indicadoras <- subset(instructions, str_detect(instructions$inst, '_') == T)

padroes <- substr(indicadoras$inst, 
                  str_locate(indicadoras$inst, '_'),
                  stringr::str_length(indicadoras$inst))

padroes <- as.data.frame(table(padroes))
arrange(padroes, desc(Freq))

#-------------------------------------------------------------------
