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

# mal_ajustados dos ajustes univariados

# mal_ajustados <- sort(unique(c(MAUS_MIN, MAUS_MAX)))

mal_ajustados <- c(36, 38, 51, 53, 57, 58, 61, 63, 65, 67, 
                   71, 73, 79, 80, 82, 83, 85, 87, 89, 90, 
                   93, 94, 95, 99, 101, 103, 105, 106, 107, 108, 
                   109, 110, 111, 112, 113, 114, 116, 117, 118, 134, 
                   151, 155, 162, 166, 171, 175, 177, 180, 182, 194, 
                   199, 207, 211, 215, 296, 298, 300, 302, 312, 313, 
                   314, 316, 322, 327, 341, 422, 423, 440, 441, 450, 
                   454, 458, 506, 530, 531, 532, 533, 534, 535, 536, 
                   537, 538, 540, 547, 549, 551, 553, 554, 563, 565, 
                   647, 649, 651, 653, 655, 657, 660, 664, 667, 669, 
                   671, 673, 675, 677, 679, 688, 691, 693, 694, 696, 
                   699, 701, 703, 705, 707, 709, 710, 711, 712, 713, 
                   714, 715, 716, 717, 718, 719, 720, 721, 722, 724, 
                   725, 726, 727, 728, 729, 731, 735, 736, 737, 738, 
                   739, 740, 741, 742, 767, 768, 769, 772, 773, 800, 
                   801, 808, 809, 820, 821, 828, 829, 837, 841, 843, 
                   846, 848, 863, 868, 880, 884, 885, 892, 893, 1032, 
                   1033, 1051, 1053, 1055, 1057, 1075, 1077, 1100, 1104, 1107, 
                   1113, 1114, 1115, 1116, 1117, 1118, 1119, 1120, 1121, 1164, 
                   1165, 1168, 1169, 1224, 1228, 1232, 1236, 1240, 1244, 1248, 
                   1252, 1256, 1260, 1264, 1268, 1286, 1287, 1288, 1289, 1438, 
                   1484, 1485, 1486, 1487, 1488, 1489, 1490, 1491, 1492, 1493, 
                   1494, 1495, 1496, 1498, 1511, 1513, 1515, 1517, 1519, 1520, 
                   1521, 1522, 1535, 1539, 1543, 1545)


massa <- subset(dados,  dados$n %in% indices & !(dados$n %in% mal_ajustados))

analista <- subset(dados,  !(dados$n %in% indices) | (dados$n %in% mal_ajustados))

nrow(dados)
nrow(massa) + nrow(analista)

#---------------------------------------------------------------