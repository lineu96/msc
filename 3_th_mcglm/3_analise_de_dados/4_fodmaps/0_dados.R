#---------------------------------------------------------------
# DADOS
#---------------------------------------------------------------

# Diretório dos dados e biblioteca

library(mcglm)
library(Matrix)
library(tidyverse)

#---------------------------------------------------------------

# Leitura e tratamento dos dados

dados <- read.csv2("dados1.csv")

#---------------------------------------------------------------

# Transformando respostas em binárias

dados$dor_abd2 <- ifelse(dados$dor_abd == 1, 0, 1)
#azia
#refluxo
#fome_dor
dados$nausea2 <- ifelse(dados$nausea == 1, 0, 1)
dados$ronco2 <- ifelse(dados$ronco == 1, 0, 1)
#ar
dados$arroto2 <- ifelse(dados$arroto == 1, 0, 1)
#gases
dados$constip2 <- ifelse(dados$constip == 1, 0, 1)
dados$diarr2 <- ifelse(dados$diarr == 1, 0, 1)
dados$fe_moles2 <- ifelse(dados$fe_moles == 1, 0, 1)
dados$fe_duras2 <- ifelse(dados$fe_duras == 1, 0, 1)
dados$urg_defec2 <- ifelse(dados$urg_defec == 1, 0, 1)
dados$n_esvaz2 <- ifelse(dados$n_esvaz == 1, 0, 1)

#---------------------------------------------------------------

# Tratando níveis das covariáveis

dados$sexo <- as.factor(ifelse(dados$sexo == 2, 'Feminino', 'Masculino'))
dados$sexo <- relevel(dados$sexo, ref = "Masculino")

dados$hpylori <- as.factor(ifelse(dados$hpylori == 1, 'Apresenta', 'Não apresenta'))
dados$hpylori <- relevel(dados$hpylori, ref = "Não apresenta")

dados$pre_dm <- as.factor(ifelse(dados$pre_dm == 1, 'Apresenta', 'Não apresenta'))
dados$pre_dm <- relevel(dados$pre_dm, ref = "Não apresenta")

dados$dm <- as.factor(ifelse(dados$dm == 1, 'Apresenta', 'Não apresenta'))
dados$dm <- relevel(dados$dm, ref = "Não apresenta")

dados$dislip <- as.factor(ifelse(dados$dislip == 1, 'Apresenta', 'Não apresenta'))
dados$dislip <- relevel(dados$dislip, ref = "Não apresenta")

dados$has <- as.factor(ifelse(dados$has == 1, 'Apresenta', 'Não apresenta'))
dados$has <- relevel(dados$has, ref = "Não apresenta")

dados$dcv <- as.factor(ifelse(dados$dcv == 1, 'Apresenta', 'Não apresenta'))
dados$dcv <- relevel(dados$dcv, ref = "Não apresenta")

dados$apneia <- as.factor(ifelse(dados$apneia == 1, 'Apresenta', 'Não apresenta'))
dados$apneia <- relevel(dados$apneia, ref = "Não apresenta")

dados$dor_art <- as.factor(ifelse(dados$dor_art == 1, 'Apresenta', 'Não apresenta'))
dados$dor_art <- relevel(dados$dor_art, ref = "Não apresenta")

dados$frutose2 <- as.factor(ifelse(dados$frutose == 0 | dados$frutose == 1,'baixo/medio','alto'))
dados$frutose2 <- relevel(dados$frutose2, ref = "baixo/medio")

dados$lactose2 <- as.factor(ifelse(dados$lactose == 0 | dados$lactose == 1,'baixo/medio','alto'))
dados$lactose2 <- relevel(dados$lactose2, ref = "baixo/medio")

dados$sorbitol2 <- as.factor(ifelse(dados$sorbitol == 0 | dados$sorbitol == 1,'baixo/medio','alto'))
dados$sorbitol2 <- relevel(dados$sorbitol2, ref = "baixo/medio")

dados$gos2 <- as.factor(ifelse(dados$gos == 0 | dados$gos == 1,'baixo/medio','alto'))
dados$gos2 <- relevel(dados$gos2, ref = "baixo/medio")

dados$fructano2 <- as.factor(ifelse(dados$fructano == 0 | dados$fructano == 1,'baixo/medio','alto'))
dados$fructano2 <- relevel(dados$fructano2, ref = "baixo/medio")

dados$manitol2 <- as.factor(ifelse(dados$manitol == 0 | dados$manitol == 1,'baixo/medio','alto'))
dados$manitol2 <- relevel(dados$manitol2, ref = "baixo/medio")

#---------------------------------------------------------------

# Retirando missing

dados2 <- na.omit(dados)

#---------------------------------------------------------------

# Exploratória

source('~/Dropbox/Aulas-Acessorias/9 Bruna Maia/2_scripts/0_funcoes.R')

nausea <- tb_freq(dados2$nausea2)
nausea$Niveis <- c('Não apresenta', 'Apresenta')
a<-bar_freqtab(nausea, 'Náusea')

fe_duras <- tb_freq(dados2$fe_duras2)
fe_duras$Niveis <- c('Não apresenta', 'Apresenta')
b<-bar_freqtab(fe_duras, 'Fezes duras')

urg_defec <- tb_freq(dados2$urg_defec2)
urg_defec$Niveis <- c('Não apresenta', 'Apresenta')
c<-bar_freqtab(urg_defec, 'Urg. p/ defecar')

n_esvaz <- tb_freq(dados2$n_esvaz2)
n_esvaz$Niveis <- c('Não apresenta', 'Apresenta')
d<-bar_freqtab(n_esvaz, 'Não esvaziamento')

x11()
ggpubr::ggarrange(a,b,c,d, nrow = 1, ncol = 4)

#---------------------------------------------------------------