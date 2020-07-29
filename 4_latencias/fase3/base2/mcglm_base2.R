#################################################################
### AJUSTE MODELO MULTIVARIADO LATENCIAS EM FUNÇÃO DAS PORTAS ###
#################################################################

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

# Ajuste do modelo

#---------------------------------------------------------------

# Preditores

form.min_lat <- min_lat ~ (p23 + p4 + p015 + p5 + p0 + p01 + count)
form.max_lat <- max_lat ~ (p23 + p4 + p015 + p5 + p0 + p01 + count)

#----------------------------------------------------------------

# Matrix linear predictor

Z0 <- mc_id(massa) # Identidade

#----------------------------------------------------------------

# Ajuste

fit <- 
  mcglm(linear_pred = c(form.min_lat, form.max_lat),
        matrix_pred = list(c(Z0), 
                           c(Z0)),
        link = c("log", "log"),
        variance = c("poisson_tweedie", "poisson_tweedie"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 0.1,
                                 max_iter = 20,
                                 tol = 1e-01),
        #power_fixed = c(F,F),
        data = massa)

#----------------------------------------------------------------

matplot(fit$IterationCovariance, type = 'l', xlim = c(1,10)) 

#----------------------------------------------------------------

# Resumo do modelo

summary(fit)

coef(fit, type = 'beta')
coef(fit, type = 'power')
coef(fit, type = 'tau')

#Poisson (ϕ = 1 & p = 1)

#---------------------------------------------------------------

# Análise de resíduos

#---------------------------------------------------------------

# Obtenção dos resíduos

## chol(vcov) inversa
chol_inv <- Matrix::chol(fit$inv_C)

## Resíduos empilhados
res <- as.numeric(residuals(fit, type = 'raw'))

## Produto matricial
residuos <- as.numeric(chol_inv%*%res)

## Preditos
preditos <- fit$fitted

## Respostas
resp <- c(rep('min_lat',nrow(massa)),rep('max_lat',nrow(massa)))

## Dataframe
res_pred <- data.frame(index = rep(1:nrow(massa), 2),
                       resp = resp,
                       observado = c(massa$min_lat, massa$max_lat),
                       preditos = preditos,
                       residuos = residuos
)

#---------------------------------------------------------------

MASS::fitdistr(res_pred$residuos, 
               densfun = "normal")

# Gráficos para todas as respostas

## Histograma
ggplot(data = res_pred, aes(x=residuos))+
  geom_histogram(fill=1,alpha=0.5, col = 1)+
  theme_bw()+
  xlab('Resíduos')+
  ylab('Densidade')+
  geom_vline(xintercept = 0, col = 2, lty = 2, lwd = 1)#+
#stat_function(fun = function(x) dnorm(x, 
#                                      mean = -0.01770990, 
#                                      sd = 0.72789316) * nrow(massa),
#              color = "darkred", size = 1)+
#stat_function(fun = function(x) dnorm(x, 
#                                      mean = 0, 
#                                      sd = 1) * nrow(massa),
#              color = "blue", size = 1)

## Densidade
ggplot(data = res_pred, aes(x=residuos))+
  geom_density(fill=1,alpha=0.5, col = 1)+
  theme_bw()+
  xlab('Resíduos')+
  ylab('Densidade')+
  geom_vline(xintercept = 0, col = 2, lty = 2, lwd = 1)

## Resíduos vs Predito
ggplot(data = res_pred, aes(y=residuos,x=preditos))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2)+
  xlab('Preditos')+
  ylab('Resíduos')

## Observado vs Predito
ggplot(data = res_pred, aes(x=observado,y=preditos))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2)+
  xlab('Preditos')+
  ylab('Resíduos')

## qqplot
ggplot(data = res_pred, 
       mapping = aes(sample = residuos)) +
  geom_qq(alpha = 0.5) + geom_qq_line(col = 2)+
  theme_bw()# + facet_wrap(~resp)


#---------------------------------------------------------------

# Gráficos por resposta

MASS::fitdistr(subset(res_pred, res_pred$resp == 'min_lat')$residuos, 
               densfun = "normal")

MASS::fitdistr(subset(res_pred, res_pred$resp == 'max_lat')$residuos, 
               densfun = "normal")


## Histograma
ggplot(data = res_pred, aes(x=residuos))+
  geom_histogram(fill=1,alpha=0.5, col = 1)+
  theme_bw()+
  xlab('Resíduos')+
  ylab('Densidade')+
  geom_vline(xintercept = 0, col = 2, lty = 2, lwd = 1) + 
  facet_wrap(~resp, scales = "free")

## Densidade
ggplot(data = res_pred, aes(x=residuos))+
  geom_density(fill=1,alpha=0.5, col = 1)+
  theme_bw()+
  xlab('Resíduos')+
  ylab('Densidade')+
  geom_vline(xintercept = 0, col = 2, lty = 2, lwd = 1) + 
  facet_wrap(~resp, scales = "free")

## Resíduos vs Predito
ggplot(data = res_pred, aes(y=residuos,x=preditos))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2)+
  xlab('Preditos')+
  ylab('Resíduos') + 
  facet_wrap(~resp, scales = "free")

## Observado vs Predito
ggplot(data = res_pred, aes(x=observado,y=preditos))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2)+
  xlab('Preditos')+
  ylab('Resíduos') + 
  facet_wrap(~resp, scales = "free")

## qqplot
ggplot(data = res_pred, 
       mapping = aes(sample = residuos)) +
  geom_qq(alpha = 0.5) + geom_qq_line(col = 2)+
  theme_bw() + facet_wrap(~resp, scales = "free")

## Residuos x Índice
ggplot(data = res_pred, aes(y=residuos,x=index))+
  geom_point(alpha=0.4)+
  geom_smooth(col=2, se=F)+
  theme_bw()+
  facet_wrap(~resp)+
  xlab('Índices')+
  ylab('Resíduos')

#--------------------------------------------------------------------

#################################################################
# ANÁLISE DOS INDIVÍDUOS MAL AJUSTADOS E REAJUSTE DO MODELO
#################################################################

massa$pred_min <- subset(res_pred, resp == 'min_lat')$preditos
massa$res_min <- subset(res_pred, resp == 'min_lat')$residuos
massa$pred_max <- subset(res_pred, resp == 'max_lat')$preditos
massa$res_max <- subset(res_pred, resp == 'max_lat')$residuos

massa[,-c(1,4)]

names(massa)

mal_ajustados <- subset(massa, 
                        res_min > 2   | 
                          res_min < -2  | 
                          res_max > 2   | 
                          res_max < -2  |
                          pred_min > 45 |
                          pred_max > 45)
mal_ajustados[,-c(1,4)]


# MODELO FICOU MELHOR COM COUNT
# AVALIAR CORRELAÇÃO COUNT COM NUMERICAS
# TESTAR INTERAÇÕES NO MODELO
# TESTAR RETIRAR DIV E IDIV