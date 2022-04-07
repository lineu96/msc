
#---------------------------------------------------------------
# SOYA MCGLM
#---------------------------------------------------------------

library(mcglm)
library(Matrix)

library(ggplot2)
library(ggpubr)

source('~/msc/0_funcoes/functions.R')

#---------------------------------------------------------------

# Contexto

## Efeito de água e adubo na produção de soja.

## Conjunto de dados de `soya` disponível no pacote mcglm. 

## Dados de um experimento realizado em uma casa de vegetação 
## com soja. 

## Duas plantas por parcela.

## Três níveis do fator correspondente à quantidade de água 
## no solo (water).

## Cinco níveis de adubação com potássio (pot).

## Cinco blocos (block).

## Três variáveis de resposta: 
### 1. Produtividade de grãos (contínua).
### 2. Número de sementes (contagem).
### 3. Número de sementes viáveis por planta (binomial). 

## O objetivo do experimento é medir os efeitos das covariáveis na 
## variável resposta.

#---------------------------------------------------------------

# Dados

soya
str(soya)

#---------------------------------------------------------------

# Exploratória

summary(soya)

a<-ggplot(soya, aes(x = grain)) +
  geom_histogram(col = 1, fill='white',
                 breaks = hist(soya$grain, 
                               plot = F)$breaks) +
  xlab('Rendimento') +
  ylab('Frequência') +
  theme_bw()

b<-ggplot(data = soya, 
       mapping = aes(x=block, 
                     y=grain)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_bw() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Bloco') + 
  ylab('Rendimento')

c<-ggplot(data = soya, 
       mapping = aes(x=water, 
                     y=grain)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_bw() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Água') + 
  ylab('Rendimento')

d<-ggplot(data = soya, 
       mapping = aes(x=pot, 
                     y=grain)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_bw() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Potássio') + 
  ylab('Rendimento')

e<-ggplot(soya, aes(x = seeds)) +
  geom_histogram(col = 1, fill='white',
                 breaks = hist(soya$seeds, 
                               plot = F)$breaks) +
  xlab('Grãos') +
  ylab('Frequência') +
  theme_bw()

f<-ggplot(data = soya, 
          mapping = aes(x=block, 
                        y=seeds)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_bw() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Bloco') + 
  ylab('Grãos')

g<-ggplot(data = soya, 
          mapping = aes(x=water, 
                        y=seeds)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_bw() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Água') + 
  ylab('Grãos')

h<-ggplot(data = soya, 
          mapping = aes(x=pot, 
                        y=seeds)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_bw() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Potássio') + 
  ylab('Grãos')


i<-ggplot(soya, aes(x = viablepeas/totalpeas)) +
  geom_histogram(col = 1, fill='white',
                 breaks = hist(soya$viablepeas / soya$totalpeas, 
                               plot = F)$breaks) +
  xlab('Prop. de sementes viáveis') +
  ylab('Frequência') +
  theme_bw()

j<-ggplot(data = soya, 
          mapping = aes(x=block, 
                        y=viablepeas/totalpeas)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_bw() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Bloco') + 
  ylab('Prop. de sementes viáveis')

k<-ggplot(data = soya, 
          mapping = aes(x=water, 
                        y=viablepeas/totalpeas)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_bw() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Água') + 
  ylab('Prop. de sementes viáveis')

l<-ggplot(data = soya, 
          mapping = aes(x=pot, 
                        y=viablepeas/totalpeas)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_bw() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Potássio') + 
  ylab('Prop. de sementes viáveis')


ggarrange(a,b,c,d,
          e,f,g,h,
          i,j,k,l,
          nrow = 3, 
          ncol = 4)

#---------------------------------------------------------------

# Preditores para média

form.grain <- grain ~ water * pot
form.seed <- seeds ~ water * pot

soya$viablepeasP <- soya$viablepeas / soya$totalpeas
form.peas <- viablepeasP ~ water * pot

#---------------------------------------------------------------

# Preditor matricial

Z0 <- mc_id(soya)
Z1 <- mc_mixed(~0 + factor(block), data = soya)

#---------------------------------------------------------------

# Ajuste

fit_joint <- mcglm(linear_pred = c(form.grain, 
                                   form.seed, 
                                   form.peas),
                   matrix_pred = list(c(Z0, Z1), 
                                      c(Z0, Z1), 
                                      c(Z0, Z1)),
                   link = c("identity",
                            "log", 
                            "logit"),
                   variance = c("constant", 
                                "tweedie", 
                                "binomialP"),
                   Ntrial = list(NULL, 
                                 NULL, 
                                 soya$totalpeas),
                   power_fixed = c(T,T,T),
                   data = soya)

#---------------------------------------------------------------

# Resumos

summary(fit_joint, verbose = TRUE, print = "Regression")
#summary(fit_joint, verbose = TRUE, print = "power")
summary(fit_joint, verbose = TRUE, print = "Dispersion")
summary(fit_joint, verbose = TRUE, print = "Correlation")

#---------------------------------------------------------------

# Anovas

mc_anova_I(fit_joint)
mc_anova_II(fit_joint)
mc_anova_III(fit_joint)

#---------------------------------------------------------------

# Manovas

mc_manova_I(fit_joint)
mc_manova_II(fit_joint)
mc_manova_III(fit_joint)

#---------------------------------------------------------------

# Avaliando parâmetros de dispersão

mc_anova_disp(fit_joint,
              p_var = list(c(0,1), c(0,1), c(0,1)),
              names = list(c('tau10', 'tau11'),
                           c('tau20', 'tau11'),
                           c('tau30', 'tau11')))

mc_manova_disp(fit_joint,
              p_var = c(0,1),
              names = c('tau11', 'tau21'))

#---------------------------------------------------------------

# Hipóteses lineares gerais

parametros <- coef(fit_joint, type = 'beta')[,-3]
parametros$beta_names <- Reduce(c,fit_joint$beta_names)
parametros

# Testando um único parâmetro (water50)
mc_linear_hypothesis(object =  fit_joint, 
                     hypothesis = c('beta11 = 0'))

# Testando dois parâmetros de uma mesma resposta
# water50 e water62.5
mc_linear_hypothesis(object =  fit_joint, 
                     hypothesis = c('beta11 = 0', 
                                    'beta12 = 0'))

# Testando parâmetros de diferentes respostas
# water50 e water62.5 para as 3 respostas
mc_linear_hypothesis(object =  fit_joint, 
                     hypothesis = c('beta11 = 0', 
                                    'beta12 = 0',
                                    'beta21 = 0',
                                    'beta22 = 0',
                                    'beta31 = 0',
                                    'beta32 = 0'))

# Testando igualdade entre parâmetros
mc_linear_hypothesis(object =  fit_joint, 
                     hypothesis = c('beta11 = beta21'))

# Testando parêmtros de dispersão

coef(fit_joint, type = 'tau')[,-3]

mc_linear_hypothesis(object =  fit_joint, 
                     hypothesis = c('tau11 = 0'))

mc_linear_hypothesis(object =  fit_joint, 
                     hypothesis = c('tau11 = 0',
                                    'tau21 = 0',
                                    'tau31 = 0'))

mc_linear_hypothesis(object =  fit_joint,
                     hypothesis = c('tau12 = tau22'))

#---------------------------------------------------------------

# Comparações múltiplas

## Por resposta
mc_multcomp(object = fit_joint,
            effect = list(c('water'), 
                          c('water'),
                          c('water')), 
            data = soya)

mc_multcomp(object = fit_joint,
            effect = list(c('pot'), 
                          c('pot'),
                          c('pot')), 
            data = soya)

mc_multcomp(object = fit_joint,
            effect = list(c('water', 'pot'), 
                          c('water', 'pot'),
                          c('water', 'pot')), 
            data = soya)

## Multivariado
mc_mult_multcomp(object = fit_joint, 
                 effect = c('water'), 
                 data = soya)

mc_mult_multcomp(object = fit_joint, 
                 effect = c('pot'), 
                 data = soya)

mc_mult_multcomp(object = fit_joint, 
                 effect = c('water', 'pot'), 
                 data = soya)

#---------------------------------------------------------------
