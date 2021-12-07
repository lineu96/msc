#----------------------------------------------------------------
# TESTES
#----------------------------------------------------------------

# bibliotecas necessárias

library(mcglm)
library(Matrix)

#----------------------------------------------------------------
# minhas funções
source('~/msc/3_th_mcglm/0_funcoes/functions.R')
#----------------------------------------------------------------

source('~/msc/3_th_mcglm/4_estudo_simulacao/varia_hipotese.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/varia_modelo.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/analises/grafico.R')

#----------------------------------------------------------------

# Testes

sample_size = 1000
n_datasets = 10
n_treatment = 4
n_distances = 20

betas_normal = c(5,0,0,0)
betas_poisson = c(2.3,0,0,0)
betas_binomial = c(0.05,0,0,0)

initial_betas_normal = c(5,0,0,0)
initial_betas_poisson = c(2.3,0,0,0)
initial_betas_binomial = c(0.05,0,0,0)

#----------------------------------------------------------------

normal1 <- varia_hipotese(sample_size = sample_size,
                          n_datasets = n_datasets,
                          n_treatment = n_treatment,
                          betas = betas_normal,
                          n_distances = n_distances,
                          distribution = 'normal')

normal2 <- varia_modelo(sample_size = sample_size,
                        n_datasets = n_datasets,
                        n_treatment = n_treatment,
                        initial_betas = initial_betas_normal,
                        n_distances = n_distances,
                        distribution = 'normal')

#----------------------------------------------------------------

poisson1 <- varia_hipotese(sample_size = sample_size,
                           n_datasets = n_datasets,
                           n_treatment = n_treatment,
                           betas = betas_poisson,
                           n_distances = n_distances,
                           distribution = 'poisson')

poisson2 <- varia_modelo(sample_size = sample_size,
                         n_datasets = n_datasets,
                         n_treatment = n_treatment,
                         initial_betas = initial_betas_poisson,
                         n_distances = n_distances,
                         distribution = 'poisson')

#----------------------------------------------------------------

binomial1 <- varia_hipotese(sample_size = sample_size,
                            n_datasets = n_datasets,
                            n_treatment = n_treatment,
                            betas = betas_binomial,
                            n_distances = n_distances,
                            distribution = 'binomial')

binomial2 <- varia_modelo(sample_size = sample_size,
                          n_datasets = n_datasets,
                          n_treatment = n_treatment,
                          initial_betas = initial_betas_binomial,
                          n_distances = n_distances,
                          distribution = 'binomial')

#----------------------------------------------------------------

beta1 <- varia_hipotese(sample_size = sample_size,
                        n_datasets = n_datasets,
                        n_treatment = n_treatment,
                        betas = betas_beta,
                        n_distances = n_distances,
                        distribution = 'beta')

beta2 <- varia_modelo(sample_size = sample_size,
                      n_datasets = n_datasets,
                      n_treatment = n_treatment,
                      initial_betas = initial_betas_beta,
                      n_distances = n_distances,
                      distribution = 'beta')

#----------------------------------------------------------------

png(filename='~/msc/3_th_mcglm/4_estudo_simulacao/n50.png',
    width = 800, height = 500)

par(mfrow=c(2,4),oma = c(0, 0, 2, 0))

grafico(normal1, main = 'Normal - varia hipótese')
grafico(poisson1, main = 'Poisson - varia hipótese')
grafico(binomial1, main = 'Binomial - varia hipótese')


grafico(normal2, main = 'Normal - varia modelo')
grafico(poisson2, main = 'Poisson - varia modelo')
grafico(binomial2, main = 'Binomial - varia modelo')

mtext("20 distâncias, 500 datasets, n = 50", outer = TRUE, cex = 1.5)

dev.off()

#----------------------------------------------------------------

