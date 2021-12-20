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

source('~/msc/3_th_mcglm/4_estudo_simulacao/simula_thmcglm.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/analises/grafico.R')

#----------------------------------------------------------------

# Testes

n_datasets = 500
n_treatment = 4
n_distances = 20

betas_normal = c(5,0,0,0)
betas_poisson = c(2.3,0,0,0)
betas_binomial = c(0.5,0,0,0)

#----------------------------------------------------------------

sample_size = 50

normal_n50 <- simula(sample_size = sample_size,
                     n_datasets = n_datasets,
                     n_treatment = n_treatment,
                     betas = betas_normal,
                     n_distances = n_distances,
                     distribution = 'normal')

binomial_n50 <- simula(sample_size = sample_size,
                       n_datasets = n_datasets,
                       n_treatment = n_treatment,
                       betas = betas_binomial,
                       n_distances = n_distances,
                       distribution = 'binomial')

poisson_n50 <- simula(sample_size = sample_size,
                      n_datasets = n_datasets,
                      n_treatment = n_treatment,
                      betas = betas_poisson,
                      n_distances = n_distances,
                      distribution = 'poisson')


#----------------------------------------------------------------

sample_size = 100

normal_n100 <- simula(sample_size = sample_size,
                      n_datasets = n_datasets,
                      n_treatment = n_treatment,
                      betas = betas_normal,
                      n_distances = n_distances,
                      distribution = 'normal')

binomial_n100 <- simula(sample_size = sample_size,
                        n_datasets = n_datasets,
                        n_treatment = n_treatment,
                        betas = betas_binomial,
                        n_distances = n_distances,
                        distribution = 'binomial')

poisson_n100 <- simula(sample_size = sample_size,
                       n_datasets = n_datasets,
                       n_treatment = n_treatment,
                       betas = betas_poisson,
                       n_distances = n_distances,
                       distribution = 'poisson')

#----------------------------------------------------------------

sample_size = 250

normal_n250 <- simula(sample_size = sample_size,
                      n_datasets = n_datasets,
                      n_treatment = n_treatment,
                      betas = betas_normal,
                      n_distances = n_distances,
                      distribution = 'normal')

binomial_n250 <- simula(sample_size = sample_size,
                        n_datasets = n_datasets,
                        n_treatment = n_treatment,
                        betas = betas_binomial,
                        n_distances = n_distances,
                        distribution = 'binomial')

poisson_n250 <- simula(sample_size = sample_size,
                       n_datasets = n_datasets,
                       n_treatment = n_treatment,
                       betas = betas_poisson,
                       n_distances = n_distances,
                       distribution = 'poisson')


#----------------------------------------------------------------

sample_size = 500

normal_n500 <- simula(sample_size = sample_size,
                      n_datasets = n_datasets,
                      n_treatment = n_treatment,
                      betas = betas_normal,
                      n_distances = n_distances,
                      distribution = 'normal')


binomial_n500 <- simula(sample_size = sample_size,
                        n_datasets = n_datasets,
                        n_treatment = n_treatment,
                        betas = betas_binomial,
                        n_distances = n_distances,
                        distribution = 'binomial')

poisson_n500 <- simula(sample_size = sample_size,
                       n_datasets = n_datasets,
                       n_treatment = n_treatment,
                       betas = betas_poisson,
                       n_distances = n_distances,
                       distribution = 'poisson')

#----------------------------------------------------------------

sample_size = 1000

normal_n1000 <- simula(sample_size = sample_size,
                       n_datasets = n_datasets,
                       n_treatment = n_treatment,
                       betas = betas_normal,
                       n_distances = n_distances,
                       distribution = 'normal')


binomial_n1000 <- simula(sample_size = sample_size,
                         n_datasets = n_datasets,
                         n_treatment = n_treatment,
                         betas = betas_binomial,
                         n_distances = n_distances,
                         distribution = 'binomial')

poisson_n1000 <- simula(sample_size = sample_size,
                        n_datasets = n_datasets,
                        n_treatment = n_treatment,
                        betas = betas_poisson,
                        n_distances = n_distances,
                        distribution = 'poisson')


#----------------------------------------------------------------

save(
    normal_n50,
    binomial_n50,
    poisson_n50,
    
    normal_n100,
    binomial_n100,
    poisson_n100,
    
    normal_n250,
    binomial_n250,
    poisson_n250,
    
    normal_n500,
    binomial_n500,
    poisson_n500,
     
    normal_n1000,
    binomial_n1000,
    poisson_n1000,
    
    file = "testes_uni.RData")

#----------------------------------------------------------------

png(filename='~/msc/3_th_mcglm/4_estudo_simulacao/testes.png',
    width = 800, height = 500)

par(mfrow = c(3,5), par(oma=c(1,3,3,0),
                        mar=c(5,6,4,1)+.1), cex.lab = 1.8)

grafico(normal_n50, main = '', ylab = '', xlab = '')
mtext('n = 50', side=3, line=3, cex=1.3)
mtext("NORMAL", side=2, line=5, cex=1.3 )

grafico(normal_n100, main = '', xlab = '', ylab = '')
mtext('n = 100', side=3, line=3, cex=1.3)

grafico(normal_n250, main = '', xlab = '', ylab = '')
mtext('n = 250', side=3, line=3, cex=1.3)

grafico(normal_n500, main = '', xlab = '', ylab = '')
mtext('n = 500', side=3, line=3, cex=1.3)

grafico(normal_n1000, main = '', xlab = '', ylab = '')
mtext('n = 1000', side=3, line=3, cex=1.3)

grafico(poisson_n50, main = '', xlab = '', ylab = '% Rejeições')
mtext("POISSON", side=2, line=5, cex=1.3 )
grafico(poisson_n100, main = '', xlab = '', ylab = '')
grafico(poisson_n250, main = '', xlab = '', ylab = '')
grafico(poisson_n500, main = '', xlab = '', ylab = '')
grafico(poisson_n1000, main = '', xlab = '', ylab = '')

grafico(binomial_n50, main = '', xlab = '', ylab = '')
mtext("BINOMIAL", side=2, line=5, cex=1.3 )
grafico(binomial_n100, main = '', xlab = '', ylab = '')
grafico(binomial_n250, main = '', xlab = 'Distância', ylab = '')
grafico(binomial_n500, main = '', xlab = '', ylab = '')
grafico(binomial_n1000, main = '', xlab = '', ylab = '')

dev.off()

#----------------------------------------------------------------

