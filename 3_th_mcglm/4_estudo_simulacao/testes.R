#----------------------------------------------------------------

source('~/msc/3_th_mcglm/4_estudo_simulacao/libs.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/th_mcglm_sim.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/analises/grafico.R')

#----------------------------------------------------------------

# Testes

sample_size = 100
n_datasets = 100
n_treatment = 4
betas_normal = c(5,0,0,0)
betas_poisson = c(1,0,0,0)
betas_binomial = c(0.5,0,0,0)
betas_beta = c(0.5,0,0,0)
n_distances = 20

#----------------------------------------------------------------

normal <- th_mcglm_sim(sample_size = sample_size,
                       n_datasets = n_datasets,
                       n_treatment = n_treatment,
                       betas = betas_normal,
                       n_distances = n_distances,
                       distribution = 'normal')

poisson <- th_mcglm_sim(sample_size = sample_size,
                        n_datasets = n_datasets,
                        n_treatment = n_treatment,
                        betas = betas_poisson,
                        n_distances = n_distances,
                        distribution = 'poisson')

binomial <- th_mcglm_sim(sample_size = sample_size,
                         n_datasets = n_datasets,
                         n_treatment = n_treatment,
                         betas = betas_binomial,
                         n_distances = n_distances,
                         distribution = 'binomial')

beta <- th_mcglm_sim(sample_size = sample_size,
                     n_datasets = n_datasets,
                     n_treatment = n_treatment,
                     betas = betas_beta,
                     n_distances = n_distances,
                     distribution = 'beta')

#----------------------------------------------------------------

x11()

#----------------------------------------------------------------

par(mfrow=c(2,2),oma = c(0, 0, 2, 0))

grafico(normal_n50, main = 'Normal')

grafico(poisson_n50, main = 'Poisson')

grafico(binomial_n50, main = 'Binomial')

grafico(beta_n50, main = 'Beta')

mtext("100 datasets, n = 50", outer = TRUE, cex = 1.5)

#----------------------------------------------------------------

