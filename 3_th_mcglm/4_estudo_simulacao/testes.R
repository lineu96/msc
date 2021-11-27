#----------------------------------------------------------------
source('~/msc/3_th_mcglm/4_estudo_simulacao/libs.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/simula_distr/simula_normal.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/simula_distr/simula_poisson.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/simula_distr/simula_binomial.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/simula_distr/simula_beta.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/analises/grafico.R')
#----------------------------------------------------------------

# Testes

sample_size = 50
n_datasets = 1000
n_trat = 4
betas = c(5,0,0,0)
dif_effects = 0.25
n_dists = 20

#----------------------------------------------------------------

normal_n50_antigo <- simula_normal(sample_size = sample_size,
                                   n_datasets = n_datasets,
                                   n_trat = n_trat,
                                   betas = betas,
                                   dif_effects = dif_effects)

normal_n50_novo <- th_mcglm_sim(sample_size = sample_size,
                                n_datasets = n_datasets,
                                n_trat = n_trat,
                                betas = betas,
                                n_dists = n_dists,
                                distribuicao = 'normal')

poisson_n50_antigo <- simula_poisson(sample_size = sample_size,
                                     n_datasets = n_datasets,
                                     n_trat = n_trat,
                                     betas = betas,
                                     dif_effects = dif_effects)

poisson_n50_novo <- th_mcglm_sim(sample_size = sample_size,
                                 n_datasets = n_datasets,
                                 n_trat = n_trat,
                                 betas = betas,
                                 n_dists = n_dists,
                                 distribuicao = 'poisson')

binomial_n50_antigo <- simula_binomial(sample_size = sample_size,
                                       n_datasets = n_datasets,
                                       n_trat = n_trat,
                                       betas = betas,
                                       dif_effects = dif_effects)

binomial_n50_novo <- th_mcglm_sim(sample_size = sample_size,
                                  n_datasets = n_datasets,
                                  n_trat = n_trat,
                                  betas = betas,
                                  n_dists = n_dists,
                                  distribuicao = 'binomial')

beta_n50_antigo <- simula_beta(sample_size = sample_size,
                               n_datasets = n_datasets,
                               n_trat = n_trat,
                               betas = betas,
                               dif_effects = dif_effects)

beta_n50_novo <- th_mcglm_sim(sample_size = sample_size,
                              n_datasets = n_datasets,
                              n_trat = n_trat,
                              betas = betas,
                              n_dists = n_dists,
                              distribuicao = 'beta')

#----------------------------------------------------------------

x11()

#----------------------------------------------------------------

par(mfrow=c(2,4),oma = c(0, 0, 2, 0))

grafico(normal_n50_antigo, main = 'Normal antigo')
grafico(normal_n50_novo, main = 'Normal novo')

grafico(poisson_n50_antigo, main = 'Poisson antigo')
grafico(poisson_n50_novo, main = 'Poisson novo')

grafico(binomial_n50_antigo, main = 'Binomial antigo')
grafico(binomial_n50_novo, main = 'Binomial novo')

grafico(beta_n50_antigo, main = 'Beta antigo')
grafico(beta_n50_novo, main = 'Beta novo')

mtext("1000 datasets, n = 100", outer = TRUE, cex = 1.5)

#----------------------------------------------------------------

