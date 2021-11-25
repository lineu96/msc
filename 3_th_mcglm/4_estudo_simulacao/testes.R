#----------------------------------------------------------------
source('~/msc/3_th_mcglm/4_estudo_simulacao/libs.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/simula_distr/simula_normal.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/simula_distr/simula_poisson.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/simula_distr/simula_binomial.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/simula_distr/simula_beta.R')
source('~/msc/3_th_mcglm/4_estudo_simulacao/analises/grafico.R')
#----------------------------------------------------------------

# Testes

sample_size = 500
n_datasets = 1000
n_trat = 4
betas = c(5,0, 0, 0)
dif_effects = 0.1

#----------------------------------------------------------------

normal_n25 <- simula_normal(sample_size = sample_size,
                            n_datasets = n_datasets,
                            n_trat = n_trat,
                            betas = betas,
                            dif_effects = dif_effects)

poisson_n25 <- simula_poisson(sample_size = sample_size,
                              n_datasets = n_datasets,
                              n_trat = n_trat,
                              betas = betas,
                              dif_effects = dif_effects)

binomial_n25 <- simula_binomial(sample_size = sample_size,
                                n_datasets = n_datasets,
                                n_trat = n_trat,
                                betas = betas,
                                dif_effects = dif_effects)

beta_n25 <- simula_beta(sample_size = sample_size,
                        n_datasets = n_datasets,
                        n_trat = n_trat,
                        betas = betas,
                        dif_effects = dif_effects)

#----------------------------------------------------------------

x11()

#----------------------------------------------------------------

par(mfrow=c(2,2),oma = c(0, 0, 2, 0))

grafico(normal_n25, main = 'Normal')
grafico(poisson_n25, main = 'Poisson')
grafico(binomial_n25, main = 'Binomial')
grafico(beta_n25, main = 'Beta')

mtext("1000 datasets, n = 500", outer = TRUE, cex = 1.5)

#----------------------------------------------------------------

