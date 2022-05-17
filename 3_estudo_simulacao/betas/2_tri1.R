#----------------------------------------------------------------

# TRIVARIADOS

#----------------------------------------------------------------

start_time_tri <- Sys.time()

#----------------------------------------------------------------

sample_size = 50

normal_tri_n50 <- simula_tri_normal(sample_size = sample_size,
                                    n_datasets = n_datasets,
                                    n_treatment = n_treatment,
                                    betas = betas_normal,
                                    n_distances = n_distances,
                                    distribution = 'normal',
                                    decrease = decrease_normal)

poisson_tri_n50 <- simula_tri_pois_binom(sample_size = sample_size,
                                         n_datasets = n_datasets,
                                         n_treatment = n_treatment,
                                         betas = betas_poisson,
                                         n_distances = n_distances,
                                         distribution = 'poisson',
                                         decrease = decrease_poisson)

bernoulli_tri_n50 <- simula_tri_pois_binom(sample_size = sample_size,
                                           n_datasets = n_datasets,
                                           n_treatment = n_treatment,
                                           betas = betas_bernoulli,
                                           n_distances = n_distances,
                                           distribution = 'bernoulli',
                                           decrease = decrease_bernoulli)

save.image(file = 'results_tri_pois_binom50.Rdata')

#----------------------------------------------------------------

sample_size = 100

normal_tri_n100 <- simula_tri_normal(sample_size = sample_size,
                                     n_datasets = n_datasets,
                                     n_treatment = n_treatment,
                                     betas = betas_normal,
                                     n_distances = n_distances,
                                     distribution = 'normal',
                                     decrease = decrease_normal)

poisson_tri_n100 <- simula_tri_pois_binom(sample_size = sample_size,
                                          n_datasets = n_datasets,
                                          n_treatment = n_treatment,
                                          betas = betas_poisson,
                                          n_distances = n_distances,
                                          distribution = 'poisson',
                                          decrease = decrease_poisson)

bernoulli_tri_n100 <- simula_tri_pois_binom(sample_size = sample_size,
                                            n_datasets = n_datasets,
                                            n_treatment = n_treatment,
                                            betas = betas_bernoulli,
                                            n_distances = n_distances,
                                            distribution = 'bernoulli',
                                            decrease = decrease_bernoulli)

save.image(file = 'results_tri_pois_binom100.Rdata')

#----------------------------------------------------------------

sample_size = 250

normal_tri_n250 <- simula_tri_normal(sample_size = sample_size,
                                     n_datasets = n_datasets,
                                     n_treatment = n_treatment,
                                     betas = betas_normal,
                                     n_distances = n_distances,
                                     distribution = 'normal',
                                     decrease = decrease_normal)

poisson_tri_n250 <- simula_tri_pois_binom(sample_size = sample_size,
                                          n_datasets = n_datasets,
                                          n_treatment = n_treatment,
                                          betas = betas_poisson,
                                          n_distances = n_distances,
                                          distribution = 'poisson',
                                          decrease = decrease_poisson)

bernoulli_tri_n250 <- simula_tri_pois_binom(sample_size = sample_size,
                                            n_datasets = n_datasets,
                                            n_treatment = n_treatment,
                                            betas = betas_bernoulli,
                                            n_distances = n_distances,
                                            distribution = 'bernoulli',
                                            decrease = decrease_bernoulli)

save.image(file = 'results_tri_pois_binom250.Rdata')

#----------------------------------------------------------------

sample_size = 500

normal_tri_n500 <- simula_tri_normal(sample_size = sample_size,
                                     n_datasets = n_datasets,
                                     n_treatment = n_treatment,
                                     betas = betas_normal,
                                     n_distances = n_distances,
                                     distribution = 'normal',
                                     decrease = decrease_normal)

poisson_tri_n500 <- simula_tri_pois_binom(sample_size = sample_size,
                                          n_datasets = n_datasets,
                                          n_treatment = n_treatment,
                                          betas = betas_poisson,
                                          n_distances = n_distances,
                                          distribution = 'poisson',
                                          decrease = decrease_poisson)

bernoulli_tri_n500 <- simula_tri_pois_binom(sample_size = sample_size,
                                            n_datasets = n_datasets,
                                            n_treatment = n_treatment,
                                            betas = betas_bernoulli,
                                            n_distances = n_distances,
                                            distribution = 'bernoulli',
                                            decrease = decrease_bernoulli)

save.image(file = 'results_pois_binom500.Rdata')

#----------------------------------------------------------------

sample_size = 1000

normal_tri_n1000 <- simula_tri_normal(sample_size = sample_size,
                                      n_datasets = n_datasets,
                                      n_treatment = n_treatment,
                                      betas = betas_normal,
                                      n_distances = n_distances,
                                      distribution = 'normal',
                                      decrease = decrease_normal)

poisson_tri_n1000 <- simula_tri_pois_binom(sample_size = sample_size,
                                           n_datasets = n_datasets,
                                           n_treatment = n_treatment,
                                           betas = betas_poisson,
                                           n_distances = n_distances,
                                           distribution = 'poisson',
                                           decrease = decrease_poisson)

bernoulli_tri_n1000 <- simula_tri_pois_binom(sample_size = sample_size,
                                             n_datasets = n_datasets,
                                             n_treatment = n_treatment,
                                             betas = betas_bernoulli,
                                             n_distances = n_distances,
                                             distribution = 'bernoulli',
                                             decrease = decrease_bernoulli)

save.image(file = 'results_tri_pois_binom1000.Rdata')

#----------------------------------------------------------------

end_time_tri <- Sys.time()

time_taken_tri <- end_time_tri - start_time_tri

#----------------------------------------------------------------

save.image(file = 'results_tri1.Rdata')

#----------------------------------------------------------------