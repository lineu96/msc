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
                                    distribution = 'normal')

poisson_tri_n50 <- simula_tri_pois_binom(sample_size = sample_size,
                                         n_datasets = n_datasets,
                                         n_treatment = n_treatment,
                                         betas = betas_poisson,
                                         n_distances = n_distances,
                                         distribution = 'poisson')

binomial_tri_n50 <- simula_tri_pois_binom(sample_size = sample_size,
                                          n_datasets = n_datasets,
                                          n_treatment = n_treatment,
                                          betas = betas_binomial,
                                          n_distances = n_distances,
                                          distribution = 'binomial')

#----------------------------------------------------------------

sample_size = 100

normal_tri_n100 <- simula_tri_normal(sample_size = sample_size,
                                     n_datasets = n_datasets,
                                     n_treatment = n_treatment,
                                     betas = betas_normal,
                                     n_distances = n_distances,
                                     distribution = 'normal')

poisson_tri_n100 <- simula_tri_pois_binom(sample_size = sample_size,
                                          n_datasets = n_datasets,
                                          n_treatment = n_treatment,
                                          betas = betas_poisson,
                                          n_distances = n_distances,
                                          distribution = 'poisson')

binomial_tri_n100 <- simula_tri_pois_binom(sample_size = sample_size,
                                           n_datasets = n_datasets,
                                           n_treatment = n_treatment,
                                           betas = betas_binomial,
                                           n_distances = n_distances,
                                           distribution = 'binomial')

#----------------------------------------------------------------

sample_size = 250

normal_tri_n250 <- simula_tri_normal(sample_size = sample_size,
                                     n_datasets = n_datasets,
                                     n_treatment = n_treatment,
                                     betas = betas_normal,
                                     n_distances = n_distances,
                                     distribution = 'normal')

poisson_tri_n250 <- simula_tri_pois_binom(sample_size = sample_size,
                                          n_datasets = n_datasets,
                                          n_treatment = n_treatment,
                                          betas = betas_poisson,
                                          n_distances = n_distances,
                                          distribution = 'poisson')

binomial_tri_n250 <- simula_tri_pois_binom(sample_size = sample_size,
                                           n_datasets = n_datasets,
                                           n_treatment = n_treatment,
                                           betas = betas_binomial,
                                           n_distances = n_distances,
                                           distribution = 'binomial')

#----------------------------------------------------------------

sample_size = 500

normal_tri_n500 <- simula_tri_normal(sample_size = sample_size,
                                     n_datasets = n_datasets,
                                     n_treatment = n_treatment,
                                     betas = betas_normal,
                                     n_distances = n_distances,
                                     distribution = 'normal')

poisson_tri_n500 <- simula_tri_pois_binom(sample_size = sample_size,
                                          n_datasets = n_datasets,
                                          n_treatment = n_treatment,
                                          betas = betas_poisson,
                                          n_distances = n_distances,
                                          distribution = 'poisson')

binomial_tri_n500 <- simula_tri_pois_binom(sample_size = sample_size,
                                           n_datasets = n_datasets,
                                           n_treatment = n_treatment,
                                           betas = betas_binomial,
                                           n_distances = n_distances,
                                           distribution = 'binomial')

#----------------------------------------------------------------

sample_size = 1000

normal_tri_n1000 <- simula_tri_normal(sample_size = sample_size,
                                      n_datasets = n_datasets,
                                      n_treatment = n_treatment,
                                      betas = betas_normal,
                                      n_distances = n_distances,
                                      distribution = 'normal')

poisson_tri_n1000 <- simula_tri_pois_binom(sample_size = sample_size,
                                           n_datasets = n_datasets,
                                           n_treatment = n_treatment,
                                           betas = betas_poisson,
                                           n_distances = n_distances,
                                           distribution = 'poisson')

binomial_tri_n1000 <- simula_tri_pois_binom(sample_size = sample_size,
                                            n_datasets = n_datasets,
                                            n_treatment = n_treatment,
                                            betas = betas_binomial,
                                            n_distances = n_distances,
                                            distribution = 'binomial')

#----------------------------------------------------------------

end_time_tri <- Sys.time()

time_taken_tri <- end_time_tri - start_time_tri

#----------------------------------------------------------------

save.image(file = 'tri1.Rdata')

#----------------------------------------------------------------