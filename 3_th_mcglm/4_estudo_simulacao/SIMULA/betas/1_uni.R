#----------------------------------------------------------------

# UNIVARIADOS

#----------------------------------------------------------------

start_time_uni <- Sys.time()

#----------------------------------------------------------------

sample_size = 50

normal_uni_n50 <- simula_uni(sample_size = sample_size,
                             n_datasets = n_datasets,
                             n_treatment = n_treatment,
                             betas = betas_normal,
                             n_distances = n_distances,
                             distribution = 'normal')

poisson_uni_n50 <- simula_uni(sample_size = sample_size,
                              n_datasets = n_datasets,
                              n_treatment = n_treatment,
                              betas = betas_poisson,
                              n_distances = n_distances,
                              distribution = 'poisson')

binomial_uni_n50 <- simula_uni(sample_size = sample_size,
                               n_datasets = n_datasets,
                               n_treatment = n_treatment,
                               betas = betas_binomial,
                               n_distances = n_distances,
                               distribution = 'binomial')

#----------------------------------------------------------------

sample_size = 100

normal_uni_n100 <- simula_uni(sample_size = sample_size,
                              n_datasets = n_datasets,
                              n_treatment = n_treatment,
                              betas = betas_normal,
                              n_distances = n_distances,
                              distribution = 'normal')


poisson_uni_n100 <- simula_uni(sample_size = sample_size,
                               n_datasets = n_datasets,
                               n_treatment = n_treatment,
                               betas = betas_poisson,
                               n_distances = n_distances,
                               distribution = 'poisson')

binomial_uni_n100 <- simula_uni(sample_size = sample_size,
                                n_datasets = n_datasets,
                                n_treatment = n_treatment,
                                betas = betas_binomial,
                                n_distances = n_distances,
                                distribution = 'binomial')

#----------------------------------------------------------------

sample_size = 250

normal_uni_n250 <- simula_uni(sample_size = sample_size,
                              n_datasets = n_datasets,
                              n_treatment = n_treatment,
                              betas = betas_normal,
                              n_distances = n_distances,
                              distribution = 'normal')

poisson_uni_n250 <- simula_uni(sample_size = sample_size,
                               n_datasets = n_datasets,
                               n_treatment = n_treatment,
                               betas = betas_poisson,
                               n_distances = n_distances,
                               distribution = 'poisson')

binomial_uni_n250 <- simula_uni(sample_size = sample_size,
                                n_datasets = n_datasets,
                                n_treatment = n_treatment,
                                betas = betas_binomial,
                                n_distances = n_distances,
                                distribution = 'binomial')

#----------------------------------------------------------------

sample_size = 500

normal_uni_n500 <- simula_uni(sample_size = sample_size,
                              n_datasets = n_datasets,
                              n_treatment = n_treatment,
                              betas = betas_normal,
                              n_distances = n_distances,
                              distribution = 'normal')

poisson_uni_n500 <- simula_uni(sample_size = sample_size,
                               n_datasets = n_datasets,
                               n_treatment = n_treatment,
                               betas = betas_poisson,
                               n_distances = n_distances,
                               distribution = 'poisson')

binomial_uni_n500 <- simula_uni(sample_size = sample_size,
                                n_datasets = n_datasets,
                                n_treatment = n_treatment,
                                betas = betas_binomial,
                                n_distances = n_distances,
                                distribution = 'binomial')

#----------------------------------------------------------------

sample_size = 1000

normal_uni_n1000 <- simula_uni(sample_size = sample_size,
                               n_datasets = n_datasets,
                               n_treatment = n_treatment,
                               betas = betas_normal,
                               n_distances = n_distances,
                               distribution = 'normal')

poisson_uni_n1000 <- simula_uni(sample_size = sample_size,
                                n_datasets = n_datasets,
                                n_treatment = n_treatment,
                                betas = betas_poisson,
                                n_distances = n_distances,
                                distribution = 'poisson')

binomial_uni_n1000 <- simula_uni(sample_size = sample_size,
                                 n_datasets = n_datasets,
                                 n_treatment = n_treatment,
                                 betas = betas_binomial,
                                 n_distances = n_distances,
                                 distribution = 'binomial')

#----------------------------------------------------------------

end_time_uni <- Sys.time()

time_taken_uni <- end_time_uni - start_time_uni
time_taken_uni

#----------------------------------------------------------------

save.image(file = "univariados.RData")

#----------------------------------------------------------------
