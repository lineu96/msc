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
                             distribution = 'normal',
                             decrease = decrease_normal)

poisson_uni_n50 <- simula_uni(sample_size = sample_size,
                              n_datasets = n_datasets,
                              n_treatment = n_treatment,
                              betas = betas_poisson,
                              n_distances = n_distances,
                              distribution = 'poisson',
                              decrease = decrease_poisson)

bernoulli_uni_n50 <- simula_uni(sample_size = sample_size,
                               n_datasets = n_datasets,
                               n_treatment = n_treatment,
                               betas = betas_bernoulli,
                               n_distances = n_distances,
                               distribution = 'bernoulli',
                               decrease = decrease_bernoulli)

#----------------------------------------------------------------

sample_size = 100

normal_uni_n100 <- simula_uni(sample_size = sample_size,
                              n_datasets = n_datasets,
                              n_treatment = n_treatment,
                              betas = betas_normal,
                              n_distances = n_distances,
                              distribution = 'normal',
                              decrease = decrease_normal)


poisson_uni_n100 <- simula_uni(sample_size = sample_size,
                               n_datasets = n_datasets,
                               n_treatment = n_treatment,
                               betas = betas_poisson,
                               n_distances = n_distances,
                               distribution = 'poisson',
                               decrease = decrease_poisson)

bernoulli_uni_n100 <- simula_uni(sample_size = sample_size,
                                n_datasets = n_datasets,
                                n_treatment = n_treatment,
                                betas = betas_bernoulli,
                                n_distances = n_distances,
                                distribution = 'bernoulli',
                                decrease = decrease_bernoulli)

#----------------------------------------------------------------

sample_size = 250

normal_uni_n250 <- simula_uni(sample_size = sample_size,
                              n_datasets = n_datasets,
                              n_treatment = n_treatment,
                              betas = betas_normal,
                              n_distances = n_distances,
                              distribution = 'normal',
                              decrease = decrease_normal)

poisson_uni_n250 <- simula_uni(sample_size = sample_size,
                               n_datasets = n_datasets,
                               n_treatment = n_treatment,
                               betas = betas_poisson,
                               n_distances = n_distances,
                               distribution = 'poisson',
                               decrease = decrease_poisson)

bernoulli_uni_n250 <- simula_uni(sample_size = sample_size,
                                n_datasets = n_datasets,
                                n_treatment = n_treatment,
                                betas = betas_bernoulli,
                                n_distances = n_distances,
                                distribution = 'bernoulli',
                                decrease = decrease_bernoulli)

#----------------------------------------------------------------

sample_size = 500

normal_uni_n500 <- simula_uni(sample_size = sample_size,
                              n_datasets = n_datasets,
                              n_treatment = n_treatment,
                              betas = betas_normal,
                              n_distances = n_distances,
                              distribution = 'normal',
                              decrease = decrease_normal)

poisson_uni_n500 <- simula_uni(sample_size = sample_size,
                               n_datasets = n_datasets,
                               n_treatment = n_treatment,
                               betas = betas_poisson,
                               n_distances = n_distances,
                               distribution = 'poisson',
                               decrease = decrease_poisson)

bernoulli_uni_n500 <- simula_uni(sample_size = sample_size,
                                n_datasets = n_datasets,
                                n_treatment = n_treatment,
                                betas = betas_bernoulli,
                                n_distances = n_distances,
                                distribution = 'bernoulli',
                                decrease = decrease_bernoulli)

#----------------------------------------------------------------

sample_size = 1000

normal_uni_n1000 <- simula_uni(sample_size = sample_size,
                               n_datasets = n_datasets,
                               n_treatment = n_treatment,
                               betas = betas_normal,
                               n_distances = n_distances,
                               distribution = 'normal',
                               decrease = decrease_normal)

poisson_uni_n1000 <- simula_uni(sample_size = sample_size,
                                n_datasets = n_datasets,
                                n_treatment = n_treatment,
                                betas = betas_poisson,
                                n_distances = n_distances,
                                distribution = 'poisson',
                                decrease = decrease_poisson)

bernoulli_uni_n1000 <- simula_uni(sample_size = sample_size,
                                 n_datasets = n_datasets,
                                 n_treatment = n_treatment,
                                 betas = betas_bernoulli,
                                 n_distances = n_distances,
                                 distribution = 'bernoulli',
                                 decrease = decrease_bernoulli)

#----------------------------------------------------------------

end_time_uni <- Sys.time()

time_taken_uni <- end_time_uni - start_time_uni

#----------------------------------------------------------------

save.image(file = 'results_uni.Rdata')

#----------------------------------------------------------------