#----------------------------------------------------------------

# TRIVARIADOS

#----------------------------------------------------------------

start_time_tri <- Sys.time()

#----------------------------------------------------------------

# DIST NORMAL RODA MAIS RÁPIDO

sample_size = 50

normal_tri_n50 <- simula_tri_long_normal(sample_size = sample_size,
                                         n_datasets = n_datasets,
                                         n_rep = n_rep,
                                         taus = taus,
                                         n_distances = n_distances,
                                         distribution = 'normal')

sample_size = 100

normal_tri_n100 <- simula_tri_long_normal(sample_size = sample_size,
                                          n_datasets = n_datasets,
                                          n_rep = n_rep,
                                          taus = taus,
                                          n_distances = n_distances,
                                          distribution = 'normal')

sample_size = 250

normal_tri_n250 <- simula_tri_long_normal(sample_size = sample_size,
                                          n_datasets = n_datasets,
                                          n_rep = n_rep,
                                          taus = taus,
                                          n_distances = n_distances,
                                          distribution = 'normal')

sample_size = 500

normal_tri_n500 <- simula_tri_long_normal(sample_size = sample_size,
                                          n_datasets = n_datasets,
                                          n_rep = n_rep,
                                          taus = taus,
                                          n_distances = n_distances,
                                          distribution = 'normal')

sample_size = 1000

normal_tri_n1000 <- simula_tri_long_normal(sample_size = sample_size,
                                           n_datasets = n_datasets,
                                           n_rep = n_rep,
                                           taus = taus,
                                           n_distances = n_distances,
                                           distribution = 'normal')

save.image(file = 'tri_normal.Rdata')

#----------------------------------------------------------------

sample_size = 50

poisson_tri_n50 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                              n_datasets = n_datasets,
                                              n_rep = n_rep,
                                              taus = taus,
                                              n_distances = n_distances,
                                              distribution = 'poisson')

binomial_tri_n50 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                               n_datasets = n_datasets,
                                               n_rep = n_rep,
                                               taus = taus,
                                               n_distances = n_distances,
                                               distribution = 'binomial')

save.image(file = 'tri_n50.Rdata')

#----------------------------------------------------------------

sample_size = 100

poisson_tri_n100 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                               n_datasets = n_datasets,
                                               n_rep = n_rep,
                                               taus = taus,
                                               n_distances = n_distances,
                                               distribution = 'poisson')

binomial_tri_n100 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                                n_datasets = n_datasets,
                                                n_rep = n_rep,
                                                taus = taus,
                                                n_distances = n_distances,
                                                distribution = 'binomial')

save.image(file = 'tri_n100.Rdata')

#----------------------------------------------------------------

sample_size = 250

poisson_tri_n250 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                               n_datasets = n_datasets,
                                               n_rep = n_rep,
                                               taus = taus,
                                               n_distances = n_distances,
                                               distribution = 'poisson')

binomial_tri_n250 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                                n_datasets = n_datasets,
                                                n_rep = n_rep,
                                                taus = taus,
                                                n_distances = n_distances,
                                                distribution = 'binomial')

save.image(file = 'tri_n250.Rdata')

#----------------------------------------------------------------

sample_size = 500

poisson_tri_n500 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                               n_datasets = n_datasets,
                                               n_rep = n_rep,
                                               taus = taus,
                                               n_distances = n_distances,
                                               distribution = 'poisson')

binomial_tri_n500 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                                n_datasets = n_datasets,
                                                n_rep = n_rep,
                                                taus = taus,
                                                n_distances = n_distances,
                                                distribution = 'binomial')

save.image(file = 'tri_n500.Rdata')

#----------------------------------------------------------------

sample_size = 1000

poisson_tri_n1000 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                                n_datasets = n_datasets,
                                                n_rep = n_rep,
                                                taus = taus,
                                                n_distances = n_distances,
                                                distribution = 'poisson')

binomial_tri_n1000 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                                 n_datasets = n_datasets,
                                                 n_rep = n_rep,
                                                 taus = taus,
                                                 n_distances = n_distances,
                                                 distribution = 'binomial')

save.image(file = 'tri_n1000.Rdata')

#----------------------------------------------------------------

end_time_tri <- Sys.time()

time_taken_tri <- end_time_tri - start_time_tri

#----------------------------------------------------------------

save.image(file = 'tri1.Rdata')

#----------------------------------------------------------------