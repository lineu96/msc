#----------------------------------------------------------------

# TRIVARIADOS

#----------------------------------------------------------------

start_time_tri <- Sys.time()

#----------------------------------------------------------------

sample_size = 50

normal_tri_n50 <- simula_tri_long_normal(sample_size = sample_size,
                                         n_datasets = n_datasets,
                                         n_rep = n_rep,
                                         taus = taus,
                                         n_distances = n_distances,
                                         distribution = 'normal')

normal_tri_n50$df_final

poisson_tri_n50 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                              n_datasets = n_datasets,
                                              n_rep = n_rep,
                                              taus = taus,
                                              n_distances = n_distances,
                                              distribution = 'poisson')

poisson_tri_n50$df_final

bernoulli_tri_n50 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                                n_datasets = n_datasets,
                                                n_rep = n_rep,
                                                taus = taus,
                                                n_distances = n_distances,
                                                distribution = 'bernoulli')

bernoulli_tri_n50$df_final

save.image(file = 'poisson_long_tri_n50.Rdata')

#----------------------------------------------------------------

sample_size = 100

normal_tri_n100 <- simula_tri_long_normal(sample_size = sample_size,
                                          n_datasets = n_datasets,
                                          n_rep = n_rep,
                                          taus = taus,
                                          n_distances = n_distances,
                                          distribution = 'normal')

normal_tri_n100$df_final

poisson_tri_n100 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                               n_datasets = n_datasets,
                                               n_rep = n_rep,
                                               taus = taus,
                                               n_distances = n_distances,
                                               distribution = 'poisson')

poisson_tri_n100$df_final

bernoulli_tri_n100 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                                 n_datasets = n_datasets,
                                                 n_rep = n_rep,
                                                 taus = taus,
                                                 n_distances = n_distances,
                                                 distribution = 'bernoulli')

bernoulli_tri_n100$df_final

save.image(file = 'long_tri_n100.Rdata')

#----------------------------------------------------------------

sample_size = 250

normal_tri_n250 <- simula_tri_long_normal(sample_size = sample_size,
                                          n_datasets = n_datasets,
                                          n_rep = n_rep,
                                          taus = taus,
                                          n_distances = n_distances,
                                          distribution = 'normal')

normal_tri_n250$df_final

poisson_tri_n250 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                               n_datasets = n_datasets,
                                               n_rep = n_rep,
                                               taus = taus,
                                               n_distances = n_distances,
                                               distribution = 'poisson')

poisson_tri_n250$df_final

bernoulli_tri_n250 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                                 n_datasets = n_datasets,
                                                 n_rep = n_rep,
                                                 taus = taus,
                                                 n_distances = n_distances,
                                                 distribution = 'bernoulli')

bernoulli_tri_n250$df_final

save.image(file = 'long_tri_n250.Rdata')

#----------------------------------------------------------------

sample_size = 500

normal_tri_n500 <- simula_tri_long_normal(sample_size = sample_size,
                                          n_datasets = n_datasets,
                                          n_rep = n_rep,
                                          taus = taus,
                                          n_distances = n_distances,
                                          distribution = 'normal')

normal_tri_n500$df_final

poisson_tri_n500 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                               n_datasets = n_datasets,
                                               n_rep = n_rep,
                                               taus = taus,
                                               n_distances = n_distances,
                                               distribution = 'poisson')

poisson_tri_n500$df_final

bernoulli_tri_n500 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                                 n_datasets = n_datasets,
                                                 n_rep = n_rep,
                                                 taus = taus,
                                                 n_distances = n_distances,
                                                 distribution = 'bernoulli')

bernoulli_tri_n500$df_final

save.image(file = 'long_tri_n500.Rdata')

#----------------------------------------------------------------

sample_size = 1000

normal_tri_n1000 <- simula_tri_long_normal(sample_size = sample_size,
                                           n_datasets = n_datasets,
                                           n_rep = n_rep,
                                           taus = taus,
                                           n_distances = n_distances,
                                           distribution = 'normal')

normal_tri_n1000$df_final

poisson_tri_n1000 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                                n_datasets = n_datasets,
                                                n_rep = n_rep,
                                                taus = taus,
                                                n_distances = n_distances,
                                                distribution = 'poisson')

poisson_tri_n1000$df_final

bernoulli_tri_n1000 <- simula_tri_long_pois_binom(sample_size = sample_size,
                                                  n_datasets = n_datasets,
                                                  n_rep = n_rep,
                                                  taus = taus,
                                                  n_distances = n_distances,
                                                  distribution = 'bernoulli')

bernoulli_tri_n1000$df_final

save.image(file = 'long_tri_n1000.Rdata')

#----------------------------------------------------------------

end_time_tri <- Sys.time()

time_taken_tri <- end_time_tri - start_time_tri

#----------------------------------------------------------------