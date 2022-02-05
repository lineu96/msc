#----------------------------------------------------------------

# TRIVARIADOS COM DIFERENTES DISTRIBUIÇÕES

#----------------------------------------------------------------

start_time_tri2 <- Sys.time()

#----------------------------------------------------------------

sample_size = 50

tri_n50 <- simula_tri(sample_size = sample_size,
                      n_datasets = n_datasets,
                      n_treatment = n_treatment,
                      betas_normal = betas_normal,
                      betas_poisson = betas_poisson,
                      betas_binomial = betas_binomial,
                      n_distances = n_distances)

#----------------------------------------------------------------

sample_size = 100

tri_n100 <- simula_tri(sample_size = sample_size,
                       n_datasets = n_datasets,
                       n_treatment = n_treatment,
                       betas_normal = betas_normal,
                       betas_poisson = betas_poisson,
                       betas_binomial = betas_binomial,
                       n_distances = n_distances)

#----------------------------------------------------------------

sample_size = 250

tri_n250 <- simula_tri(sample_size = sample_size,
                       n_datasets = n_datasets,
                       n_treatment = n_treatment,
                       betas_normal = betas_normal,
                       betas_poisson = betas_poisson,
                       betas_binomial = betas_binomial,
                       n_distances = n_distances)

#----------------------------------------------------------------

sample_size = 500

tri_n500 <- simula_tri(sample_size = sample_size,
                       n_datasets = n_datasets,
                       n_treatment = n_treatment,
                       betas_normal = betas_normal,
                       betas_poisson = betas_poisson,
                       betas_binomial = betas_binomial,
                       n_distances = n_distances)

#----------------------------------------------------------------

sample_size = 1000

tri_n1000 <- simula_tri(sample_size = sample_size,
                        n_datasets = n_datasets,
                        n_treatment = n_treatment,
                        betas_normal = betas_normal,
                        betas_poisson = betas_poisson,
                        betas_binomial = betas_binomial,
                        n_distances = n_distances)


#----------------------------------------------------------------

end_time_tri2 <- Sys.time()

time_taken_tri2 <- end_time_tri2 - start_time_tri2

#----------------------------------------------------------------

save.image(file = 'tri2.Rdata')

#----------------------------------------------------------------