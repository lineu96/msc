#----------------------------------------------------------------

# Testes

#----------------------------------------------------------------

teste_prot1 <- prototipo1_lm(sample_size = 200,
                             n_datasets = 1000,
                             variance_error = 1.5,
                             betas = c(5,0,0,0),
                             dif_effects = 0.5,
                             outcome_type = NULL)

teste_prot2 <- prototipo2_lm(sample_size = 200,
                             n_datasets = 1000,
                             variance_error = 1.5,
                             betas = c(5,0,0,0),
                             dif_effects = 0.5,
                             outcome_type = NULL)

#----------------------------------------------------------------

grafico(teste_prot1)
grafico(teste_prot2)

#----------------------------------------------------------------
