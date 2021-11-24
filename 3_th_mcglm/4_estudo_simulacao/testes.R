#----------------------------------------------------------------

# Testes

# Protótipo 1
# sample_size = 500
# n_datasets = 100
# dif_effects = 0.2 (25 PONTOS)
# 30 SEGUNDOS

# Protótipo 2
# sample_size = 50
# n_datasets = 100
# dif_effects = 0.2 (25 PONTOS)
# 6 MINUTOS E 30 SEGUNDOS

#----------------------------------------------------------------

prot1_n50 <- prototipo1_lm(sample_size = 50,
                             n_datasets = 100,
                             variance_error = 1.5,
                             betas = c(5,0,0,0),
                             dif_effects = 0.2,
                             outcome_type = NULL)

prot2_n50 <- prototipo2_lm(sample_size = 50,
                             n_datasets = 100,
                             variance_error = 1.5,
                             betas = c(5,0,0,0),
                             dif_effects = 0.2,
                             outcome_type = NULL)

prot1_n100 <- prototipo1_lm(sample_size = 100,
                           n_datasets = 100,
                           variance_error = 1.5,
                           betas = c(5,0,0,0),
                           dif_effects = 0.2,
                           outcome_type = NULL)

prot2_n100 <- prototipo2_lm(sample_size = 100,
                           n_datasets = 100,
                           variance_error = 1.5,
                           betas = c(5,0,0,0),
                           dif_effects = 0.2,
                           outcome_type = NULL)

prot1_n500 <- prototipo1_lm(sample_size = 500,
                            n_datasets = 100,
                            variance_error = 1.5,
                            betas = c(5,0,0,0),
                            dif_effects = 0.2,
                            outcome_type = NULL)

prot2_n500 <- prototipo2_lm(sample_size = 500,
                            n_datasets = 100,
                            variance_error = 1.5,
                            betas = c(5,0,0,0),
                            dif_effects = 0.2,
                            outcome_type = NULL)

#----------------------------------------------------------------

x11()

#----------------------------------------------------------------

par(mfrow=c(1,2),oma = c(0, 0, 2, 0))

grafico2(prot1_n50, main = 'Simula de um modelo, varia hipótese')
grafico2(prot2_n50, main = 'Varia o modelo, testa mesma hipótese')

mtext("100 datasets, n = 50", outer = TRUE, cex = 1.5)

#----------------------------------------------------------------

par(mfrow=c(1,2),oma = c(0, 0, 2, 0))

grafico2(prot1_n100, main = 'Simula de um modelo, varia hipótese')
grafico2(prot2_n100, main = 'Varia o modelo, testa mesma hipótese')

mtext("100 datasets, n = 100", outer = TRUE, cex = 1.5)

#----------------------------------------------------------------

par(mfrow=c(1,2),oma = c(0, 0, 2, 0))

grafico2(prot1_n500, main = 'Simula de um modelo, varia hipótese')
grafico2(prot2_n500, main = 'Varia o modelo, testa mesma hipótese')

mtext("100 datasets, n = 500", outer = TRUE, cex = 1.5)

#----------------------------------------------------------------
