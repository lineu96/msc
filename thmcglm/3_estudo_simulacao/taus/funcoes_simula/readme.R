#----------------------------------------------------------------

# Funções para estudo de simulação teste wald para mcglm

#----------------------------------------------------------------

# Simula de um modelo, varia hipótese

#----------------------------------------------------------------

# Considera um modelo sem variáveis explicativas

# Simula conjuntos de dados fixando tau0 e tau1 em 0.5

# Testa a hipótese de que os betas são iguais aos betas simulados

# Altera a hipótese: faz um decréscimo nos taus

# Repete este procedimento algumas vezes

# Para cada uma das vezes toma a distância euclideana do vetor de 
# betas original para o modificado

# Para cada ponto avalia quantas vezes houve rejeição da hipótese
# nula

#----------------------------------------------------------------

# Argumentos

# sample_size - tamanho das amostras
# n_datasets - numero de conjuntos de dados
# n_rep - número de medidas tomada em cada observação (caso longitudinal)
# taus - valores dos parametros de dispersão
# n_distances - número de distâncias (vai definir decréscimo nos taus)
# distribution - distribuição (normal, poisson, binomial n=1, beta)

#----------------------------------------------------------------

# Funções

# simula_uni_long_pois_binom

#----------------------------------------------------------------

