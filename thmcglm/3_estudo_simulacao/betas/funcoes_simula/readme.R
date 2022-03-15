#----------------------------------------------------------------

# Funções para estudo de simulação teste wald para mcglm

#----------------------------------------------------------------

# Simula de um modelo, varia hipótese

#----------------------------------------------------------------

# Considera um modelo em que há uma variável explicativa categórica

# Simula conjuntos de dados para o caso em que beta0 é igual a um 
# valor e os demais betas são 0

# Testa a hipótese de que os betas são iguais aos betas simulados

# Altera a hipótese: faz um decréscimo em beta 0, distribui esse
# decrécimo igualmente entre os demais betas

# Repete este procedimento algumas vezes

# Para cada uma das vezes toma a distância euclideana do vetor de 
# betas original para o modificado

# Para cada ponto avalia quantas vezes houve rejeição da hipótese
# nula

#----------------------------------------------------------------

# Argumentos

# sample_size - tamanho das amostras
# n_datasets - numero de conjuntos de dados
# n_treatment - número de tratamentos
# betas - valores dos parametros de regressao
# n_distances - número de distâncias (vai definir decréscimo em beta 0
#                               para distribuição nos demais betas)
# distribution - distribuição (normal, poisson, binomial n=1, beta)

#----------------------------------------------------------------

# Funções

# simula_uni
# simula_tri_normal
# simula_tri_pois_binom
# simula_tri

#----------------------------------------------------------------

