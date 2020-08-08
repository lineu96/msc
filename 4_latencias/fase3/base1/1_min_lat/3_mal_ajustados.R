#---------------------------------------------------------------
# MAL AJUSTADOS
#---------------------------------------------------------------

massa$pred_min <- res_pred$preditos
massa$res_pearson <- res_pred$pearson
massa$res_raw <- res_pred$raw

#massa[,-c(1,4)]

names(massa)

mal_ajustados <- subset(massa, 
                        res_raw > 4  | 
                        res_raw < -4
                        )


mal_ajustados[,-c(1,4)]

mal_ajustados$n

#---------------------------------------------------------------

# Divisão da nova base massa e analista (com mal ajustados)

indices2 <- subset(dados, dados$min_lat<=25 & 
                     dados$max_lat<=45)$n

massa2 <- subset(dados, dados$n %in% indices & !(dados$n %in% mal_ajustados$n))

analista2 <- subset(dados, !(dados$n %in% indices) | (dados$n %in% mal_ajustados$n))

nrow(massa)
nrow(massa2)

nrow(analista)
nrow(analista2)

#---------------------------------------------------------------
