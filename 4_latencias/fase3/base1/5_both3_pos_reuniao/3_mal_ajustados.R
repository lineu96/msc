#---------------------------------------------------------------
# MAL AJUSTADOS
#---------------------------------------------------------------

dados$pred_min <- res_pred$preditos[1:nrow(dados)]
dados$pred_max <- res_pred$preditos[(nrow(dados)+1):nrow(res_pred)]

dados$pearson_min <- res_pred$pearson[1:nrow(dados)]
dados$pearson_max <- res_pred$pearson[(nrow(dados)+1):nrow(res_pred)]

dados$raw_min <- res_pred$raw[1:nrow(dados)]
dados$raw_max <- res_pred$raw[(nrow(dados)+1):nrow(res_pred)]

dados[,c(14,17,21,19,
         15,18,22,20)]

mal_ajustados <- subset(dados, 
                        raw_min > 3.5  | 
                          raw_max > 3.5  |   
                          raw_min < -3.5 | 
                          raw_max < -3.5 |   
                        pred_min > 40    |
                        pred_max > 40
                        )

nrow(mal_ajustados)

mal_ajustados <- subset(mal_ajustados, mal_ajustados$count == 0)

nrow(mal_ajustados)

mal_ajustados[,c(14,17,21,19,
                 15,18,22,20)]

mal_ajustados$n

# NOVA BASE

massa <- subset(dados,  !(dados$n %in% mal_ajustados$n))

analista <- subset(dados,(dados$n %in% mal_ajustados$n))

nrow(dados)
nrow(massa) + nrow(analista)
