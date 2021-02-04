#---------------------------------------------------------------
# MAL AJUSTADOS
#---------------------------------------------------------------

dados2$pred_min <- res_pred$preditos[1:nrow(dados2)]
dados2$pred_max <- res_pred$preditos[(nrow(dados2)+1):nrow(res_pred)]

dados2$pearson_min <- res_pred$pearson[1:nrow(dados2)]
dados2$pearson_max <- res_pred$pearson[(nrow(dados2)+1):nrow(res_pred)]

dados2$raw_min <- res_pred$raw[1:nrow(dados2)]
dados2$raw_max <- res_pred$raw[(nrow(dados2)+1):nrow(res_pred)]

mal_ajustados <- subset(dados2,
                        pearson_min   >  2    |
                          pearson_min < -2    |
                          pearson_max  >  2 |
                          pearson_max  < -2 |
                          pred_min > 40|
                          pred_max > 40
)

nrow(mal_ajustados)

# NOVA BASE

massa <- subset(dados2,  !(dados2$n %in% mal_ajustados$n))

analista <- subset(dados2,(dados2$n %in% mal_ajustados$n))

nrow(dados2)
nrow(massa) + nrow(analista)
