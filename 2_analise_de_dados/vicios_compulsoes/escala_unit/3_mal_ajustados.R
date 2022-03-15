#---------------------------------------------------------------
# MAL AJUSTADOS
#---------------------------------------------------------------

dados_dissertacao$pred_YFAS <- res_pred$preditos[1:nrow(dados_dissertacao)]
dados_dissertacao$pred_BES <- res_pred$preditos[(nrow(dados_dissertacao)+1):nrow(res_pred)]

dados_dissertacao$pearson_YFAS <- res_pred$pearson[1:nrow(dados_dissertacao)]
dados_dissertacao$pearson_BES <- res_pred$pearson[(nrow(dados_dissertacao)+1):nrow(res_pred)]

dados_dissertacao$raw_YFAS <- res_pred$raw[1:nrow(dados_dissertacao)]
dados_dissertacao$raw_BES <- res_pred$raw[(nrow(dados_dissertacao)+1):nrow(res_pred)]

mal_ajustados <- subset(dados_dissertacao,
                         pearson_YFAS   >  2    |
                           pearson_YFAS < -2    |
                           pearson_BES  >  2 |
                           pearson_BES  < -2
                        )

nrow(mal_ajustados)

mal_ajustados

#---------------------------------------------------------------