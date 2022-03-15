#---------------------------------------------------------------
# MAL AJUSTADOS
#---------------------------------------------------------------

dados4$pred_yale <- res_pred$preditos[1:nrow(dados4)]
dados4$pred_ecap <- res_pred$preditos[(nrow(dados4)+1):nrow(res_pred)]

dados4$pearson_yale <- res_pred$pearson[1:nrow(dados4)]
dados4$pearson_ecap <- res_pred$pearson[(nrow(dados4)+1):nrow(res_pred)]

dados4$raw_yale <- res_pred$raw[1:nrow(dados4)]
dados4$raw_ecap <- res_pred$raw[(nrow(dados4)+1):nrow(res_pred)]

mal_ajustados <- subset(dados4,
                         pearson_yale   >  2    |
                           pearson_yale < -2    |
                           pearson_ecap  >  2 |
                           pearson_ecap  < -2
                        )

nrow(mal_ajustados)

mal_ajustados

#---------------------------------------------------------------