#---------------------------------------------------------------
# MAL AJUSTADOS
#---------------------------------------------------------------

massa$pred_min <- res_pred$preditos[1:nrow(massa)]
massa$pred_max <- res_pred$preditos[(nrow(massa)+1):nrow(res_pred)]

massa$pearson_min <- res_pred$pearson[1:nrow(massa)]
massa$pearson_max <- res_pred$pearson[(nrow(massa)+1):nrow(res_pred)]

massa$raw_min <- res_pred$raw[1:nrow(massa)]
massa$raw_max <- res_pred$raw[(nrow(massa)+1):nrow(res_pred)]

massa[,c(14,17,21,19,
         15,18,22,20)]

mal_ajustados <- subset(massa, 
                        pearson_min > 3  | 
                        pearson_max > 3  |   
                        pearson_min < -3 | 
                        pearson_max < -3 |   
                        pred_min > 40    |
                        pred_max > 40
                        )

mal_ajustados[,c(14,17,21,19,
                 15,18,22,20)]

mal_ajustados$n
