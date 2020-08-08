#---------------------------------------------------------------
# MAL AJUSTADOS
#---------------------------------------------------------------

massa2$pred_max <- res_pred$preditos
massa2$res_pearson <- res_pred$pearson
massa2$res_raw <- res_pred$raw

#massa[,-c(1,4)]

names(massa2)

mal_ajustados <- subset(massa2,
                        res_raw > 4  | 
                        res_raw < -4
)


mal_ajustados[,-c(1,4)]

mal_ajustados$n

#---------------------------------------------------------------