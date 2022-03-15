#---------------------------------------------------------------
# MAL AJUSTADOS
#---------------------------------------------------------------

data_rbb$n <- 1:nrow(data_rbb)

data_rbb$pred_count <- res_pred$preditos[1:nrow(data_rbb)]
data_rbb$pred_prop <- res_pred$preditos[(nrow(data_rbb)+1):nrow(res_pred)]

data_rbb$pearson_count <- res_pred$pearson[1:nrow(data_rbb)]
data_rbb$pearson_prop <- res_pred$pearson[(nrow(data_rbb)+1):nrow(res_pred)]

data_rbb$raw_count <- res_pred$raw[1:nrow(data_rbb)]
data_rbb$raw_prop <- res_pred$raw[(nrow(data_rbb)+1):nrow(res_pred)]

mal_ajustados <- subset(data_rbb, 
                        pearson_count   >  2    | 
                          pearson_count < -2    |
                          pearson_prop  >  2  | 
                          pearson_prop  < -2
                        )

nrow(mal_ajustados)

mal_ajustados[,c('count', 'pred_count', 'pearson_count',
                 'prop', 'pred_prop', 'pearson_prop')]

# NOVA BASE

data_rbb2 <- subset(data_rbb,  !(data_rbb$n %in% mal_ajustados$n))

analista <- subset(data_rbb,(data_rbb$n %in% mal_ajustados$n))

nrow(data_rbb2)
nrow(analista)
