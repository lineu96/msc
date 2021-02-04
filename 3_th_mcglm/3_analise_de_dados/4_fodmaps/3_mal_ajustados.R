#---------------------------------------------------------------
# MAL AJUSTADOS
#---------------------------------------------------------------

dados2$pred_nausea <- res_pred$preditos[1:nrow(dados2)]
dados2$pred_fe_duras <- res_pred$preditos[(nrow(dados2)+1):(nrow(dados2)*2)]
dados2$pred_urg_defec <- res_pred$preditos[((nrow(dados2)*2)+1):(nrow(dados2)*3)]
dados2$pred_n_esvaz <- res_pred$preditos[((nrow(dados2)*3)+1):(nrow(dados2)*4)]

dados2$pearson_nausea <- res_pred$pearson[1:nrow(dados2)]
dados2$pearson_fe_duras <- res_pred$pearson[(nrow(dados2)+1):(nrow(dados2)*2)]
dados2$pearson_urg_defec <- res_pred$pearson[((nrow(dados2)*2)+1):(nrow(dados2)*3)]
dados2$pearson_n_esvaz <- res_pred$pearson[((nrow(dados2)*3)+1):(nrow(dados2)*4)]

dados2$raw_nausea <- res_pred$raw[1:nrow(dados2)]
dados2$raw_fe_duras <- res_pred$raw[(nrow(dados2)+1):(nrow(dados2)*2)]
dados2$raw_urg_defec <- res_pred$raw[((nrow(dados2)*2)+1):(nrow(dados2)*3)]
dados2$raw_n_esvaz <- res_pred$raw[((nrow(dados2)*3)+1):(nrow(dados2)*4)]


mal_ajustados <- subset(dados2,
                        pearson_nausea > 2 |
                        pearson_nausea < -2 |
                        pearson_fe_duras > 2 |
                        pearson_fe_duras < -2 |
                        pearson_urg_defec > 2 |
                        pearson_urg_defec < -2 |
                        pearson_n_esvaz > 2 |
                        pearson_n_esvaz < -2 
                         )

nrow(mal_ajustados)
names(mal_ajustados)

mal_ajustados[,c("nausea2",
                 "pred_nausea",
                 
                 "fe_duras2",
                 "pred_fe_duras",    
                 
                 "urg_defec2",
                 "pred_urg_defec",
                 
                 "n_esvaz2",
                 "pred_n_esvaz")
              ]



#---------------------------------------------------------------