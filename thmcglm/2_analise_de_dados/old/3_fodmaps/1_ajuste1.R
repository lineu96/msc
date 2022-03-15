#---------------------------------------------------------------
# AJUSTE
#---------------------------------------------------------------

# Preditor

#pred_dor_abd <- dor_abd2 ~ frutose2 + lactose2 + manitol2 + sorbitol2 + gos2 + fructano2 + hpylori + dm +  has + apneia + dor_art + idade
pred_nausea <- nausea2 ~ frutose2 + lactose2 + manitol2 + sorbitol2 + gos2 + fructano2 + hpylori + dm +  has + apneia + dor_art + idade
#pred_ronco <- ronco2 ~ frutose2 + lactose2 + manitol2 + sorbitol2 + gos2 + fructano2 + hpylori + dm +  has + apneia + dor_art + idade
#pred_arroto <- arroto2 ~ frutose2 + lactose2 + manitol2 + sorbitol2 + gos2 + fructano2 + hpylori + dm +  has + apneia + dor_art + idade
#pred_constip <- constip2 ~ frutose2 + lactose2 + manitol2 + sorbitol2 + gos2 + fructano2 + hpylori + dm +  has + apneia + dor_art + idade
#pred_diarr <- diarr2 ~ frutose2 + lactose2 + manitol2 + sorbitol2 + gos2 + fructano2 + hpylori + dm +  has + apneia + dor_art + idade
#pred_fe_moles <- fe_moles2 ~ frutose2 + lactose2 + manitol2 + sorbitol2 + gos2 + fructano2 + hpylori + dm +  has + apneia + dor_art + idade
pred_fe_duras <- fe_duras2 ~ frutose2 + lactose2 + manitol2 + sorbitol2 + gos2 + fructano2 + hpylori + dm +  has + apneia + dor_art + idade
pred_urg_defec <- urg_defec2 ~ frutose2 + lactose2 + manitol2 + sorbitol2 + gos2 + fructano2 + hpylori + dm +  has + apneia + dor_art + idade
pred_n_esvaz <- n_esvaz2 ~ frutose2 + lactose2 + manitol2 + sorbitol2 + gos2 + fructano2 + hpylori + dm +  has + apneia + dor_art + idade

#----------------------------------------------------------------

# Matrix linear predictor

Z0 <- mc_id(dados2) # Identidade

#----------------------------------------------------------------

# Ajuste

fit <- 
  mcglm(linear_pred = c(#pred_dor_abd, 
                        pred_nausea,
                        #pred_ronco, 
                        #pred_arroto, 
                        #pred_constip,
                        #pred_diarr, 
                        #pred_fe_moles, 
                        pred_fe_duras,
                        pred_urg_defec, 
                        pred_n_esvaz),
        
        matrix_pred = list(c(Z0),c(Z0),
                           c(Z0),c(Z0)),
        
        link = c("logit", "logit",
                 "logit", "logit"),
        
        variance = c("binomialP", "binomialP", 
                     "binomialP", "binomialP"), 
        
        control_algorithm = list(verbose = T, 
                                 tuning = 0.01,
                                 max_iter = 140,
                                 tol = 0.01
                                 ),
        #power_fixed = c(F,F,F,F),
        data = dados2)

#----------------------------------------------------------------

matplot(fit$IterationCovariance, type = 'l', xlim = c(1,150)) 

#----------------------------------------------------------------

# Resumo do modelo

summary(fit)

coef(fit, type = 'beta')
coef(fit, type = 'tau')
#coef(fit, type = 'power')

#----------------------------------------------------------------