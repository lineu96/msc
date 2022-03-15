#---------------------------------------------------------------
# RESÍDUOS
#---------------------------------------------------------------

# Obtenção dos resíduos

## chol(vcov) inversa
chol_inv <- Matrix::chol(fit$inv_C)

## Resíduos empilhados
residuos <- as.numeric(residuals(fit, type = 'raw'))

## Produto matricial
pearson <- as.numeric(chol_inv%*%residuos)

## Preditos
preditos <- fit$fitted

## Dataframe
res_pred <- data.frame(index = rep(1:nrow(dados2),4),
                       resp = c(rep('nausea', nrow(dados2)),
                                rep('fe_duras', nrow(dados2)),
                                rep('urg_defec', nrow(dados2)),
                                rep('n_esvaz', nrow(dados2))),
                       observado = c(dados2$nausea2,
                                     dados2$fe_duras2,
                                     dados2$urg_defec2,
                                     dados2$n_esvaz2),
                       preditos = preditos,
                       pearson = pearson,
                       raw = residuos
)

#---------------------------------------------------------------

# AJUSTE À NORMAL DOS RESÍDUOS

## Resíduo Pearson

resp1 = MASS::fitdistr(res_pred$pearson[1:nrow(dados2)], 
                       densfun = "normal")

resp2 = MASS::fitdistr(res_pred$pearson[(nrow(dados2)+1):(nrow(dados2)*2)], 
                       densfun = "normal")

resp3 = MASS::fitdistr(res_pred$pearson[((nrow(dados2)*2)+1):(nrow(dados2)*3)], 
                       densfun = "normal")

resp4 = MASS::fitdistr(res_pred$pearson[((nrow(dados2)*3)+1):(nrow(dados2)*4)], 
                       densfun = "normal")

resp1
resp2
resp3
resp4

## Resíduo cru

resp1 = MASS::fitdistr(res_pred$raw[1:nrow(dados2)], 
                       densfun = "normal")

resp2 = MASS::fitdistr(res_pred$raw[(nrow(dados2)+1):(nrow(dados2)*2)], 
                       densfun = "normal")

resp3 = MASS::fitdistr(res_pred$raw[((nrow(dados2)*2)+1):(nrow(dados2)*3)], 
                       densfun = "normal")

resp4 = MASS::fitdistr(res_pred$raw[((nrow(dados2)*3)+1):(nrow(dados2)*4)], 
                       densfun = "normal")

resp1
resp2
resp3
resp4

#---------------------------------------------------------------

# HISTOGRAMAS

## Resíduo Pearson

g1 <- ggplot(data = res_pred, aes(x=pearson))+
  geom_histogram(fill=1,alpha=0.5, col = 1)+
  theme_bw()+
  xlab('')+
  ylab('')+
  geom_vline(xintercept = 0, col = 2, lty = 2, lwd = 1)+
  facet_wrap(~resp, scales = 'free', ncol = 4) + 
  ggtitle('Resíduo Pearson')

## Resíduo cru

g2 <- ggplot(data = res_pred, aes(x=raw))+
  geom_histogram(fill=1,alpha=0.5, col = 1)+
  theme_bw()+
  xlab('')+
  ylab('')+
  geom_vline(xintercept = 0, col = 2, lty = 2, lwd = 1)+
  facet_wrap(~resp, scales = 'free', ncol = 4) + 
  ggtitle('Resíduo cru')

ggpubr::ggarrange(g1,g2, nrow = 2)

#---------------------------------------------------------------

# QQ-PLOT

## Resíduo Pearson

g1 <- ggplot(data = res_pred, 
             mapping = aes(sample = pearson)) +
  geom_qq(alpha = 0.5) + geom_qq_line(col = 2)+
  theme_bw() +
  xlab('')+
  ylab('Quantis amostrais') + 
  ggtitle('Resíduo Pearson') + facet_wrap(~resp, scales = 'free', ncol = 4)

## Resíduo cru

g2 <- ggplot(data = res_pred, 
             mapping = aes(sample = raw)) +
  geom_qq(alpha = 0.5) + geom_qq_line(col = 2)+
  theme_bw() +
  xlab('Quantis teóricos')+
  ylab('Quantis amostrais') + 
  ggtitle('Resíduo cru') + facet_wrap(~resp, scales = 'free', ncol = 4)

ggpubr::ggarrange(g1,g2, nrow = 2)

#---------------------------------------------------------------

## RESIDUOS vs PREDITO

g1 <- ggplot(data = res_pred, aes(y=pearson,x=preditos))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'loess', se=F)+
  xlab('')+
  ylab('Resíduos') + 
  ggtitle('Resíduo Pearson') + 
  facet_wrap(~resp, scales = 'free', ncol = 4)

g2 <- ggplot(data = res_pred, aes(y=raw,x=preditos))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'loess', se=F)+
  xlab('Preditos')+
  ylab('Resíduos') + 
  ggtitle('Resíduo cru') + 
  facet_wrap(~resp, scales = 'free', ncol = 4)

ggpubr::ggarrange(g1,g2, nrow = 2)

#---------------------------------------------------------------

## RESIDUOS vs ÍNDICE

g1 <- ggplot(data = res_pred, aes(y=pearson,x=index))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'loess', se=F)+
  xlab('')+
  ylab('Resíduos') + 
  ggtitle('Resíduo Pearson') + 
  facet_wrap(~resp, scales = 'free', ncol = 4)

g2 <- ggplot(data = res_pred, aes(y=raw,x=index))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'loess', se=F)+
  xlab('Preditos')+
  ylab('Resíduos') + 
  ggtitle('Resíduo cru') + 
  facet_wrap(~resp, scales = 'free', ncol = 4)

ggpubr::ggarrange(g1,g2, nrow = 2)

#---------------------------------------------------------------
