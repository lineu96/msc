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
res_pred <- data.frame(index = rep(1:nrow(data_rbb),2),
                       resp = c(rep('count', nrow(data_rbb)),
                                rep('prop', nrow(data_rbb))),
                       observado = c(data_rbb$count,
                                     data_rbb$prop),
                       preditos = preditos,
                       pearson = pearson,
                       raw = residuos
                      )

#---------------------------------------------------------------

# AJUSTE À NORMAL DOS RESÍDUOS

## Resíduo Pearson

mean1 = MASS::fitdistr(res_pred$pearson[1:180], 
                       densfun = "normal")$estimate[1]

sd1 = MASS::fitdistr(res_pred$pearson[1:180], 
                     densfun = "normal")$estimate[2]

mean2 = MASS::fitdistr(res_pred$pearson[181:360], 
                       densfun = "normal")$estimate[1]

sd2 = MASS::fitdistr(res_pred$pearson[181:360], 
                     densfun = "normal")$estimate[2]

mean1
sd1

mean2
sd2

## Resíduo cru

mean3 = MASS::fitdistr(res_pred$raw[1:180], 
                       densfun = "normal")$estimate[1]

sd3 = MASS::fitdistr(res_pred$raw[1:180], 
                     densfun = "normal")$estimate[2]

mean4 = MASS::fitdistr(res_pred$raw[181:360], 
                       densfun = "normal")$estimate[1]

sd4 = MASS::fitdistr(res_pred$raw[181:360], 
                     densfun = "normal")$estimate[2]

mean3
sd3

mean4
sd4

#---------------------------------------------------------------

# HISTOGRAMAS

## Resíduo Pearson

g1 <- ggplot(data = res_pred, aes(x=pearson))+
  geom_histogram(fill=1,alpha=0.5, col = 1)+
  theme_bw()+
  xlab('')+
  ylab('')+
  geom_vline(xintercept = 0, col = 2, lty = 2, lwd = 1)+
  facet_wrap(~resp, scales = 'free') + 
  ggtitle('Resíduo Pearson')

## Resíduo cru

g2 <- ggplot(data = res_pred, aes(x=raw))+
  geom_histogram(fill=1,alpha=0.5, col = 1)+
  theme_bw()+
  xlab('')+
  ylab('')+
  geom_vline(xintercept = 0, col = 2, lty = 2, lwd = 1)+
  facet_wrap(~resp, scales = 'free') + 
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
  ggtitle('Resíduo Pearson') + facet_wrap(~resp, scales = 'free')

## Resíduo cru

g2 <- ggplot(data = res_pred, 
             mapping = aes(sample = raw)) +
  geom_qq(alpha = 0.5) + geom_qq_line(col = 2)+
  theme_bw() +
  xlab('Quantis teóricos')+
  ylab('Quantis amostrais') + 
  ggtitle('Resíduo cru') + facet_wrap(~resp, scales = 'free')

ggpubr::ggarrange(g1,g2, nrow = 2)

#---------------------------------------------------------------

## RESIDUOS vs PREDITO

g1 <- ggplot(data = res_pred, aes(y=pearson,x=preditos))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'loess', se=F)+
  xlab('')+
  ylab('Resíduos') + 
  ggtitle('Resíduo Pearson') + facet_wrap(~resp, scales = 'free')

g2 <- ggplot(data = res_pred, aes(y=raw,x=preditos))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'loess', se=F)+
  xlab('Preditos')+
  ylab('Resíduos') + 
  ggtitle('Resíduo cru') + facet_wrap(~resp, scales = 'free')

ggpubr::ggarrange(g1,g2, nrow = 2)

#---------------------------------------------------------------

## RESIDUOS vs ÍNDICE

g1 <- ggplot(data = res_pred, aes(y=pearson,x=index))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'loess', se=F)+
  xlab('')+
  ylab('Resíduos') + 
  ggtitle('Resíduo Pearson') + facet_wrap(~resp, scales = 'free')

g2 <- ggplot(data = res_pred, aes(y=raw,x=index))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'loess', se=F)+
  xlab('Preditos')+
  ylab('Resíduos') + 
  ggtitle('Resíduo cru') + facet_wrap(~resp, scales = 'free')

ggpubr::ggarrange(g1,g2, nrow = 2)

#---------------------------------------------------------------
