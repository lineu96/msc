#---------------------------------------------------------------
# RESÍDUOS
#---------------------------------------------------------------

# Obtenção dos resíduos

## chol(vcov) inversa
chol_inv <- Matrix::chol(fit$inv_C)

## Resíduos empilhados
res <- as.numeric(residuals(fit, type = 'raw'))

## Produto matricial
residuos <- as.numeric(chol_inv%*%res)

## Preditos
preditos <- fit$fitted

## Dataframe
res_pred <- data.frame(index = 1:nrow(massa),
                       resp = c(rep('min_lat', nrow(massa)),
                                rep('max_lat', nrow(massa))),
                       observado = massa$max_lat,
                       preditos = preditos,
                       pearson = residuos,
                       raw = res
                      )

#---------------------------------------------------------------

# HISTOGRAMAS

## Resíduo Pearson
mean = MASS::fitdistr(res_pred$pearson, 
                      densfun = "normal")$estimate[1]

sd = MASS::fitdistr(res_pred$pearson, 
                    densfun = "normal")$estimate[2]

g1 <- ggplot(data = res_pred, aes(x=pearson))+
  geom_histogram(fill=1,alpha=0.5, col = 1)+
  theme_bw()+
  xlab('Resíduos')+
  ylab('Densidade')+
  geom_vline(xintercept = 0, col = 2, lty = 2, lwd = 1)+
  stat_function(fun = function(x) dnorm(x, 
                                        mean = mean, 
                                        sd = sd) * 1300,
                color = "darkred", size = 1) + 
  ggtitle('Resíduo Pearson')

# Resíduo cru

mean2 = MASS::fitdistr(res_pred$raw, 
                      densfun = "normal")$estimate[1]

sd2 = MASS::fitdistr(res_pred$raw, 
                    densfun = "normal")$estimate[2]


g2 <- ggplot(data = res_pred, aes(x=raw))+
  geom_histogram(fill=1,alpha=0.5, col = 1)+
  theme_bw()+
  xlab('Resíduos')+
  ylab('Densidade')+
  geom_vline(xintercept = 0, col = 2, lty = 2, lwd = 1)+
  stat_function(fun = function(x) dnorm(x, 
                                        mean = mean2, 
                                        sd = sd2) * 2450,
                color = "darkred", size = 1) + 
  ggtitle('Resíduo cru')

ggpubr::ggarrange(g1,g2)

#---------------------------------------------------------------

# HISTOGRAMAS

## Resíduo Pearson
mean = MASS::fitdistr(res_pred$pearson, 
                      densfun = "normal")$estimate[1]

sd = MASS::fitdistr(res_pred$pearson, 
                    densfun = "normal")$estimate[2]

g1 <- ggplot(data = res_pred, aes(x=pearson))+
  geom_histogram(fill=1,alpha=0.5, col = 1)+
  theme_bw()+
  xlab('')+
  ylab('')+
  geom_vline(xintercept = 0, col = 2, lty = 2, lwd = 1)+
  facet_wrap(~resp, scales = 'free') + 
  ggtitle('Resíduo Pearson')

# Resíduo cru

mean2 = MASS::fitdistr(res_pred$raw, 
                       densfun = "normal")$estimate[1]

sd2 = MASS::fitdistr(res_pred$raw, 
                     densfun = "normal")$estimate[2]


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

## Resíduos vs Predito
g1 <- ggplot(data = res_pred, aes(y=pearson,x=preditos))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'lm', se=F)+
  xlab('Preditos')+
  ylab('Resíduos') + 
  ggtitle('Resíduo Pearson')

g2 <- ggplot(data = res_pred, aes(y=raw,x=preditos))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'lm', se=F)+
  xlab('Preditos')+
  ylab('Resíduos') + 
  ggtitle('Resíduo cru')

ggpubr::ggarrange(g1,g2)

#---------------------------------------------------------------

## Resíduos vs Predito
g1 <- ggplot(data = res_pred, aes(y=pearson,x=preditos))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'lm', se=F)+
  xlab('')+
  ylab('Resíduos') + 
  ggtitle('Resíduo Pearson') + facet_wrap(~resp, scales = 'free')

g2 <- ggplot(data = res_pred, aes(y=raw,x=preditos))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'lm', se=F)+
  xlab('Preditos')+
  ylab('Resíduos') + 
  ggtitle('Resíduo cru') + facet_wrap(~resp, scales = 'free')

ggpubr::ggarrange(g1,g2, nrow = 2)

#---------------------------------------------------------------

## qqplot
g1 <- ggplot(data = res_pred, 
       mapping = aes(sample = pearson)) +
  geom_qq(alpha = 0.5) + geom_qq_line(col = 2)+
  theme_bw() +
  xlab('Quantis teóricos')+
  ylab('Quantis amostrais') + 
  ggtitle('Resíduo Pearson')

g2 <- ggplot(data = res_pred, 
             mapping = aes(sample = raw)) +
  geom_qq(alpha = 0.5) + geom_qq_line(col = 2)+
  theme_bw() +
  xlab('Quantis teóricos')+
  ylab('Quantis amostrais') + 
  ggtitle('Resíduo cru')

ggpubr::ggarrange(g1,g2)

#---------------------------------------------------------------

## qqplot
g1 <- ggplot(data = res_pred, 
             mapping = aes(sample = pearson)) +
  geom_qq(alpha = 0.5) + geom_qq_line(col = 2)+
  theme_bw() +
  xlab('')+
  ylab('Quantis amostrais') + 
  ggtitle('Resíduo Pearson') + facet_wrap(~resp, scales = 'free')

g2 <- ggplot(data = res_pred, 
             mapping = aes(sample = raw)) +
  geom_qq(alpha = 0.5) + geom_qq_line(col = 2)+
  theme_bw() +
  xlab('Quantis teóricos')+
  ylab('Quantis amostrais') + 
  ggtitle('Resíduo cru') + facet_wrap(~resp, scales = 'free')

ggpubr::ggarrange(g1,g2, nrow = 2)

#---------------------------------------------------------------

## Observado vs Predito
ggplot(data = res_pred, aes(x=observado,y=preditos))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(method = 'lm')+
  xlab('Observado')+
  ylab('Predito') + facet_wrap(~resp, scales = 'free') #+ 
  #geom_abline(slope = 1, intercept = 1, col = 2, lwd = 1.1)

#---------------------------------------------------------------