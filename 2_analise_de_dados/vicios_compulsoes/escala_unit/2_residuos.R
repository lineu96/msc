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
res_pred <- data.frame(index = rep(1:nrow(dados_dissertacao),2),
                       resp = c(rep('YFAS', nrow(dados_dissertacao)),
                                rep('BES', nrow(dados_dissertacao))),
                       observado = c(dados_dissertacao$YFAS_taxa,
                                     dados_dissertacao$BES_taxa),
                       preditos = preditos,
                       pearson = pearson,
                       raw = residuos
)

#---------------------------------------------------------------

# AJUSTE À NORMAL DOS RESÍDUOS

## Resíduo Pearson

mean1 = MASS::fitdistr(res_pred$pearson[1:nrow(dados_dissertacao)], 
                       densfun = "normal")$estimate[1]

sd1 = MASS::fitdistr(res_pred$pearson[1:nrow(dados_dissertacao)], 
                     densfun = "normal")$estimate[2]

mean2 = MASS::fitdistr(res_pred$pearson[(nrow(dados_dissertacao)+1):nrow(res_pred)], 
                       densfun = "normal")$estimate[1]

sd2 = MASS::fitdistr(res_pred$pearson[(nrow(dados_dissertacao)+1):nrow(res_pred)], 
                     densfun = "normal")$estimate[2]

round(mean1, 2)
round(sd1, 2)

round(mean2, 2)
round(sd2, 2)

## Resíduo cru

mean3 = MASS::fitdistr(res_pred$raw[1:nrow(dados_dissertacao)], 
                       densfun = "normal")$estimate[1]

sd3 = MASS::fitdistr(res_pred$raw[1:nrow(dados_dissertacao)], 
                     densfun = "normal")$estimate[2]

mean4 = MASS::fitdistr(res_pred$raw[(nrow(dados_dissertacao)+1):nrow(res_pred)], 
                       densfun = "normal")$estimate[1]

sd4 = MASS::fitdistr(res_pred$raw[(nrow(dados_dissertacao)+1):nrow(res_pred)], 
                     densfun = "normal")$estimate[2]

mean3
sd3

mean4
sd4

#---------------------------------------------------------------

# HISTOGRAMAS

## Resíduo Pearson

a<-ggplot(data = subset(res_pred, resp == 'YFAS'), 
          aes(x=pearson))+
  geom_histogram(col = 1, fill='white',
                 breaks = hist(res_pred$pearson, 
                               plot = F)$breaks) +
  theme_bw()+
  xlab('Resíduo')+
  ylab('Frequência')+
  ggtitle('Resíduo Pearson para YFAS')

b<-ggplot(data = subset(res_pred, resp == 'BES'), 
          aes(x=pearson))+
  geom_histogram(col = 1, fill='white',
                 breaks = hist(res_pred$pearson, 
                               plot = F)$breaks) +
  theme_bw()+
  xlab('Resíduo')+
  ylab('Frequência')+
  ggtitle('Resíduo Pearson para BES')


g <- ggpubr::ggarrange(a,b,
                       nrow = 1, ncol = 2)


ggsave(filename='res_hist.pdf', 
       plot=g, device="pdf", 
       path=getwd(),
       dpi=500, 
       height = 3, 
       width = 7)


## Resíduo bruto

a<-ggplot(data = subset(res_pred, resp == 'YFAS'), 
          aes(x=raw))+
  geom_histogram(col = 1, fill='white',
                 breaks = hist(res_pred$raw, 
                               plot = F)$breaks) +
  theme_bw()+
  xlab('Resíduo')+
  ylab('Frequência')+
  ggtitle('Resíduo bruto para YFAS')

b<-ggplot(data = subset(res_pred, resp == 'BES'), 
          aes(x=raw))+
  geom_histogram(col = 1, fill='white',
                 breaks = hist(res_pred$raw, 
                               plot = F)$breaks) +
  theme_bw()+
  xlab('Resíduo')+
  ylab('Frequência')+
  ggtitle('Resíduo brutp para BES')


g <- ggpubr::ggarrange(a,b,
                       nrow = 1, ncol = 2)

#---------------------------------------------------------------

## RESIDUOS vs PREDITOS

a <- ggplot(data = subset(res_pred, resp == 'YFAS'), 
            aes(y=pearson,x=preditos))+
  geom_jitter()+
  theme_bw()+
  geom_smooth(col= "#696969", method = 'loess', se=F)+
  xlab('Preditos')+
  ylab('Resíduos') + 
  ggtitle('Resíduos x Preditos para YFAS')

b <- ggplot(data = subset(res_pred, resp == 'BES'), 
            aes(y=pearson,x=preditos))+
  geom_jitter()+
  theme_bw()+
  geom_smooth(col='#696969', method = 'loess', se=F)+
  xlab('Preditos')+
  ylab('Resíduos') + 
  ggtitle('Resíduos x Preditos para BES')

g <- ggpubr::ggarrange(a,b,
                       nrow = 1, ncol = 2)

#---------------------------------------------------------------

ggsave(filename='res_pred.pdf', 
       plot=g, device="pdf", 
       path=getwd(),
       dpi=500, 
       height = 3, 
       width = 7)

a <- ggplot(data = subset(res_pred, resp == 'YFAS'), 
            aes(y=raw,x=preditos))+
  geom_jitter(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=1, method = 'loess', se=F)+
  xlab('Preditos')+
  ylab('Resíduos') + 
  ggtitle('Resíduos brutos x Preditos para YFAS')

b <- ggplot(data = subset(res_pred, resp == 'BES'), 
            aes(y=raw,x=preditos))+
  geom_jitter(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=1, method = 'loess', se=F)+
  xlab('Preditos')+
  ylab('Resíduos') + 
  ggtitle('Resíduos brutos x Preditos para BES')

g <- ggpubr::ggarrange(a,b,
                       nrow = 1, ncol = 2)

#---------------------------------------------------------------

## RESIDUOS vs ÍNDICE

g1 <- ggplot(data = res_pred, aes(y=pearson,x=index))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'loess', se=F)+
  xlab('')+
  ylab('Índice') + 
  ggtitle('Resíduo Pearson') + facet_wrap(~resp, scales = 'free')

g2 <- ggplot(data = res_pred, aes(y=raw,x=index))+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_smooth(col=2, method = 'loess', se=F)+
  xlab('Índice')+
  ylab('Resíduos') + 
  ggtitle('Resíduo cru') + facet_wrap(~resp, scales = 'free')

ggpubr::ggarrange(g1,g2, nrow = 2)

#---------------------------------------------------------------
