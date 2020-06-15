#-------------------------------------------------------------------
# Análise TG Bruno Tissei
#-------------------------------------------------------------------

# GRÁFICOS - ANÁLISE UNIVARIADA
#-------------------------------------------------------------------

## Setores para Indicadoras
setores(table(dados$lock), title = 'lock')
setores(table(dados$set_rex), title = 'set_rex')
setores(table(dados$mem_xchg), title = 'mem_xchg')

#setores(table(dados$segment), title = 'segment')

seg <- factor(dados$segment, levels = 0:6)
seg <- as.data.frame(table(seg))

ggplot(data = seg, 
       aes(x = seg, y = Freq))+
  geom_bar(stat = 'identity') +
  xlab('')+ ylab('')+
  ylim(c(0, 1200))+
  geom_text(aes(label=Freq), 
            vjust=-0.1, 
            hjust = 0.5,
            color=1, 
            size=6) +theme_bw()

ggplot(data = seg[-1,], 
       aes(x = seg, y = Freq))+
  geom_bar(stat = 'identity') +
  xlab('')+ ylab('')+
  ylim(c(0, 20))+
  geom_text(aes(label=Freq), 
            vjust=-0.1, 
            hjust = 0.5,
            color=1, 
            size=6) +theme_bw()

## Barras para indicadoras agrupadas

ind <- rep(NA, nrow(dados))
ind <- ifelse(dados$lock == 1, 'lock', ind)
ind <- ifelse(dados$set_rex == 1, 'set_rex', ind)
ind <- ifelse(dados$mem_xchg == 1, 'mem_xchg', ind)
ind <- ifelse(dados$segment %in% 1:6, 'segment', ind)
ind <- ifelse(is.na(ind) == T, 'outro', ind)

ind <- factor(ind, levels = c("outro","lock","segment","set_rex", "mem_xchg"))

ind <- as.data.frame(table(ind))

### Com 'outros'
ggplot(data = ind, 
       aes(x = ind, y = Freq))+
  geom_bar(stat = 'identity') +
  coord_flip() + xlab('')+ ylab('') +
  ylim(c(0, 1000))+
  geom_text(aes(label=Freq), 
            vjust=0, 
            hjust = -0.1,
            color=1, 
            size=6) +theme_bw()

### Sem outros  
ggplot(data = ind[2:5,], 
       aes(x = ind, y = Freq))+
  geom_bar(stat = 'identity') +
  coord_flip() + xlab('')+ ylab('') +
  ylim(c(0, 140))+
  geom_text(aes(label=Freq), 
            vjust=0, 
            hjust = -0.1,
            color=1, 
            size=6) +theme_bw()

## Gráficos de barras para as portas
barras(dados, "p0156", 7)
barras(dados, "p06", 8)
barras(dados, "p23", 9)
barras(dados, "p237", 10)
barras(dados, "p4", 11)
barras(dados, "p1", 12)
barras(dados, "p15", 13)
barras(dados, "p015", 14)
barras(dados, "p5", 15)
barras(dados, "p0", 16)
barras(dados, "p05", 17)
barras(dados, "p01", 18)
barras(dados, "p6", 19)

## Gráficos de barras para as latências

### Latência mínima

g1 <- ggplot(data = dados, 
       aes_string(x = 'min_lat'))+
  geom_bar(col = 1,
           fill = 1,
           alpha = 0.6) +
  xlab('Valores')+ ylab('Frequência') +
  ggtitle('Competo') + theme_bw()

g2 <- ggplot(data = subset(dados, dados$min_lat <21), 
       aes_string(x = 'min_lat'))+
  geom_bar(col = 1,
           fill = 1,
           alpha = 0.6) +
  xlab('Valores')+ ylab('') + 
  ggtitle('0-20') + theme_bw()

grid.arrange(g1,g2,
             nrow=1,ncol=2, 
             top = 'Latência mínima')

### Latência máxima

g3 <- ggplot(data = dados, 
             aes_string(x = 'max_lat'))+
  geom_bar(col = 1,
           fill = 1,
           alpha = 0.6) +
  xlab('Valores')+ ylab('Frequência') +
  ggtitle('Completo') + theme_bw()

g4 <- ggplot(data = subset(dados, dados$max_lat <41), 
             aes_string(x = 'max_lat'))+
  geom_bar(col = 1,
           fill = 1,
           alpha = 0.6) +
  xlab('Valores')+ ylab('') +
  ggtitle('0-40')+
  theme_bw()


grid.arrange(g3,g4,
             nrow=1,ncol=2, 
             top = 'Latência máxima')
             
#-------------------------------------------------------------------