#-------------------------------------------------------------------
# Análise TG Bruno Tissei
#-------------------------------------------------------------------


# CONTEXTO
#-------------------------------------------------------------------

# Intrução entra no processador e é decodificada.

# Uma macroinstrução é quebrada em diversas micro

# Ao todo existem 2700 macroinstruções

# Não sabemos o que é feito na micro

# As micro são destinadas para portas que podem ter uma ou mais 
# unidades de execução (a mesma porta pode ser capaz de realizar
# mais de um tipo de tarefa), e unidades de execução diferentes
# podem realizar a mesma tarefa

# Macro gera micros, micros são executadas em diferentes lugares

# PROBLEMA: quanto tempo uma macro leva para ser executada?
# Dependa das micro

# Em determinadas unidades as instruções levam sempre o mesmo
# tempo para serem executadas. 

# É feito um trabalho de inferência para verificar quais portas
# são usadas

# Dada a instrução e as portas, qual foi a latência
#-------------------------------------------------------------------

# VARIÁVEIS

# NÚMERO DA INSTRUÇÃO
# N

# NOME DA INSTRUÇÃO
# Instruction

# VARIÁVEIS INDICADORAS
# LOCK
# SET-REX
# SEGMENT
# MEM_XCHG

# PORTAS (quantas vezes foi usada)
# p0156
# p06
# p23
# p237
# p4
# p1
# p15
# p015
# p5
# p0
# p05
# p01
# p6

# Min Latency	
# Max Latency

#-------------------------------------------------------------------
library(tidyverse)
#-------------------------------------------------------------------

# DADOS
#-------------------------------------------------------------------

dados <- read.csv2('tissei.csv', header = T, sep = ',')

str(dados)

names(dados) <- c('n', 'inst', 'lock', 'set_rex', 'segment',
                  'mem_xchg', 'p0156', 'p06', 'p23', 'p237', 
                  'p4', 'p1', 'p15', 'p015', 'p5', 'p0', 
                  'p05', 'p01', 'p6', 'min_lat', 'max_lat')

#-------------------------------------------------------------------

# ANÁLISE POR VARIÁVEL

#---

# n vai de 1 até 1127 (mesmo numero de linhas)

#---

# 299 instruções (as com mesmo nome passam por diferentes portas)
instructions <- as.data.frame(sort(table(dados$inst), decreasing = T))
names(instructions) <- c('inst', 'freq')

nrow(instructions) 

#---

# Indicadoras fornecidas
table(dados$lock) # tem lock
table(dados$set_rex) # começam com set
table(dados$segment) 
table(dados$mem_xchg) # tem xchg

# Dá pra juntar numa variável só, mas tem mt NA

ind <- rep(NA, nrow(dados))
ind <- ifelse(dados$lock == 1, 'lock', ind)
ind <- ifelse(dados$set_rex == 1, 'set_rex', ind)
ind <- ifelse(dados$mem_xchg == 1, 'mem_xchg', ind)
ind <- ifelse(dados$segment %in% 1:6, 'segment', ind)
ind <- ifelse(is.na(ind) == T, 'outro', ind)

ind <- factor(ind, levels = c("outro","lock","segment","set_rex", "mem_xchg"))

table(ind)

#---

# Instruções com padrão '_'
indicadoras <- subset(instructions, str_detect(instructions$inst, '_') == T)

padroes <- substr(indicadoras$inst, 
                  str_locate(indicadoras$inst, '_'),
                  stringr::str_length(indicadoras$inst))

padroes <- as.data.frame(table(padroes))
arrange(padroes, desc(Freq))

#---

# PORTAS

# Tabelas de frequencia
tb_freq <- function(vetor){
  aux <- factor(vetor, 
                levels = 0:max(vetor))
  
  freq <- table(aux)
  freq_r <- round(prop.table(freq),3) # frequência relativa
  
  table <- data.frame(Niveis = names(freq),
                      Frequencia = as.vector(freq),
                      Freq_relativa = as.vector(freq_r))
  
  return(table)
  
}

t_p0156 <- tb_freq(dados$p0156)    
t_p06 <- tb_freq(dados$p06)     
t_p23 <- tb_freq(dados$p23)      
t_p237 <- tb_freq(dados$p237)     
t_p4 <- tb_freq(dados$p4)       
t_p1 <- tb_freq(dados$p1)      
t_p15 <- tb_freq(dados$p15)      
t_p015 <- tb_freq(dados$p015)     
t_p5 <- tb_freq(dados$p5)       
t_p0 <- tb_freq(dados$p0)      
t_p05 <- tb_freq(dados$p05)      
t_p01 <- tb_freq(dados$p01)      
t_p6 <- tb_freq(dados$p6)

# Medidas de posição
posic <- function(vetor){
             data.frame( 
               Mínimo = quantile(vetor)[1],
               Quartil_1 = quantile(vetor)[2],
               Média = round(mean(vetor),2),
               Mediana = quantile(vetor)[3],
               Moda = names(sort(table(vetor), 
                                 decreasing = TRUE)[1]),
               Quartil_3 = quantile(vetor)[4],
               Máximo = quantile(vetor)[5]
             )
} 

m_p0156 <- posic(dados$p0156)    
m_p06 <- posic(dados$p06)     
m_p23 <- posic(dados$p23)      
m_p237 <- posic(dados$p237)     
m_p4 <- posic(dados$p4)       
m_p1 <- posic(dados$p1)      
m_p15 <- posic(dados$p15)      
m_p015 <- posic(dados$p015)     
m_p5 <- posic(dados$p5)       
m_p0 <- posic(dados$p0)      
m_p05 <- posic(dados$p05)      
m_p01 <- posic(dados$p01)      
m_p6 <- posic(dados$p6)
m_min_lat <- posic(dados$min_lat)
m_max_lat <- posic(dados$max_lat)

m1 <- rbind(m_p0156, m_p06, m_p23, m_p237,
            m_p4, m_p1, m_p15, m_p015, m_p5,
            m_p0, m_p05, m_p01, m_p6, m_min_lat,
            m_max_lat)

row.names(m1) <- c("p0156", "p06", "p23", "p237",
                   "p4", "p1", "p15", "p015", "p5",
                   "p0", "p05", "p01", "p6", "min_lat",
                   "max_lat")

# Medidas de dispersão
disp <- function(vetor){
  data.frame(Amplitude = diff(range(vetor)),
             Variância = round(var(vetor), 2),
             Desvio_padrão = round(sd(vetor), 2),
             Coef_variação = paste0(round(100*sd(vetor)/mean(vetor), 2), "%"))
}
  
d_p0156 <- disp(dados$p0156)    
d_p06 <- disp(dados$p06)     
d_p23 <- disp(dados$p23)      
d_p237 <- disp(dados$p237)     
d_p4 <- disp(dados$p4)       
d_p1 <- disp(dados$p1)      
d_p15 <- disp(dados$p15)      
d_p015 <- disp(dados$p015)     
d_p5 <- disp(dados$p5)       
d_p0 <- disp(dados$p0)      
d_p05 <- disp(dados$p05)      
d_p01 <- disp(dados$p01)      
d_p6 <- disp(dados$p6)
d_min_lat <- disp(dados$min_lat)
d_max_lat <- disp(dados$max_lat)

d1 <- rbind(d_p0156, d_p06, d_p23, d_p237,
            d_p4, d_p1, d_p15, d_p015, d_p5,
            d_p0, d_p05, d_p01, d_p6, d_min_lat,
            d_max_lat)

row.names(d1) <- c("p0156", "p06", "p23", "p237",
                   "p4", "p1", "p15", "p015", "p5",
                   "p0", "p05", "p01", "p6", "min_lat",
                   "max_lat")

#---

# LATENCIA MINIMA

#Tabela 1

h <- hist(dados$min_lat, plot = FALSE)

breaks <- h$breaks

Classes <- cut(dados$min_lat, breaks = breaks, 
               include.lowest = TRUE, right = TRUE)

tabela <- as.data.frame(table(Classes))

tabela$Freq_relativa <- round(prop.table(tabela$Freq),3)

tabela

#Tabela 2

breaks2 <- c(seq(0,35,5), seq(40, 80,20), seq(100, max(dados$max_lat), max(dados$max_lat)-100))

Classes <- cut(dados$min_lat, breaks = breaks2, 
               include.lowest = TRUE, right = TRUE)

tabela2 <- as.data.frame(table(Classes))

tabela2$Freq_relativa <- round(prop.table(tabela2$Freq),3)

tabela2


#---

# LATENCIA MAXIMA

#Tabela 1

h <- hist(dados$max_lat, plot = FALSE)

breaks <- h$breaks

Classes <- cut(dados$max_lat, breaks = breaks, 
               include.lowest = TRUE, right = TRUE)

tabela <- as.data.frame(table(Classes))

tabela$Freq_relativa <- round(prop.table(tabela$Freq),3)

tabela

#Tabela 2

breaks2 <- c(seq(0,35,5), seq(40, 80,20), seq(100, max(dados$max_lat), max(dados$max_lat)-100))

Classes <- cut(dados$max_lat, breaks = breaks2, 
               include.lowest = TRUE, right = TRUE)

tabela2 <- as.data.frame(table(Classes))

tabela2$Freq_relativa <- round(prop.table(tabela2$Freq),3)

tabela2


#---


# Gráficos

setores <- function(table, title = 'title'){
  
  tb <- as.data.frame(table)
  
  tb <- tb %>% 
    arrange(desc(Var1)) %>%
    mutate(prop = Freq / sum(tb$Freq) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  
  ggplot(tb, aes(x="", y=prop, fill=Var1)) +
    geom_bar(stat="identity", 
             width=1, 
             color="white",
             alpha = 0.8) +
    coord_polar("y", start=0) +
    ggtitle(title)+
    theme_bw() + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.title = element_blank())+
    
    geom_text(aes(y = ypos, label = Freq), 
              color = "white", size=6) +
    scale_fill_brewer(palette="Set1")
  
}

## Indicadoras
setores(table(dados$lock), title = 'lock')
setores(table(dados$set_rex), title = 'set_rex')
setores(table(dados$segment), title = 'segment')
setores(table(dados$mem_xchg), title = 'mem_xchg')


### Com 'outros'
ggplot(data = as.data.frame(table(ind)), 
       aes(x = ind, y = Freq))+
  geom_bar(stat = 'identity') +
  coord_flip() + xlab('')+ ylab('') +
  ylim(c(0, 1000))+
  geom_text(aes(label=Freq), 
            vjust=0, 
            hjust = -1,
            color=1, 
            size=6) +theme_bw()

### Sem outros  
ggplot(data = as.data.frame(table(ind))[2:5,], 
       aes(x = ind, y = Freq))+
  geom_bar(stat = 'identity') +
  coord_flip() + xlab('')+ ylab('') +
  ylim(c(0, 140))+
  geom_text(aes(label=Freq), 
            vjust=0, 
            hjust = -1,
            color=1, 
            size=6) +theme_bw()

# Histogramas e densidades
hist_den <- function(df,var,n_col){
  
  g1 <- ggplot(data = df, 
               aes_string(x = var))+
    geom_density(col = 1,
                 fill = 1,
                 alpha = 0.6) + 
    theme_bw() +
    xlab('')+ ylab('Densidade') + ggtitle('Com 0') +
    theme_bw()
  
  g2 <- ggplot(data = subset(df, df[,n_col] > 0), 
               aes_string(x = var))+
    geom_density(col = 1,
                 fill = 1,
                 alpha = 0.6) + 
    
    theme_bw() + ggtitle('Sem 0') +
    xlab('')+ ylab('') +theme_bw()
  
  
  g3 <- ggplot(data = df, 
               aes_string(x = var))+
    geom_histogram(col = 1,
                   fill = 1,
                   alpha = 0.6) +
    xlab('Valores')+ ylab('Frequência') + theme_bw()
  
  g4 <- ggplot(data = subset(df, df[,n_col] > 0), 
               aes_string(x = var))+
    geom_histogram(col = 1,
                   fill = 1,
                   alpha = 0.6) +
    xlab('Valores') + ylab('') +theme_bw()
  
  library(gridExtra)
  grid.arrange(g1,g2,g3,g4,
               nrow=2,ncol=2, 
               top = var
  )  
  
}

hist_den(dados, "p0156", 7)
hist_den(dados, "p06", 8)
hist_den(dados, "p23", 9)
hist_den(dados, "p237", 10)
hist_den(dados, "p4", 11)
hist_den(dados, "p1", 12)
hist_den(dados, "p15", 13)
hist_den(dados, "p015", 14)
hist_den(dados, "p5", 15)
hist_den(dados, "p0", 16)
hist_den(dados, "p05", 17)
hist_den(dados, "p01", 18)
hist_den(dados, "p6", 19)

hist_den(dados, "min_lat", 20)
hist_den(dados, "max_lat", 21)

names(dados)
