#-------------------------------------------------------------------
# Análise TG Bruno Tissei
#-------------------------------------------------------------------

# TABELAS
#-------------------------------------------------------------------

# Indicadoras fornecidas
table(dados$lock) # tem lock
table(dados$set_rex) # começam com set
table(dados$mem_xchg) # tem xchg
table(dados$segment) 

# Unindo as indicadoras num mesmo vetor
ind <- rep(NA, nrow(dados))
ind <- ifelse(dados$lock == 1, 'lock', ind)
ind <- ifelse(dados$set_rex == 1, 'set_rex', ind)
ind <- ifelse(dados$mem_xchg == 1, 'mem_xchg', ind)
ind <- ifelse(dados$segment %in% 1:6, 'segment', ind)
ind <- ifelse(is.na(ind) == T, 'outro', ind)

ind <- factor(ind, levels = c("outro","lock","segment","set_rex", "mem_xchg"))

table(ind)

#-------------------------------------------------------------------

# Portas
(t_p0156 <- tb_freq(dados$p0156))
(t_p06 <- tb_freq(dados$p06))
(t_p23 <- tb_freq(dados$p23))
(t_p237 <- tb_freq(dados$p237))
(t_p4 <- tb_freq(dados$p4))
(t_p1 <- tb_freq(dados$p1))
(t_p15 <- tb_freq(dados$p15))
(t_p015 <- tb_freq(dados$p015))
(t_p5 <- tb_freq(dados$p5))
(t_p0 <- tb_freq(dados$p0))
(t_p05 <- tb_freq(dados$p05))
(t_p01 <- tb_freq(dados$p01))
(t_p6 <- tb_freq(dados$p6))

#-------------------------------------------------------------------

# Latências

## Mínima

### Tabela 1

h <- hist(dados$min_lat, plot = FALSE)
breaks <- h$breaks
Classes <- cut(dados$min_lat, breaks = breaks, 
               include.lowest = TRUE, right = TRUE)
tabela1_min <- as.data.frame(table(Classes))
tabela1_min$Freq_relativa <- round(prop.table(tabela1_min$Freq),3)

tabela1_min

### Tabela 2

breaks2 <- c(seq(0,35,5), seq(40, 80,20), seq(100, max(dados$max_lat), max(dados$max_lat)-100))
Classes <- cut(dados$min_lat, breaks = breaks2, 
               include.lowest = TRUE, right = TRUE)
tabela2_min <- as.data.frame(table(Classes))
tabela2_min$Freq_relativa <- round(prop.table(tabela2_min$Freq),3)

tabela2_min

#---------

## Máxima

### Tabela 1

h <- hist(dados$max_lat, plot = FALSE)
breaks <- h$breaks
Classes <- cut(dados$max_lat, breaks = breaks, 
               include.lowest = TRUE, right = TRUE)
tabela1_max <- as.data.frame(table(Classes))
tabela1_max$Freq_relativa <- round(prop.table(tabela1_max$Freq),3)

tabela1_max

#Tabela 2

breaks2 <- c(seq(0,35,5), seq(40, 80,20), seq(100, max(dados$max_lat), max(dados$max_lat)-100))
Classes <- cut(dados$max_lat, breaks = breaks2, 
               include.lowest = TRUE, right = TRUE)

tabela2_max <- as.data.frame(table(Classes))
tabela2_max$Freq_relativa <- round(prop.table(tabela2_max$Freq),3)

tabela2_max

#-------------------------------------------------------------------

# Medidas de posição
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

m1

#-------------------------------------------------------------------

# Medidas de dispersão
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

d1

#-------------------------------------------------------------------
