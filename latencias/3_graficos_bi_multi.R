#-------------------------------------------------------------------
# Análise TG Bruno Tissei
#-------------------------------------------------------------------

# GRÁFICOS - ANÁLISE BI E MULTIVARIADAS
# LATENCIAS EM FUNÇÃO DE PORTAS
#-------------------------------------------------------------------

# Gráficos de dispersão

scat('p0156')
scat('p06')
scat('p23')
scat('p237')
scat('p4')
scat('p1')
scat('p15')
scat('p015')
scat('p5')
scat('p0')
scat('p05')
scat('p01')
scat('p6')

# Correlograma

numericas <- dados[,c(7:18,20,21)]

names(numericas)[c(13,14)] <- c('min', 'max')

cor1 <- cor(numericas, method = 'pearson')

corrplot::corrplot.mixed(cor1, 
                         upper = 'ellipse',
                         tl.pos = "d"#,
                         #diag = 'n'
                         )
