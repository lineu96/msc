#------------------------------------------------------------
##Anova Tables for Various Statistical Models
#------------------------------------------------------------
library(car)
#------------------------------------------------------------

## TRV PARA MODELOS ENCAIXADOS

# trv = (D_q - D_p)/phi
# trv = ((D_q - D_p)/(p-q)) / (D_p/(n-p))

m1 <- lm(prestige ~ education + log2(income) + type, 
         data = na.omit(Prestige))

m2 <- lm(prestige ~ 1, 
         data = na.omit(Prestige))


#par(mfrow = c(2,2))
#plot(m1, 1:4)
#plot(m2, 1:4)

#H0: OS MODELOS SÃO IGUAIS (a exclusão de variáveis não gera perda ao modelo)
#H1: OS MODELOS DIFEREM (a exclusão de variáveis gera perda ao modelo)

anova(m1,m2)

# SERVE PARA lm() e glm()
#------------------------------------------------------------

#Sequential Analysis of Variance

#anova() to an individual "lm" object, producing a sequential 
#analysis-of-variance table

# A Tabela Anova/anodev é a representação de uma sequência de TRVs
## Por que isso funciona sem reajustar o modelo? Isto é, sem ter a deviance dos 2 modelos?

#Type I analysis of variance
anova(m1)

#prestige ~ 1 versus prestige ~ education
#prestige ~ education versus prestige ~ education + log2(income)
#prestige ~ education + log2(income) versus prestige ~ education + log2(income) + type

#Type II analysis of variance
Anova(m1, type = 2)

#prestige ~ log2(income) + type versus
#prestige ~ education + log2(income) + type

#prestige ~ education + type versus
#prestige ~ education + log2(income) + type

#prestige ~ education + log2(income) versus
#prestige ~ education + log2(income) + type
#(IDENTICA À ULTIMA LINHA DA SEQUENCIAL)

#the reported F-statistics are equal to the 
#squares of the corresponding t-statistics
#in the model summary.

#Type II analysis of variance with interaction

m3 <- lm(prestige ~ education + log2(income) + type +
           education:type + log2(income):type, 
         data = na.omit(Prestige))

Anova(m3, type = 2)

#prestige ~ log2(income) + type + log2(income):type
# vs
#prestige ~ education + log2(income) + type +
#  education:type + log2(income):type

#prestige ~ education+ type + education:type
# vs
#prestige ~ education + log2(income) + type +
#  education:type + log2(income):type


#prestige ~ education + log2(income)
# vs
#prestige ~ education + log2(income) + type +
#  education:type + log2(income):type

# OS OUTROS 2: MODELO COM INTERAÇÃO X SEM????

#-------------
#For glm() Each line of the analysis-of-deviance 
#table provides a likelihood-ratio test based
#on the change in deviance comparing two models
#-------------

# Using a Nonstandard Coefficient Covariance Matrix

##Heteroscedasticity-Corrected Covariance Matrices
Anova(m3, vcov. = hccm)
hccm(m3)

#------------------------------------------------------------
