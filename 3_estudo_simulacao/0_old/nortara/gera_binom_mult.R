
#---------------------------------------------------
# COMO SIMULAR DATASETS COM 3 RESPOSTAS BERNOULLIS 
# E UMA EXPLICATIVA CATEGÓRICA
#---------------------------------------------------

sample_size = 500
n_treatment = 4
betas = c(0.5,0,0,0)

#---------------------------------------------------

# tratamentos
trat <- gl(n_treatment, sample_size/n_treatment)

# matriz do modelo
X <- model.matrix(~ trat)

#---------------------------------------------------

# Marginais

invcdfnames <- c("qbinom","qbinom", "qbinom")

#---------------------------------------------------

# Argumentos das marginais

p <- exp(X%*%betas)/(1 + exp(X%*%betas))

paramslists <- list(
  m1 = list(p = p, size = 1),
  m2 = list(p = p, size = 1),
  m3 = list(p = p, size = 1)
)

#---------------------------------------------------

# Matriz de correlação alvo

cor_matrix <- matrix(c(1.0,  0.75, 0.5,
                       0.75,  1.0, 0.25,
                       0.5,  0.25, 1.0
),3,3)

#---------------------------------------------------

# Gerando as respostas

y <- NORTARA::genNORTARA(sample_size,
                         cor_matrix,
                         invcdfnames,
                         paramslists)

#---------------------------------------------------

# Comparando correlação alvo e correlação dos dados

cor_matrix
cor(y)

#---------------------------------------------------

# Gerando dataset

y <- as.data.frame(y)
names(y) <- c('y1', 'y2', 'y3')
y$x <- trat

datasets <- y

#---------------------------------------------------

# McGLM
library(mcglm)

link <- "logit"
variance <- "binomialP"
form1 = y1~x
form2 = y2~x
form3 = y3~x
Z0 <- mc_id(datasets)

fit <- 
  mcglm(linear_pred = c(form1, form2, form3),
        matrix_pred = list(Z0,Z0,Z0),
        link = c(link, link, link),
        variance = c(variance, variance, variance),
        Ntrial = list(1,1,1),
        data = datasets)

coef(fit)
