
#-----------------------------------------------------------------
# simular regressão com explicativa categórica de diferentes 
# distribuições
#-----------------------------------------------------------------

# NORMAL

sample_size = 25
trat <- gl(4, sample_size/4)
beta <- c(5,0,0,0)
X <- model.matrix(~ trat)

mu <- X%*%beta

y <- rnorm(sample_size, mean = mu, sd = 1)

dados <- data.frame(y = y,
                    x = trat)

glm(y~x, 
    family = 'gaussian', 
    dados)

#-----------------------------------------------------------------

# POISSON

sample_size = 25
trat <- gl(4, sample_size/4)
beta <- c(5,0,0,0)
X <- model.matrix(~ trat)

lambda <- exp(X%*%beta)

y <- rpois(sample_size, 
           lambda = lambda)

dados <- data.frame(y = y,
                    x = trat)

glm(y~x, 
    family = 'poisson', 
    dados)

#-----------------------------------------------------------------

# BINOMIAL

sample_size = 25
trat <- gl(4, sample_size/4)
beta <- c(5,0,0,0)
X <- model.matrix(~ trat)

p <- exp(X%*%beta)/(1 + exp(X%*%beta))

y <- rbinom(sample_size, 
           p = p,
           size = 10)

dados <- data.frame(y = y,
                    x = trat)

glm(cbind(y, 10-y)~x, 
    family = 'binomial', 
    dados)

#-----------------------------------------------------------------

# BETA

sample_size = 250
trat <- gl(4, sample_size/4)
beta <- c(5,0,0,0)
X <- model.matrix(~ trat)

p <- exp(X%*%beta)/(1 + exp(X%*%beta))

y <- gamlss.dist::rBE(sample_size, 
                      mu = p, 
                      sigma = 0.2)

dados <- data.frame(y = y,
                    x = trat)

gamlss::gamlss((y-0.0000001)~x, 
               family = gamlss.dist::BE(), 
               method = RS(50), 
               data = dados)


#-----------------------------------------------------------------