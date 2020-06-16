## Transformações Box-Cox, Box-Cox with Negatives Allowed, Yeo-Johnson e Basic Power

#box cox tradicional, valores não negativos

# lambda != 0, (x^(lambda)-1)/lambda
# lambda  = 0, log(x)

x <- rnorm(10, 5,1)
x_bc <- bcPower(U = x, lambda = 2)

lambda = 2
x_bc == (x^(lambda)-1)/lambda

x_log <- bcPower(U = x, lambda = 0)
x_log == log(x)

#---------------------

bcnPower #negativos permitidos

# gamma > 0 se U possui negativos, >=0 caso contrario
# transformação box cox de .5 (U + √[U^2 + gamma^2])

## Com negativos

y <- rnorm(10,10,20)

bcn <- bcnPower(U = y, lambda = 1, gamma = 1)

gamma = 1
bcn2 <- bcPower(0.5*(y + sqrt(y^2 + gamma^2)), lambda = 1) 

bcn == bcn2


## Sem negativos

w <- rnorm(10,10,3)

bcn <- bcnPower(U = w, lambda = 1, gamma = 1)

gamma = 1
bcn2 <- bcPower(0.5*(w + sqrt(w^2 + gamma^2)), lambda = 1) 

bcn == bcn2

#---------------------

#yjPower()
# Transformação box cox de:
# U+1 para não negativos, 
# |U|+1 with parameter 2-lambda for U negative.

yjPower(x, lambda = 2) == bcPower(x+1, lambda = 2)

#---------------------

basicPower 
# U^{lambda}, lambda != 0, 
# log(lambda) caso contrario, para U positivo.

basicPower(x, lambda = 2) == x^2
basicPower(x, lambda = 0) == log(x)

#---------------------

#Finding Univariate or Multivariate Power Transformations

#uses the maximum likelihood-like 
#approach of Box and Cox (1964) to 
#select a transformatiion of a univariate 
#or multivariate response for normality, 
#linearity and/or constant variance.

#"bcPower"
#"bcnPower"
#"yjPower"

g <- rgamma(100, 1,3)
densityPlot(g) 

powerTransform(g, family="bcPower")

g2 <- bcPower(U = g, lambda = 0.2827951)
densityPlot(g2) 


p1 <- powerTransform(cycles ~ len + amp + load, Wool, 
                     family = "bcPower")

densityPlot(Wool$cycles) 
densityPlot(bcPower(U = Wool$cycles, lambda = -0.05915814)) 


#----------------

# Boxplots for transformations to symmetry

r <- rbeta(100, 0.7, 5)

symbox(~r)
densityPlot(r)
densityPlot(r^0.5)

#-------

## Graph the profile log-likelihood for Box-Cox transformations in 1D, or in 2D with the bcnPower family.

x <- rgamma(100,1,1)
y <- rpois(100, 4)
z <- rnorm(100, 5)

boxCox(x~y+z)

bc <- boxCox(x~y+z)

bc <- data.frame(lambda = bc$x,
                 loglik = bc$y)

t <- subset(bc, bc$loglik == max(bc$loglik))

densityPlot(x)
densityPlot(x^as.numeric(t[1]))


#-----------------

#Axes for Transformed Variables

#Simple power transformation, 
#x' = x^p for p != 0 
#x' = log x for p = 0.

with(UN, plot(log(ppgdp, 10), log(infantMortality, 10)))

basicPowerAxis(0, base=10, side="above", 
               at=c(50, 200, 500, 2000, 5000, 20000), grid=TRUE, 
               axis.title="GDP per capita")

basicPowerAxis(0, base=10, side="right",
               at=c(5, 10, 20, 50, 100), grid=TRUE, 
               axis.title="infant mortality rate per 1000")


#Box-Cox power transformation
#x' = (x^p - 1)/p for x != 0
#x' = log(x) for p = 0.

with(UN, plot(bcPower(ppgdp, 0), bcPower(infantMortality, 0)))

bcPowerAxis(0, side="above", 
            grid=TRUE, axis.title="GDP per capita")

bcPowerAxis(0, side="right",
            grid=TRUE, axis.title="infant mortality rate per 1000")

#---------------

# Box-Tidwell Transformations

# Box-Tidwell power transformations of the predictors in a linear model.

boxTidwell(prestige ~ income + education, 
           ~ type + poly(women, 2), 
           data=Prestige)

#-----------

# Choose a Predictor Transformation Visually or Numerically


with(UN, invTranPlot(ppgdp, infantMortality))
with(UN, invTranEstimate(ppgdp, infantMortality))

#-----------

# Logit Transformation

b <- rbeta(10,1,2)

round(logit(b),4) == round(log(b/(1 - b)),4)

#-----------------

# Likelihood-Ratio Tests for Univariate or Multivariate Power Transformations to Normality

#testTransform computes likelihood ratio tests 
#for particular values of the power parameter 
#based on powerTransform objects.


x <- rgamma(100,1,1)
y <- rpois(100, 4)
z <- rnorm(100, 5)

t <- powerTransform(x ~ y+z)
testTransform(t, c(0))


a <- powerTransform(cbind(len, adt, trks, sigs1) ~ htype, Highway1)
testTransform(a, c(0, 0, 0, -1))

q <- powerTransform(lm(cbind(LoBD$I1L2, LoBD$I1L1) ~ pool, LoBD), family="bcnPower")
testTransform(q, c(.3, .8))

