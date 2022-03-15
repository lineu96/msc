library(car)

#-------

# Export a data frame to disk in one of many formats

Export(iris, 'iris.csv')

#-------

# Import data from many file formats

Import("iris.csv")

#-------

# Recode a Variable

x<-rep(1:3,3)
x

recode(x, "c(1,2)='A'; 
	else='B'")

Recode(x, "1:2='A'; 3='B'")

#---------------

#Position of Row Names

whichNames(c('minister', 'conductor'), Duncan)
Duncan[c(6,16),]

#-------

# Apply a Function to a Variable Within Factor Levels

Tapply(Sepal.Length ~ Species, mean, data=iris)
Tapply(Sepal.Length ~ Species, sd, data=iris)


#-------

# Sample a Few Elements of an Object

some(iris, n = 5)

# Print Abbreviated Ouput

brief(rnorm(100))
brief(Duncan)

#-------

#Nonparametric Density Estimates

x <- rnorm(100)
y <- rgamma(100, shape = 1.5, rate = 2)

par(mfrow = c(1,2))
densityPlot(x)
densityPlot(y)

densityPlot(~income, data=Prestige)

densityPlot(income ~ type, kernel=dnorm, data=Prestige)
#densityPlot(income ~ type, kernel=depan, data=Prestige)
#densityPlot(income ~ type, kernel=dbiwt, data=Prestige)

#-------

#Boxplots With Point Identification

Boxplot(x)
Boxplot(y)

Boxplot(~income, data=Prestige, id=list(n=Inf)) # identify all outliers
Boxplot(income ~ type, data=Prestige)

#-------

# Enhanced Scatterplots with Marginal Boxplots, Point Marking, Smoothers, and More

scatterplot(prestige ~ income, data=Prestige, ellipse=TRUE)

scatterplot(prestige ~ income, data=Prestige, ellipse=FALSE)

scatterplot(prestige ~ income, data=Prestige, ellipse=FALSE,
            smooth=list(smoother=NULL))

scatterplot(prestige ~ income | type, data=Prestige, legend=list(coords="topleft"))

scatterplot(income ~ type, data=Prestige)

#-------

#Smoothers to Draw Lines on Scatterplots
v1 = rnorm(1500, 1, 5)
v2 = rnorm(1500, 1, 5)
x = v1*2
y = v1+v2

par(mfrow = c(1,3))
plot(y~x, main = 'gamLine')
gamLine(x, y, col = 2)

plot(y~x, main = 'loessLine')
loessLine(x, y, col = 3)

plot(y~x, main = 'quantregLine')
quantregLine(x, y, col = 4)


scatterplot(prestige ~ income, data=Prestige, 
            smooth=list(smoother=gamLine))

scatterplot(prestige ~ income, data=Prestige, 
            smooth=list(smoother=loessLine))

scatterplot(prestige ~ income, data=Prestige, 
            smooth=list(smoother=quantregLine))

#-------


# Scatterplot Matrices

scatterplotMatrix(iris)

scatterplotMatrix(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width | Species, 
                  data = iris)

#-------

#Panel Function for Coplots

coplot(prestige ~ income|education, #panel=panel.car, 
       col="red", data=Prestige)

coplot(prestige ~ income|education, panel=panel.car, 
       col="red", data=Prestige)

#-------

#Data Ellipses

dataEllipse(Duncan$income, Duncan$education, levels=0.1*1:9, 
            ellipse.label=0.1*1:9, lty=2, fill=TRUE, fill.alpha=0.1)

with(Prestige, dataEllipse(income, education, type, 
                           id=list(n=2, labels=rownames(Prestige)), pch=15:17,
                           xlim=c(0, 25000), center.pch="+",
                           group.labels=c("Blue Collar", "Professional", "White Collar"),
                           ylim=c(5, 20), level=.95, fill=TRUE, fill.alpha=0.1))

#-------

# Functions to Construct Contrasts
ct <- lm(prestige ~ (income + education)*type, data=Prestige, 
   contrasts=list(type="contr.Treatment"))

cs <- lm(prestige ~ (income + education)*type, data=Prestige, 
   contrasts=list(type="contr.Sum"))

ch <- lm(prestige ~ (income + education)*type, data=Prestige, 
   contrasts=list(type="contr.Helmert"))

#-------