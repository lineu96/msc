#------------------------------------------------------------
# Test Linear Hypothesis
#------------------------------------------------------------
?linearHypothesis
#------------------------------------------------------------

#estatística F ou qui-quadrado para realizar uma comparação 
#baseada no teste de Wald entre um modelo e um modelo 
#linearmente restrito.

# estatistica de wald 
# (beta - beta_x)^2/ var(beta) ou beta - beta_x/ep(beta)
# t((beta - beta_x)) %*% vcov_inv %*% (beta - beta_x)

#------------------------------------------------------------

#Examples
mod.davis <- lm(weight ~ repwt, data=Davis)

## the following are equivalent:
linearHypothesis(mod.davis, diag(2), c(0,1))
linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"))
linearHypothesis(mod.davis, c("(Intercept)", "repwt"), c(0,1))
linearHypothesis(mod.davis, c("(Intercept)", "repwt = 1"))

## use asymptotic Chi-squared statistic
linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"), 
                 test = "Chisq")

#------------------------------------------------------------

## the following are equivalent:
## use HC3 standard errors via white.adjust option
linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"), 
                 white.adjust = TRUE)
## covariance matrix *function*
linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"), 
                 vcov = hccm)
## covariance matrix *estimate*
linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"), 
                 vcov = hccm(mod.davis, type = "hc3"))

#------------------------------------------------------------

mod.duncan <- lm(prestige ~ income + education, data=Duncan)

## the following are all equivalent:
linearHypothesis(mod.duncan, "1*income - 1*education = 0")
linearHypothesis(mod.duncan, "income = education")
linearHypothesis(mod.duncan, "income - education")
linearHypothesis(mod.duncan, "1income - 1education = 0")
linearHypothesis(mod.duncan, "0 = 1*income - 1*education")
linearHypothesis(mod.duncan, "income-education=0")
linearHypothesis(mod.duncan, "1*income - 1*education + 1 = 1")
linearHypothesis(mod.duncan, "2income = 2*education")

#------------------------------------------------------------

mod.duncan.2 <- lm(prestige ~ type*(income + education), data=Duncan)
coefs <- names(coef(mod.duncan.2))

## test against the null model (i.e., only the intercept is not set to 0)
linearHypothesis(mod.duncan.2, coefs[-1]) 

#------------------------------------------------------------

## test all interaction coefficients equal to 0
linearHypothesis(mod.duncan.2, coefs[grep(":", coefs)], verbose=TRUE)
linearHypothesis(mod.duncan.2, matchCoefs(mod.duncan.2, ":"), verbose=TRUE) # equivalent
lh <- linearHypothesis(mod.duncan.2, coefs[grep(":", coefs)])
attr(lh, "value") # value of linear function
attr(lh, "vcov")  # covariance matrix of linear function

#------------------------------------------------------------