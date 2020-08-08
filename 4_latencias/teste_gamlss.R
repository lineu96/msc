library(gamlss)

zap <- gamlss(form.min_lat,
              data = massa, method = RS(50), 
              family = ZAP())

zanbi <- gamlss(form.min_lat,
              data = massa, method = RS(50), 
              family = ZANBI())

logLik(zap)
logLik(zanbi)

zap$aic
zanbi$aic

###################

