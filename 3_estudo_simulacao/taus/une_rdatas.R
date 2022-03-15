datasets_1 <- datasets

length(datasets_1)

#load no rdata do servidor
length(datasets)

inicio <- length(datasets_1)

#for de 1 ate o numero de datasets gerado no servidor
for (i in 1:length(datasets)) {
  datasets_1[[(inicio+i)]] <- datasets[[i]]
}

length(datasets_1)
