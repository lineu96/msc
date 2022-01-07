#---------------------------------------------------

# Give your marginals

invcdfnames <- c("qpois","qpois", "qpois")

#---------------------------------------------------

# Give the marginals’ arguments:

paramslists <- list(
  m1 = list(lambda = 2),
  m2 = list(lambda = 2),
  m3 = list(lambda = 2)
)

#---------------------------------------------------

# Give the target correlation matrix

cor_matrix <- matrix(c(1.0,  0.75, 0.5,
                       0.75,  1.0, 0.25,
                       0.5,  0.25, 1.0
),3,3)

cor_matrix

#---------------------------------------------------

# Generate the wanted samples:

res <- NORTARA::genNORTARA(1000,
                           cor_matrix,
                           invcdfnames,
                           paramslists)
head(res,5)

cor(res)

#---------------------------------------------------

