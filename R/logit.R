# source("R/clean-data.R")

## Example of estimating a basic logit with network data

library("statnet")

A <- matrix(sample(0:1, 100, replace=TRUE), ncol=10, nrow=10)

colnames(A) <- LETTERS[1:10]

A

N <- network(A, bipartite=TRUE)

N

plot(N)

net_logit <- ergmMPLE(N ~ edges + b1star(2:5), output="matrix")



