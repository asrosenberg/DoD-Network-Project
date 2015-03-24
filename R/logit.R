## Example of estimating a basic logit with network data

library("statnet")

A <- matrix(sample(0:1, 100, replace=TRUE), ncol=10, nrow=10)

colnames(A) <- LETTERS[1:10]

A

N <- network(A, bipartite=TRUE)

N

plot(N)

ergmMPLE(N ~ edges + b1star(2:5), output="matrix")

# covector <- c(cospn[[1]]>1)
# winvector <- c(winintr[[1]])
# data <- cbind(covector, winvector, winvector2, winmvector, racevector)
# data <- as.data.frame(data)
# logitmodel <- glm(covector ~ winvector, data = data, family = "binomial")

## Try to replicate it with our data

source("R/clean-data.R")



