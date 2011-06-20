
library(bootstrap)
data(patch)
head(patch)

# jackknife2.R
n <- nrow(patch)
y <- patch$y
z <- patch$z
theta.hat <- mean(y)/mean(z)  # original value of statistic

theta.jack <- numeric(n)
for (i in 1:n){
  theta.jack[i] <- mean(y[-i]) / mean(z[-i])
}
bias <- (n - 1) * (mean(theta.jack) - theta.hat)
se <- sqrt((n-1) *
        mean((theta.jack - mean(theta.jack))^2))  # this is the new line

theta.hat
bias
se
bias/se
