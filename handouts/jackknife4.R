
# jackknife4.R
# initialize
data(patch, package = "bootstrap")
n <- nrow(patch)
y <- patch$y
z <- patch$z
B <- 2000
theta.b <- numeric(B)
# set up storage for the sampled indices
indices <- matrix(0, nrow = B, ncol = n)

# step one run the bootstrap
for (b in 1:B) {
  i <- sample(1:n, size = n, replace = TRUE)
  y <- patch$y[i]
  z <- patch$z[i]
  theta.b[b] <- mean(y) / mean(z)
  #save the indices for the jackknife
  indices[b, ] <- i
}

# step two run jackknife on bootstrap replicates
se.jack <- numeric(n)
for (i in 1:n) {
  #in i-th replicate omit all samples with x[i]
  keep <- (1:B)[apply(indices, MARGIN = 1,
                      FUN = function(k) {!any(k == i)})]
  se.jack[i] <- sd(theta.b[keep])
}

sd(theta.b)
sqrt((n-1) * mean((se.jack - mean(se.jack))^2))
