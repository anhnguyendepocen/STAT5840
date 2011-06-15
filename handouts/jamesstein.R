
# jamesstein.R
set.seed(1)
p <- 5             # how many populations
Iter <- 2000       # sample size
n <- 100           # number of thetas for the grid
theta <- seq(from = 0, to = 10, length.out = n)  # thetas on the grid
a <- 3             # J-S parameter, 0 < a < 2(p-2)
risk <- rep(0, times = n)

# Naive simulation
for (i in seq.int(n)){
  x <- matrix(rnorm(p*Iter, mean = theta[i]), nrow = p)
  nx <- colSums(x^2)
  js1 <- max(c(0, 1 - a/nx)) * x[1, ]
  js2 <- max(c(0, 1 - a/nx)) * x[2, ]
  js3 <- max(c(0, 1 - a/nx)) * x[3, ]
  js4 <- max(c(0, 1 - a/nx)) * x[4, ]
  js5 <- max(c(0, 1 - a/nx)) * x[5, ]
  risk[i] <- mean((js1 - theta[i])^2 + (js1 - theta[i])^2 + 
                  (js3 - theta[i])^2 + (js4 - theta[i])^2 + 
                  (js5 - theta[i])^2)
}

# Instead use the same sequence of variates
risk2 <- rep(0, times = n)
xstay <- matrix(rnorm(p*Iter), nrow = p)  # generate one set of data

for (i in seq.int(n)){
  x <- xstay + theta[i]
  nx <- colSums(x^2)
  js1 <- max(c(0, 1 - a/nx)) * x[1, ]
  js2 <- max(c(0, 1 - a/nx)) * x[2, ]
  js3 <- max(c(0, 1 - a/nx)) * x[3, ]
  js4 <- max(c(0, 1 - a/nx)) * x[4, ]
  js5 <- max(c(0, 1 - a/nx)) * x[5, ]
  risk2[i] <- mean((js1 - theta[i])^2 + (js1 - theta[i])^2 + 
                   (js3 - theta[i])^2 + (js4 - theta[i])^2 + 
                   (js5 - theta[i])^2)
}

# Now evaluate for different choices of a
a <- 1:5
risk3 <- matrix(rep(0, times = n*length(a)), nrow  = length(a))
th <- matrix(rnorm(p*Iter), nrow = p)

for (k in seq_along(a)){
  for (i in seq.int(n)){
    x <- th + theta[i]
    nx <- colSums(x^2)
    js1 <- max(c(0, 1 - a[k]/nx)) * x[1, ]
    js2 <- max(c(0, 1 - a[k]/nx)) * x[2, ]
    js3 <- max(c(0, 1 - a[k]/nx)) * x[3, ]
    js4 <- max(c(0, 1 - a[k]/nx)) * x[4, ]
    js5 <- max(c(0, 1 - a[k]/nx)) * x[5, ]
    risk3[k, i] <- mean((js1 - theta[i])^2 + (js1 - theta[i])^2 + 
                        (js3 - theta[i])^2 + (js4 - theta[i])^2 + 
                        (js5 - theta[i])^2)
  }
}

pdf(file="img/jamesstein.pdf")
plot(theta, risk, xlab = expression(theta), ylab = "risk", ylim = c(1,5.2), type = "l")
lines(theta, risk2, lwd = 2)
abline(h = 5, lty = 2)

dev.off()

pdf(file="img/jamesstein2.pdf")
plot(theta, risk3[1, ], xlab = expression(theta), ylab = "risk", ylim = c(2.8,5.2), type = "l")
abline(h = 5, lty = 2)
for (k in 2:5){print(lines(theta, risk3[k, ], lwd = k))}

dev.off()
