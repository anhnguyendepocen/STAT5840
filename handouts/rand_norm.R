
# rand_norm.R
rand_norm <- function(n){
  M <- 1.5203            # bound used in A/R algorithm
  i <- 0; N <- 0         # initialization and storage
  z <- rep(0, times = n)
  while(i < n){                      # keep going until n accepts
      x <- tan(pi*(runif(1) - 0.5))  # simulate a Cauchy
      u <- runif(1)                  # simulate a Uniform
      f <- 1/sqrt(2*pi)*exp(-x^2/2)  # compute f(x)
      g <- 1/pi/(1+x^2)              # compute g(x)
      if (u <= f/M/g){
        i <- i + 1             # got another accept
        z[i] <- x              # save this one
      }
    N <- N + 1                 # keep track of num trials
  }
list(z = z, accept = n/N)                     
}

tmp <- rand_norm(10000)
tmp$accept

pdf(file="img/ARalgo.pdf")
par(mfrow = c(2,1))
curve(dnorm, xlim = c(-3,3), ylim = c(0,0.5), lwd = 2)
f <- function(x) 1.53*dcauchy(x)
curve(f, add = TRUE, lwd = 2, lty = 2)
hist(tmp$z, , xlim = c(-3,3), xlab = "x", main = "")
par(mfrow = c(1,1))

dev.off()
