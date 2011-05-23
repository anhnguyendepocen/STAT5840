
# mydiscretev.R

mydiscretev <- function(p,N){
  # mydiscretev.R
  # p is the desired target density on 1,...,k.
  # N is the number of observations desired
  x <- rep(0, times = N)                # initialize
  k <- length(p)                        # the support of p
  M <- k*max(p)                         # the bound size
  for(i in seq.int(N)){
    accept <- FALSE                     # initialize
    while (!accept){                    # continue until we accept
      y <- sample(k, size = 1)          # y is disc unif 1...k
      u <- runif(1)                     # u is unif(0,1)
      accept <- ( u < p[y]/(M*1/k) )    # is TRUE if A/R condition satisfied
    }
    x[i] <- y                           # this y is a good one
  }
  return(x)
}

p <- c(.11, .12, .09, .08, .12, .10, .09, .09, .10, .10)
x <- mydiscretev(p, 10000)

pdf(file="img/DARalgo.pdf")
plot(prop.table(table(x)), ylab = "Relative frequency")

dev.off()
