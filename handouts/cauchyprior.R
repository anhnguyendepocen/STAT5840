
# cauchyprior.R
set.seed(1) # make the experiment reproducible
m <- 2000   # number of simulated values
x <- 3      # observed data

# Now simulate some random variables
theta <- rcauchy(m) # simulate m standard Cauchys
h <- pi*exp(-0.5*(x-theta)^2)  # compute h(theta)

C <- mean(h)  # estimate the normalizing constant
post.mean <- mean(theta*h)/mean(h) # estimate the posterior mean

# We'd like to see a running average plot to assess convergence

num <- theta*h   # vector in the numerator
rc <- rep(0, times = m)
rpm <- rep(0, times = m)

for (i in seq.int(m)){         # seq.int(m) will be 1:m
  rc[i] <- mean(h[1:i])
  rpm[i] <- mean(num[1:i])/mean(h[1:i]);
}

C
post.mean

pdf(file="img/CauchyPrior.pdf")
# now plot the results
par(mfrow = c(1,2))
plot(3:200, rpm[3:200], type = "l", ylim = c(0, 2.7))
lines(3:200, rc[3:200], type = "l")
plot(1:m, rpm, type = "l", ylim = c(0, 2.7))
lines(1:m, rc, type = "l")
par(mfrow = c(1,1))

dev.off()
