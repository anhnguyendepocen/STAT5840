
set.seed(1)

# bootstrap.R
srs <- rnorm(25, mean = 2, sd = 1)  # generate 25 normals, original data 
Iter <- 50000                       # want 50000 resamples
xbarstar <- rep(0, times = Iter)    # initialize vector xbarstar

for(i in seq.int(Iter)){
  boot.samp <- sample(srs, 25, replace = TRUE)   #sample with replacement
  xbarstar[i] <- mean(boot.samp)                 # save the xbar value
}

mean(xbarstar)
sd(xbarstar)

pdf(file="img/bootstrap.pdf")
hist(xbarstar, breaks = 40, main = "", prob = TRUE)
curve(dnorm(x, 2, 0.2), add = TRUE)

dev.off()
