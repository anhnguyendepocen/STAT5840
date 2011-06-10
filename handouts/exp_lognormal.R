
# exp_lognormal.R

# compute for a range of lambdas
lambda <- seq(from = 1.5, to = 10, by = 0.1)

m <- 100000                       # number of iterations (need a lot)
R1 <- rep(0, length(lambda))
R2 <- rep(0, length(lambda))

for (j in seq_along(lambda)){     # for each lambda, estimate risk
  l <- lambda[j]                  # get jth lambda
  sig <- sqrt(2*log(l))           # calc sigma
  x <- rlnorm(m, sdlog = sig)     # simulate m lognormals
  f1 <- l^(-1)*exp(-x/l)          # f1 is the exponential density
                                  # f2 is the lognormal density
  f2 <- (1/x)*(exp(-(log(x))^2/(2*sig^2)))/sqrt(2*pi*sig^2) 
  h <- (x - l)^2/l^2              # loss function
  R2[j] <- mean(h)                # lognormal risk
  R1[j] <- mean((h*f1)/f2)        # exponential risk
}

pdf(file="img/ExpLognormal.pdf")
# now plot the results
par(mfrow = c(2,1))
plot(R1 ~ lambda, ylim = c(0,2), type = "l", main = "Exponential")
plot(R2 ~ lambda, ylim = c(0,60), type = "l", main = "Lognormal")
par(mfrow = c(1,1))

dev.off()
