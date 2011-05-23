
# compare_estimators.R

set.seed(1)
Iter <- 1000        # number of iterations (should be multiple of 100)
n <- 20             # size of random sample
tr <- 0.05          # proportion of obs trimmed from end of sample

slmn <- 0           # the "sum of the loss for the mean"=0
slmd <- 0           # the "sum of the loss for the median"=0
sltm <- 0           # the "sum of the loss for the trimmed mean"=0

sl2mn <- 0          # the "sum of the squared loss for the mean"=0
sl2md <- 0          # the "sum of the squared loss for the median"=0
sl2tm <- 0          # the "sum of the squared loss for the trimmed mean"=0

mu <- 0             # location parameter assumed to be zero

for (i in seq.int(Iter)){
  # simulate n Laplace r.v.s
  x <- log(runif(n)/runif(n))  #n-vector of Laplace(0,1)s
                             
  loss1 <- (mean(x) - mu)^2    # compute losses
  loss2 <- (median(x) - mu)^2
  loss3 <- (mean(x, trim = tr) - mu)^2

  slmn <- slmn + loss1         # accumulate loss for mean
  slmd <- slmd + loss2         # accumulate loss for median
  sltm <- sltm + loss3         # accumulate loss for trimmed mean

  sl2mn <- sl2mn + loss1^2     # accumulate loss^2 for mean
  sl2md <- sl2md + loss2^2     # accumulate loss^2 for median
  sl2tm <- sl2tm + loss3^2     # accumulate loss^2 for median
}

# Now compute Monte Carlo risks

r1 <- round(slmn/Iter, 4)              # average loss
r2 <- round(slmd/Iter, 4)
r3 <- round(sltm/Iter, 4)

# Standard error of Loss
se1 <- round(sqrt((sl2mn - slmn^2/Iter)/Iter^2), 4)
se2 <- round(sqrt((sl2md - slmd^2/Iter)/Iter^2), 4)
se3 <- round(sqrt((sl2tm - sltm^2/Iter)/Iter^2), 4)

# Now print the results
paste("Mean:         ", r1, " (", se1, ")", sep="")
paste("Median:       ", r2, " (", se2, ")", sep="")
paste("Trimmed Mean: ", r3, " (", se3, ")", sep="")
