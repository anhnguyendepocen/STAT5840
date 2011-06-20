
#jackknife3.R
n <- length(islands)
x <- islands

#jackknife estimate of se
Mj <- rep(0, times = n)
for (i in seq.int(n)) {  
  y <- x[-i]
  Mj[i] <- median(y)
}

Mjbar <- mean(Mj)
SEj <- sqrt((n-1)/n * sum((Mj - Mjbar)^2))

#bootstrap estimate of se
Mb <- replicate(1000, expr = {
                y <- sample(x, size = n, replace = TRUE)
                median(y) })
SEb <- sd(Mb)

SEj
SEb
