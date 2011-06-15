
set.seed(1)

# bootmed.R
n <- length(islands)     
Iter <- 200
medstar <- rep(0, times = Iter)
for(i in seq.int(Iter)){
  boot.samp <- sample(islands, size = n, replace = TRUE)
  medstar[i] <- median(boot.samp)
}

mean(medstar)
mean(islands)
median(medstar)
median(islands)
sd(medstar)

pdf(file="img/bootmed.pdf")
hist(medstar, breaks = 40, main = "", prob = TRUE)
lines(density(medstar))

dev.off()
