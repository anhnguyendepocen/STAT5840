
pdf(file="img/SimTransInvMeth.pdf")
# SimTransMeth.R

par(mfrow = c(3,1))
Iter <- 10000                        # initialization and storage    
y <- rep(0, times = Iter)

# To simulate Beta(3,7)'s 
for (i in seq.int(Iter)){            # start the simulation loop	
  u <- sum(-log(runif(3)))           # a Gamma(a,1)
  v <- sum(-log(runif(7)))           # a Gamma(b,1)
  y[i] <- u/(u+v)                    # a Beta(a,b)
}
hist(y, 30, prob = TRUE, main = "Beta(3,7)")

# now plot the density function
f <- function(x) dbeta(x, shape1 = 3, shape2 = 7)
curve(f, lwd = 2, add = TRUE)


# To simulate Logis(3,7)'s
u <- runif(Iter)                     # 10,000 uniforms
x <- log(u/(1-u))                    # std logistics
y <- 3 + 7*x                         # Logis(3,7)'s
hist(y, 30, prob = TRUE, main = "Logis(3,7)")

# now plot the density function
f <- function(x) dlogis(x, location = 3, scale = 7)
curve(f, lwd = 2, add = TRUE)


# To simulate Exp(3)'s
u <- runif(Iter)             # bunch of uniforms
y <- -3*log(1-u)             # bunch of exponentials
hist(y, 30, prob = TRUE, main = "Exp(3)")

# now plot the density function
f <- function(x) dexp(x, rate = 1/3)
curve(f, lwd = 2, add = TRUE)

par(mfrow = c(1,1))

dev.off()
