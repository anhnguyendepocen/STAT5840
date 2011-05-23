
u <- runif(100)
x <- -log(1 - u)

u <- runif(100)
x <- -log(1 - u)

u = runif(100);
x = log(u/(1 - u));

u <- runif(100)   # 100 uniforms
x <- log(u(1-u))  # 100 std logistics
y <- 3 + 0.7*x    # 100 Log(3,7)'s

u <- sum(-log(runif(m)))     # a Gamma(a,1)
v <- sum(-log(runif(n)))     # a Gamma(b,1)
y <- u/(u+v)                 # a Beta(a,b)
