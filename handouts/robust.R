
# robustt.R

robustt <- function(sigma1, sigma2, Iter = 10000){
  n <- 10; m <- 10         # sample sizes

  frepeated <- function(){
    x <- rnorm(m, sd = sigma1)             # generate data
    y <- rnorm(n, sd = sigma2)  
    tmp <- t.test(x, y, var.equal = TRUE)  # do the test
    tmp$p.value < 0.05                     # did it reject? 
  }

  ptmp <- replicate(Iter, frepeated())     # do it over and over
  p <- mean(ptmp)                          # prop of rejections
  se <- round(sqrt(p*(1-p)/Iter), 4)       # standard error
  
  # report results
  print(paste('sigma1 = ', sigma1, ', sigma2 = ', sigma2, sep = ""))
  print(paste('observed level of test = ', p, ' (', se, ')', sep = ""))
}

robustt(1,1)

robustt(1,10)
