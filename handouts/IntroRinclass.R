
pdf(file="img/IntroR.pdf")
   par(mfrow = c(1,2))
   require(diagram)
   par(mex = 0.2, cex = 0.5)
   openplotmat(frame.plot=TRUE)
   print(textellipse(mid = c(0.54,0.5), box.col = grey(0.95), 
     radx = 0.24, rady = 0.37, angle = 37,
     lab = "E", cex = 10, asp = 1))
   openplotmat(frame.plot=TRUE)
   textellipse(mid = c(0.54,0.5), box.col = grey(0.95), 
     radx = 0.24, rady = 0.37, angle = 37,
     lab = "E", cex = 10, asp = 1)
   print(points(runif(1000), runif(1000)))
   par(mfrow = c(1,1))

dev.off()

   # IntroRinclass.R

   Iter <- 1000; count <- 0           # initialization and storage
   for (i in 1:Iter){                 # start the simulation loop	
     x <- runif(1); y <- runif(1)     # generates two rand coords
     if (y <= x^2){ 
       count <- count + 1             # accept the point
     }
   }
   accept <- count/Iter               # store prop of accepts
   accept                             # print prop of accepts
   sqrt(accept*(1-accept)/Iter)       # print std error of estimate
