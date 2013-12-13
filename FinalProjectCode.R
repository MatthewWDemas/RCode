#==============================================================================
# R Code for Final Project for 6005
# Author: Matthew Demas
# Description:
#   Given the desired lower (a) and upper (b) limits of a function (defined in
#   f(x)) as well as the max(f(x)) between a and b, the program will determine
#   the expected value of X who has the PDF f(x) within the defined interval
#==============================================================================

# Define the limits
a = 0
b = 2
c = 3

# Define the integrand of the expectation
# The function whose expectation will be defined is sin(x) in this case
#  with x*sin(x) being the integrand of the expectation
f <- function(x) {x*sin(x)}

# the actual value of the function
actual = integrate(f, lower = a, upper = b)

# The upper bound of the variance of the simulated random variable Z
varlim <- function(n) {1/(4*n)}

# Initialize some vectors
expectation <- c()
variance <- c()

nrun = c(10,50,100,500,1000,2500,5000,7500,10000,20000,30000,50000,75000,100000,
         500000,750000,1000000)

for (i in nrun) {
  V = runif(i,a,b)
  W = runif(i,0,c)
  
  Y = data.frame(V=V, W=W)
  Y$Yi <- 1*(Y$W <= f(Y$V))
  Y$Yi_fac <- Y$Yi
  Y$Yi_fac[which(Y$Yi_fac == 1)] <- 'rosybrown1'
  Y$Yi_fac[which(Y$Yi_fac == 0)] <- 'gray'
  
  x <- seq(a,b,0.01)
  #png("~/Box Documents/6005/final_project/real-plot-300.png", width=900, height=500)
  par(ps=30,mar=c(9,9,9,9))
  plot(Y$W ~ Y$V, col=Y$Yi_fac, xlim=c(a,b), ylim=c(0,c), xlab='V', ylab='W',
       main='x sin(x) \n a=0,b=1,c=3,n=300', cex=2,lwd=3 )
  lines(x,f(x), lwd=3)
  #dev.off()
  
  (P <- sum(Y$Yi)/length(Y$Yi))
  P*c*(b-a)
  expectation[i] <- P*c*(b-a)
  variance[i] <- P*(1-P)/length(Y$Yi)
}


png("~/Box Documents/6005/final_project/exp-plot.png", width=900, height=500)
par(ps=30,mar=c(9,9,9,9))
plot(log(expectation), col='blue', xlab="n", ylab="log(expectation)",
     main = "Log(c*(b-a)*E[Z]) vs n", cex=2,lwd=3 )
abline(log(actual$value),0, lty=3, lwd=3)
dev.off()

n <- seq(0,max(nrun))

png("~/Box Documents/6005/final_project/var-plot.png", width=900, height=500)
par(ps=30,mar=c(9,9,9,9))
plot(log(variance), col ='blue', xlab='n', ylab='log(variance)',
     main="log(variance) vs n", cex=2,lwd=3 )
lines(n,log(varlim(n)), lwd=3)
dev.off()
