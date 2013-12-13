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

# This is a function which generates the set of Y_i's based upon the defined
#   a, b, c rectangle and the number of iterations
GenY <- function(n,a,b,c) {
  # Generate the V_i's and W_i's
  V = runif(i,a,b)
  W = runif(i,0,c)
  
  # Put them together and set the value of Y_i and set the color used if 
  #   you'd like to plot (V_i, W_i) along with xf(x)
  Y.fn = data.frame(V=V, W=W)
  Y.fn$Yi <- 1*(Y.fn$W <= f(Y.fn$V))
  Y.fn$Yi_fac <- Y.fn$Yi
  Y.fn$Yi_fac[which(Y.fn$Yi_fac == 1)] <- 'rosybrown1'
  Y.fn$Yi_fac[which(Y.fn$Yi_fac == 0)] <- 'gray'

  return(Y.fn)
}
  
# the actual value of the function
actual = integrate(f, lower = a, upper = b)

# The upper bound of the variance of the simulated random variable Z
varlim <- function(n) {1/(4*n)}

# Initialize some vectors
expectation <- c()
variance <- c()

# I set my code to run for n iterations, where n is given in nrun
nrun = c(10,50,100,500,1000,2500,5000,7500,10000,20000,30000,50000,75000,100000,
         500000,750000,1000000)

for (i in nrun) {
  # Compute the set of Y_i's
  Y <- GenY(i,a,b,c)
  
  # compute E[Z] (i.e. the probability that Y_i = 1, which I call P here)
  P <- sum(Y$Yi)/length(Y$Yi)

  # compute the estimated expectation of X (i.e. E[X]) based on 
  #  E[X] = c*(b-a)*E[Z]
  expectation[i] <- P*c*(b-a)
  
  # compute the variance of Z (i.e. var(Z)) based on the equation for the 
  #   variance of a binomial distribution
  variance[i] <- P*(1-P)/length(Y$Yi)
}

# To generate the plot with the function and the points where Y_i = 1 vs
#   Y_i = 0
Y <- GenY(300,a,b,c)
x <- seq(a,b,0.01)
plot(Y$W ~ Y$V, col=Y$Yi_fac, xlim=c(a,b), ylim=c(0,c), xlab='V', ylab='W',
     main='x sin(x) \n a=0,b=1,c=3,n=300', cex=2,lwd=3 )
lines(x,f(x), lwd=3)

# To generate the Expectation versus n plot
n <- seq(0,max(nrun))
plot(log(expectation), col='blue', xlab="n", ylab="log(expectation)",
     main = "Log(c*(b-a)*E[Z]) vs n", cex=2,lwd=3 )
abline(log(actual$value),0, lty=3, lwd=3)

# To generate the variance versus n plot
plot(log(variance), col ='blue', xlab='n', ylab='log(variance)',
     main="log(variance) vs n", cex=2,lwd=3 )
lines(n,log(varlim(n)), lwd=3)
abline(0, 0, lty=3, lwd=3)
