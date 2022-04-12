#Practical 2

# set seed for random numbers 
# set where the random function starts in order to get same value
set.seed(2020)
# simulate data from Normal distribution
n <- 100
height <- rnorm(n, mean=170, sd=10)

negloklik<-function(theta,data){
  mu<-theta[1]
  sigma<-theta[2]
  x<-data
  #sum bc the function work for each of 100 individuals
  #neg (-) bc otherwise and by default it would minimmize it
  -sum(dnorm(x, mean=mu, sd=sigma,log=TRUE))
}
negloklik(theta = c(150,1),height)

fit<-optim(par = c(0,1),fn=negloklik,data=height)

#Practical 3
alpha <- ( (1 - 0.57)/(0.073*0.073) - (1/0.57) )*0.57^2
beta <- alpha * ( (1/0.57) - 1)
n <- 10000
samp <- rbeta(n, alpha, beta)
(mu <- mean(samp))
(sigma <- sqrt(var(samp)))

test<-rnorm(n=1000,mean = 0,sd = 1000000)
hist(test)

#Practical 4
plot(density(rnorm(1000, 0, 1000)),"Unreasonable")
plot(density(rnorm(1000, 2, 0.5)),"Reasonable")
plot(density(plogis(rnorm(1000,0,10)), from = 0, to = 1),"Unreasonable")
plot(density(plogis(rnorm(1000,0,1.5)), from = 0, to = 1),"Reasonable")

library(rjags)
library(R2jags)
rjags::dic.samples()