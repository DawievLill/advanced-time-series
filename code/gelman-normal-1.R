
# Normal model with unknown mean and variance (Gelman pg 64)

library(ggplot2)
theme_set(theme_minimal())
library(grid)
library(gridExtra)
library(tidyr)

set.seed(5)

# We need to generate some data (this is arbitrary)
y <- c(93, 112, 122, 135, 122, 150, 118, 90, 124, 114)

# Generate the sufficient statistics for this data
n <- length(y)
s2 <- var(y)
my <- mean(y)

# We can factorise the join posterior and sample from the join posterior using this factorisation
## Two components in this factorisation

# nu is the degrees of freedom while s2 is the scaling parameter
# In our construction we are ignoring the normalising constant, which means this is not a probability distribution -- see the wikipedia entry https://en.wikipedia.org/wiki/Scaled_inverse_chi-squared_distribution

rsinvchisq  <- function(n, nu, s2, ...) nu*s2 / rchisq(n, nu, ...) # nu*s2 is the scaling component multiplied by the inverse chi-squared distribution -- see Albert page 64. See also page 583 in Gelman. 
# Not completely sure what this helper function is used for. 
dsinvchisq <- function(x, nu, s2){
  exp(log(nu/2)*nu/2 - lgamma(nu/2) + log(s2)/2*nu - log(x)*(nu/2+1) - (nu*s2/2)/x)
}

# Sample 1000 random numbers from the p(sigma2 | y)
ns <- 1000
sigma2 <- rsinvchisq(ns, n-1, s2)

# Next we sample from p(mu | sigma2, y) -- not exactly sure where this comes from in the book (see Section 2.5)
mu <- my + sqrt(sigma2/n)*rnorm(length(sigma2)) # length(sigma) tells us how many variables to generate. In this case the defaults for rnorm are used. Shifted by ybar and multiplied by standard deviation. 

# Potentially another way to do this would have been... (This makes more intuitive sense)
mu1 <- rnorm(length(sigma2), mean = my, sd = sqrt(sigma2)/sqrt(n))

# Create a variable sigma
sigma <- sqrt(sigma2)

# For mu, sigma compute the density in a grid (ranges of the grid are specified)
t1l <- c(90, 150)
t2l <- c(10, 60)
t1 <- seq(t1l[1], t1l[2], length.out = ns)
t2 <- seq(t2l[1], t2l[2], length.out = ns)
























