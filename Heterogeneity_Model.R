
## Simulate a disease spreading generation by generation
## Ignore:
#### Time (just count generations)
#### Total population size (just count infected individuals)
####### This means we're assuming we can neglect the susceptibles going down
#### ICI3D perspective (we're just going to use the Lloyd-Smith perspective)
## Assume: process does not change through time

## LS say (correctly) that a simple ODE model corresponds to an exponential distribution of infectiousness: this is because recovery is modeled as a random [constant ?? × exponentially distributed Duration]
## kappa is CV² (unitless and equal to 1/a, where a is a standard shape parameter)

set.seed(0707)

generation <- function(I, R0, kappa){
  gshape <- 1/kappa
  gscale <- R0*kappa
  ## dispersion parameter kappa indicates greater heterogeneity as it increases
  ## How infectious is each person?
  ## FIXME: should we have an alternative that works when kappa=0
  trans <- rgamma(I, shape=gshape, scale=gscale)
  
  ## How many people do they actually infect?
  inf <- rpois(I, trans)
  return(sum(inf))
}

generation(10, 4, kappa=0.01)

## FIXME: does generation() do what we think? What are some good ways to test it?

## Write a function that calls generation over and over and determines whether a disease "establishes" 
establish <- function(iStart, R0, kappa){
  
  I <- iStart
  n <- 1
  vecI <- c(I)
  ## Threshold of 100 cases (might be too low?)
  while (I < 100 & I != 0 & n <10){
  I <- generation(I, R0, kappa)
  vecI <- append(vecI, I, after=n)
  n <- n+1
  }
 ## FIXME: Put this in a while() loop and get it to run until we do or don't figure out whether disease has established.
  return(vecI)
}

Y <- establish(1, 4, 0.01)

plot(1:length(Y),               # Generations
     Y,                  # Number infected (I) on the y axis
     xlab = "Generations",     # Label the x axis
     ylab = "Number infected",  # Label the y axis
     main = "Heterogeneity Model",    # Plot title
     xlim = c(0,10),           #
     ylim = c(0, 100),
     type = "l",                # Use a line plot
     bty = "n")  

## How are we going to define establishment?
## What's a good practical definition (avoid running forever!)
## What arguments does this function need?