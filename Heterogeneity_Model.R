
## FIXME: Get rid of the FIXME things that have been fixed ☺

## Simulate a disease spreading generation by generation
## Ignore:
#### Time (just count generations)
#### Total population size (just count infected individuals)
####### This means we're assuming we can neglect the susceptibles going down
#### ICI3D perspective (we're just going to use the Lloyd-Smith perspective)
## Assume: process does not change through time

## LS say (correctly) that a simple ODE model corresponds to an exponential distribution of infectiousness: this is because recovery is modeled as a random [constant ?? ? exponentially distributed Duration]
## kappa is CV? (unitless and equal to 1/a, where a is a standard shape parameter)

set.seed(0707)

generation <- function(I, R0, kappa, rho=0){
  ## How infectious is each person?
  ## FIXME: should we have an alternative that works when kappa=0
  
  ## rho measures correlation between susceptibility and transmissiveness
  ## if variation is all in mixing (e.g., ICI3D model, many STI models):
  ## rho should be 1
  ## In the default super-spreader case rho should be zero
  
  ## if rho > 0, the infectious people in our early generations will be more infections
  ## There is a simple theoretical expectation for this:
  
  ## weighting the average of the quantity by the quantity itself ⇒ 1+CV²
  Rinf <- R0*(1+kappa)^(rho)
  
  gshape <- 1/kappa
  gscale <- Rinf*kappa
  trans <- rgamma(I, shape=gshape, scale=gscale)
  
  ## How many people do they actually infect?
  
  inf <- rpois(I, trans)
  # inf0 <- rpois(I, trans0)
  
  # browser()
  
  if(kappa != 0) {
    result <- inf
  } else {
    result <- rep(1, I)
  }
  
  return(sum(result))
}


generation(3, 4, kappa=0.1)

## FIXME: does generation() do what we think? What are some good ways to test it?

## Write a function that calls generation over and over and determines whether a disease "establishes" 
establish <- function(iStart, R0, kappa, rho=0, threshold = 10000, max_generations = 100){
  
  I <- iStart
  n <- 1
  vecI <- c(I)
  
  while (I < threshold & I != 0 & n < max_generations){
    I <- generation(I, R0, kappa, rho)
    vecI <- c(vecI, I)
    n <- n+1
  }
  ## FIXME: Put this in a while() loop and get it to run until we do or don't figure out whether disease has established.
  ifelse(I == 0, return(FALSE), return(TRUE))
}

establish(10, 4, 0.01)

simulate <- function(num_sim, iStart, R0, kappa, rho=0){
  
  vecS <- c()
  for (i in 1:num_sim) {
    
    S <- establish(iStart, R0, kappa, rho)
    vecS <- c(vecS, S)
  }
  
  prob <- sum(vecS)/length(vecS)
  return(prob)
}

simulate(1000, 1, 2, 0.01)  
## How are we going to define establishment?
## What's a good practical definition (avoid running forever!)
## What arguments does this function need?



library(tidyverse)

crossing(I = 1, 
         R0 = seq(1, 2, 0.5),
         kappa = seq(0.1, 2.25, 0.25),
         rho = seq(0, 1, 0.5),
         runs = 1000) -> varies

results <- varies %>% 
  mutate(row = row_number()) %>% 
  nest(data = c(runs, I, R0, kappa, rho)) %>% 
  mutate(prob = as.numeric(map(data, ~ simulate(.$runs, .$I, .$R0, .$kappa, .$rho)))) %>% 
  unnest(data)


ggplot(data = results)+ 
  geom_line(aes(x = kappa, y = prob)) +
  facet_grid(R0~rho, labeller = label_both)
