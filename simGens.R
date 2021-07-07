
## Simulate a disease spreading generation by generation
## Ignore:
#### Time (just count generations)
#### Total population size (just count infected individuals)
####### This means we're assuming we can neglect the susceptibles going down
#### ICI3D perspective (we're just going to use the Lloyd-Smith perspective)
## Assume: process does not change through time

## LS say (correctly) that a simple ODE model corresponds to an exponential distribution of infectiousness: this is because recovery is modeled as a random [constant β × exponentially distributed Duration]
## kappa is CV² (unitless and equal to 1/a, where a is a standard shape parameter)

set.seed(0707)

generation <- function(I, R0, kappa){
	gshape <- 1/kappa
	gscale <- R0*kappa
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
establish <- function(iStart, R0, kappa, ...){
	I <- iStart
	I <- generation(I, R0, kappa)
	## FIXME: Put this in a while() loop and get it to run until we do or don't figure out whether disease has established.
	return(TRUE)
}

## How are we going to define establishment?
## What's a good practical definition (avoid running forever!)
## What arguments does this function need?

