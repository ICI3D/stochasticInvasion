# generalized random gamma (understands kappa=0, underlying mean is always 1)
grgam <- function(num, kappa){
	if (kappa==0) return(rep(1, num))
	return(rgamma(num, scale=kappa, shape=1/kappa))
}

