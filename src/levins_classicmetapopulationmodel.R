# levins: Classic Metapopulation Model 
#
#https://www.jstor.org/stable/2097309?seq=2#metadata_info_tab_contents 

# p = fraction of patches occupied
# e = extinction rate parameter
# c = colonisation rate of unoccupied patches parameter -- proportional to the product of the fractions of occupied and unoccupied patches.
# dynamics of p given by:
# dp/dt = cp(1-p)-ep

# Copy function from https://rdrr.io/cran/primer/man/levins.html 
# use with ode() in the deSolve package
# usage: levins(t, y, parms)
# t 			Argument for time
# y 			A scalar for the population variable
# parms 	Vector or list of parameters
library(deSolve)
function (t, y, parms)
{
	p <- y[1]
	with(as.list(parms), {
		dp <- ci * p * (1 - p) - e * p
		return(list(dp))
	})
}
library(deSolve)
p <- c(ci=.1, e=.01)
time <- 1:10
initialN <- .3
out <- ode(y=initialN, times=time, func=levins, parms=p)
plot(time, out[,-1], type='l')
