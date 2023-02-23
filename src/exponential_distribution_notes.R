# exponential distribution
# https://r-coder.com/exponential-distribution-r/ 

x = 10 #vector of quantiles
alpha = 1
d <- rexp(x, rate = alpha)
plot(d)

plot(x, dexp(x, alpha))

#---------
#Grid of X-axis values
x <- seq(from = 0, to = 10, 0.1)

# alpha = 2
plot(x, dexp(x, 2), type = "l",
		 lwd = 2, col = "red")
# alpha = 1
lines(x, dexp(x, rate = 1), col = "blue", lty = 1, lwd = 2)

# Adding a legend
legend("topright", c(expression(paste(, alpha)), "2", "1"),
			 lty = c(0, 1, 1), col = c("blue", "red"))
#---------
# About the exponential distribution from Wikipedia: "In probability theory and statistics, the exponential distribution is the probability distribution of the time between events in a Poisson point process, i.e., a process in which events occur continuously and independently at a constant average rate. It is a particular case of the gamma distribution. It is the continuous analogue of the geometric distribution, and it has the key property of being memoryless. In addition to being used for the analysis of Poisson point processes it is found in various other contexts. "

# dexp	Exponential density (Probability density function)
# pexp	Exponential distribution (Cumulative distribution function)
# qexp	Quantile function of the exponential distribution
# rexp	Exponential random number generation