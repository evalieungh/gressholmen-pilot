####################################
##    General dispersal kernels   ##
####################################
data <- read.csv('HabspesTraits_clean.csv')

# cleaning the data as needed for testing - move this to 1_tidythedata.R when finished! 
library(tidyverse)
data$replicate <- data$yr %>% # make a new column 'replicate' specifying whether it's the first or second obs of each species
  gsub(pattern = '2012', replacement = '1', .) %>%
  gsub(pattern = '2019', replacement = '2', .)
head(data$yr); head(data$replicate)
data <- data %>% select(1:17,yr,replicate, everything())
data <- subset(data,data$seed.number != 'NA') # remove all rows where seed number is unknown! skip this step when trait file is complete!
data <- subset(data,data$yr == '2019') # one year at a time for now... 
head(data[,18:41])



# Exponential power function - this looks like the most promising general function
#---------------------------------------------------------------------------------------------------------
ExP = function(d) {(b/(2*pi*(a^2)*gamma(2/b))*exp(-((d^b)/(a^b))))}
a = 1 # scale parameter. Set to 1 at random - these should maybe be modelled, as a separate layer in a hierarchy, perhaps?
b = 1 # shape parameter. Set to 1 at random
plot(ExP(1:100))

## mean distance of ExP function
meanExP = function(ExP) {a((gamma(3/b)/gamma(2/b)))} # this is maybe an OK proxy for a target's dispersal general dispersal capability?


# non-linear mixed model for fitting the ExP function to data comprising replicate dispersal kernels
 # Seed density measures y[r,d] at distance d for replicate r 
fitted = function(y) {
                      ((Q[r]+u[r])*
                      ((b/(2*pi*(a^2)*gamma(2/b))*
                      exp(-((d^b)/(a^b))))))
                      #+e[r,d]
}  # do I need a for-loop inside there to compute y for [r,d] ?

## specify parameter values
d = 1:37 # distance (non-zero values). is a distance matrix between patches correct? Maybe not - does it represent measured distances from the mother plant? 
 # Or potential distances from the mother plant? I would guess it should be a range or vector...? Trying 1:74 meters for the moment (cannot start at 0 
 # because y below is divided by it!) If this is supposed to be measured distances, maybe model it as a hierarchy layer...
y = as.matrix((rpois(37,1))/d) # random counts divided by distances - should be 'seed density' measurements
 # as.matrix(data$seed.number/d) # ? 
r = as.factor(data$replicate) # replicate. Year in my case? Just using one year for now, so this is all the same
Q = data$seed.number # number of seeds dispersed. Using the trait values for 'seed number'
### "The total number of seeds for replicate r is Q[r] with random effects u[r], which has a normal distribution"
u = rnorm(37,0,1) # random effect? what should mean and sd be?
a = 1 # scale parameter. Set to 1 at random - what should these be?
b = 1 # shape parameter. Set to 1 at random
### "residual errors e[r,d] are Poisson"
e = rpois(37,1) # residual errors. what should lambda be? 


# and then? make a for loop or use apply? Need to calculate for *two* sequences (r and d)... Maybe try to skip r for now? 
# "Individual seed density measures y[r,d] are at distance d  and for replicate r."
EstimatesOfY <- apply(y, # 
                   MARGIN = c(1,2),
                   FUN = fitted
                   )
# warnings() # If the numbers in parameter definitions above are not equal, get warnings: "In (Q[r] + u[r]) * ((b/(2 * pi * (a^2) * gamma(2/b)) *  ... : longer object length is not a multiple of shorter object length"
## results in a type 'double' value. Looks like a matrix but isn't? 
typeof(EstimatesOfY) # "double" - what does it mean? 
str(EstimatesOfY)
estimates <- as.matrix.data.frame(EstimatesOfY)
plot(d, estimates[,1])
lines(d, estimates[,1])




# Log-hyperbolic secant function - alternative generic dispersal kernel, but less promising to use
#---------------------------------------------------------------------------------------------------------
logS = function(t) {a[i,j]*x^b[i,j]}
