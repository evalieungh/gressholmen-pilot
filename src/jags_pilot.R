#############################
##   JAGS PILOT PAPER II   ##
#############################

library(rjags)
library(DHARMa)
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/Paper 2 population dispersal/Data')

# Aim: Make the connectivity model for polygons with a nested dispersal kernel model.
# Maybe a log-normal as a sort of null model for all dispersal vectors?

dat <- read.table('PolygonData.txt',sep = ";")
(centroids <- data.frame(dat$lat,dat$lon))
d <- (as.matrix(dist(centroids, diag = TRUE, upper = TRUE))/1000)
(diag(d) <- NA)
(A <- dat$ar/10000)


#S <- colSums(exp(-alpha * d)*A, na.rm = TRUE)  # this works, see 'pilot.R'
#S <- sum(exp(-alpha * distances[i,j]) * ar[j], j!=i, na.rm = TRUE) # not working but more in line with iteration setup

# NB remember that JAGS language is not R, but looks like it...

##############################################################################################
# Model specification
sink(PilotModel.txt)
cat("
  model{
  # Connectivity model
  for(i in 1:n.dat){
  S[i] <- sum(exp(-alpha * distances[i,j]) * A[j], j!=i, na.rm = TRUE)

  # Alpha model
  alpha <- function(x) {
  x = ((lambda/(2*pi*x^3))^0.5)*exp(-((lambda*(x-u)^2)/(2*u^2*x)))
  return(x)
  }  

}")




