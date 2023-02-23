###############
# Pilotstudie #   WIND ONLY SO FAR
############### 

setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/Paper 2 population dispersal/Data')

original_centroids = read.csv('GRUK_1997_1km_ocentroids.csv')
head(original_centroids)

df <- read.table('PolygonData.txt',sep = ';')
head(df)

library(ggplot2)

###############
# WALD-greier #
###############

# WALD basic function
wald.b <- function(x) {
  x = ((lambda/(2*pi*x^3))^0.5)*exp(-((lambda*(x-u)^2)/(2*u^2*x)))
  return(x)
}
v <- 2   #vindhastighet/2 (m/s) (U)
k <- 0.35 # constant representing effective eddy size/ mixing lenght inside the canopy
sigma <- 0.10     # assume 0.1 for pilot, but actually sigma^2 = k*h*(2(sigma/v))
# x<-      # distance, in meters (?)
# h<-      # seed release height (H)
# tv<-     # terminal velocity (F)


setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/Paper 2 population dispersal/Data')

df <- read.table('PolygonData.txt',sep = ';',dec = ',') # XX polygoner, hentet fra 

#Plan: re-calculate connectivity of the Gressholmen polygons with a WALD dispersal kernel for the wind dispersed species!
# wind WALD
x<- as.numeric(c(1:1000))    # x represents a vector of distances in meters. I'm making it roughly the same length (1000 m) as the greatest distance of coastline points on opposite sides of the island
h<- (sum(0.125,0.35, 0.17, 0.605, 0.51))/5; h     # average seed release height for the 5 species
tv<- 1.074      # average seed terminal velocity for the 5 species
u<- (h*v)/tv    # 0.6555
lambda <- (h/sigma)^2
# alpha <- integrate(wald.b, 0, 1000) # =1
# i is the polygon I'm calculating the connectivity(S) for. The distance d[ij] is between polygon i and another polygon j. 
(centroids<- data.frame(df$lat,df$lon)) # create df of center points of the polygons
(distances<- as.matrix(dist(centroids, diag = TRUE, upper = TRUE))) # 
(diag(distances)<- NA) # set the diagonal values to NA so they don't count as 0's
# (distances[distances>1000]) <- NA # remove distances >1km
(A<-df$ar/10000) # make it hectares to match original data
(d<-distances/1000) # transform from meters to kilometers
alpha<- 1 # set to the apparent top of > plot(wald.b) for now. Alternative to specify alpha as a function? Or maybe think of it as the optima of the dispersal kernel for the wind species
# alpha could also maybe be set to u 
# df$S<- sum(exp(-alpha * distances[i,j]) * ar[j], j!=i, na.rm = TRUE); df$S
# df$S<- apply(X, MARGIN = c(j!=i), exp(-alpha * distances[i,j]) * ar[j] )
(df$S <- colSums(exp( -alpha * d)*A, na.rm = TRUE)) # calculated connectivity for the 15 polygons
# not sure why this doesn't produce the same connectivity as original (df$konn). 
# In C:\Users\evaler\OneDrive - Universitetet i Oslo\Eva\PHD\Paper 2 population dispersal\Data fra Marianne Evju ikke rør\BRUK DENNE matrise - polygoner, sheet ARKO-polygoner, far right under 'Annes resultater'
# a column called "Antall polygoner medregnet" seems to maybe indicare that there are more 
# polygons part of the calculation than the 15 on gressholmen. This does not make sense since 
# there are not that many polygons within 1km of any of those polygons, even counting the 
# 'GRUK 2014 islands' polygons (there might be about 40 polygons within 1km radius counting these)... 
plot(df$konn,df$S) # there is no obvious relationships between my calculated S and the original S...?

(test <- colSums(exp( -alpha * d)*A, na.rm = TRUE))