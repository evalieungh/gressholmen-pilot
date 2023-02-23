
########### LOGISTIC REGRESSION #############
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/Paper 2 population dispersal/Data')
#library(MASS)
#library(ggplot2)
#library(png)
#library(grid)
#seed<- readPNG('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/Presentasjoner/Poster/seed_wind.png')

wind<- read.csv('Pilot_vind.csv',sep=';',dec=','); wind
animal<- read.csv('Pilot_dyr.csv',sep=';',dec=','); animal
novector<- read.csv('Pilot_ingen.csv',sep=';',dec=','); novector
all<- read.csv('Pilot_all.csv',sep = ';',dec = ',')

# Wind (red)
(mw<- glm(pres ~ ar + konn, family = binomial, data = wind))
summary(mw) # neither term significant - were these polygons sampeled according to the four categories large-connected/small-connected/large-isolated/small-isolated?
#(Intercept) -3.3947964  ar  0.0006607   konn 0.6983889
(mw_p<- exp(-3.3947964 + wind$ar*0.0006607 + wind$konn*0.6983889))

# Animal (green)
ma<- glm(pres ~ ar + konn, family = binomial, data = animal); ma
summary(ma) # (intercept) -1.9248287, ar 0.0002006, konn 0.4466405
ma_p<- exp(-1.9248287 + animal$ar*0.0002006 + animal$konn*0.4466405)

# Novector (blue)
mn<- glm(pres ~ ar + konn, family = binomial, data = novector); mn
summary(mn) #(Intercept)  1.9753667, ar 0.0001543,    konn -0.5304963 
mn_p<- exp(1.9753667 + novector$ar*0.0001543 - novector$konn*0.5304963)

# plot versus area
plot(wind$ar,mw_p,col=2,ylab = "predicted presence", xlab = "area") # ser ut som om vindspredte arter er mer avhengig av størrelse på habitatet enn de andre... mer R-selekterte arter ?
points(animal$ar,ma_p,col=3) #
points(novector$ar,mn_p,col=4)


# plot versus connectivity
plot(wind$konn,mw_p)
points(animal$konn,ma_p,col=3)
points(novector$konn,mn_p,col=4)
