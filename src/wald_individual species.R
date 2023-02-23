###################################
##  WALD for individual species  ##
###################################


ara.tha.h<-0.125 # seed release height
ara.tha.tv<-1.15 # seed terminal velocity
ara.tha.u<-(ara.tha.h*v)/ara.tha.tv;ara.tha.u # location parameter
ara.tha.tau<-ara.tha.h^2;ara.tha.tau # scale parameter tau, assuming turbulence param=1
hie.sec.h<-0.35; hie.sec.tv<-0.6
lin.cat.h<-0.17; lin.cat.tv<-1.86
hyp.mac.h<-0.605; hyp.mac.tv<-0.57
lin.vul.h<-0.51; lin.vul.tv<-1.19

# WALD basic function
wald.b <- function(x) {
  x = ((lambda/(2*pi*x^3))^0.5)*exp(-((lambda*(x-u)^2)/(2*u^2*x)))
  return(x)
}
v <- 2   #vindhastighet/2 (m/s) (U)
k <- 0.35 # constant representing effective eddy size/ mixing lenght inside the canopy
sigma <- 0.10     # assume 0.1 for pilot, but actually sigma^2 = k*h*(2(sigma/v))

# Ara.tha
x<- c(1:15)     # distance, in meters (?)
h<- ara.tha.h      # seed release height (H)
tv<- ara.tha.tv     # terminal velocity (F)
u<-(h*v)/tv     # mu, 
lambda <- (h/sigma)^2
ara.tha.x<- wald.b(x)
plot(ara.tha.x)

# Hie.sec
x<- c(1:15)
h<- hie.sec.h      
tv<- hie.sec.tv     
u<-(h*v)/tv    
lambda <- (h/sigma)^2
hie.sec.x<- wald.b(x)
plot(hie.sec.x)

# Lin.cat

# Hyp.mac
x<- c(1:15)
h<- hyp.mac.h     
tv<- hyp.mac.tv     
u<-(h*v)/tv    
lambda <- (h/sigma)^2
hyp.mac.x<- wald.b(x)
plot(hyp.mac.x)

# Lin.vul
x<- c(1:15)
h<- lin.vul.h  
tv<- lin.vul.tv     
u<-(h*v)/tv    
lambda <- (h/sigma)^2
lin.vul.x<- wald.b(x)
plot(lin.vul.x)