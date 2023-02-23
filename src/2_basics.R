########################################
##         PAPER II GRESSHOLMEN       ##
########################################
##             BASIC STATS            ##
########################################
#               Script 2               #

# Script by Eva Lieungh

# summary statistics, turnover between years etc

d<- read.csv('Data/data.csv')
head(d)
tail(d)

library(tidyverse)
d <- d %>% 
  select(species = Lat.navn, p1:p15) %>%
  pivot_longer(!species,
               names_to = 'polygon',
               values_to = 'pres')
d$year <- c(rep(
  c(rep('2012',15),rep('2019',15),rep('2020',15)),
    49))# 49 arter (length(unique(d$No.navn)))
head(d)

table(d$pres,d$year)
table(d$pres,d$polygon)


# turnover between the years (NOT COMPLETE)
## see e.g. https://github.com/AlexRyabov/turnover 


## Immigratino and extinction - difference in species counts for each year and  polygon (subtract the 2019 from the 2012 data, and 2020 from 2019)

d12<- d[d$yr=='2012',2:18]
d19<- d[d$yr=='2019',2:18]
d20<- d[d$yr=='2020',2:18]

### number of "immigrants" in 2019 of each species for each polygon, e.g. absence in 2012 and presence in 2019 gets +1, species who disappeared get -1
immigrants19 <- data.frame(
  p1 = d19[,2] - d12[,2],
  p2 = d19[,3] - d12[,3],
  p3 = d19[,4] - d12[,4],
  p4 = d19[,5] - d12[,5],
  p5 = d19[,6] - d12[,6],
  p6 = d19[,7] - d12[,7],
  p7 = d19[,8] - d12[,8],
  p8 = d19[,9] - d12[,9],
  p9 = d19[,10] - d12[,10],
  p10 = d19[,11] - d12[,11],
  p11 = d19[,12] - d12[,12],
  p12 = d19[,13] - d12[,13],
  p13 = d19[,14] - d12[,14],
  p14 = d19[,15] - d12[,15],
  p15 = d19[,16] - d12[,16]
)
head(immigrants19)
colSums(immigrants19) # most polygons lose species, except no.1,6,12,14 and 15. 

### number of "immigrants" in 2020 of each species for each polygon, e.g. absence in 2019 and presence in 2020 gets +1, species who disappeared get -1
immigrants20 <- data.frame(
  p1 = d20[,2] - d19[,2],
  p2 = d20[,3] - d19[,3],
  p3 = d20[,4] - d19[,4],
  p4 = d20[,5] - d19[,5],
  p5 = d20[,6] - d19[,6],
  p6 = d20[,7] - d19[,7],
  p7 = d20[,8] - d19[,8],
  p8 = d20[,9] - d19[,9],
  p9 = d20[,10] - d19[,10],
  p10 = d20[,11] - d19[,11],
  p11 = d20[,12] - d19[,12],
  p19 = d20[,13] - d19[,13],
  p13 = d20[,14] - d19[,14],
  p14 = d20[,15] - d19[,15],
  p15 = d20[,16] - d19[,16]
)
head(immigrants20)
colSums(immigrants20)#all polygons gain 1-7 species! Maybe I was better at discovering them, or maybe they had some good years after the 2018 drought?

### total number of immigrations vs extinctions from 2012-2019
p_immigrations     <- colSums(immigrants19=="1") # immigrations per polygon
sp_immigrations    <- rowSums(immigrants19=="1") # immigrations per species
sum(colSums(immigrants19=="1")) # total number of immigrations in study area = 64

p_extinctions      <- colSums(immigrants19=="-1") # extinctions per polygon
sp_extinctions     <-rowSums(immigrants19=="-1") # extinctions per species
sum(colSums(immigrants19=="-1")) # total number of extinctions in study area = 74

sum(colSums(immigrants19=="0")) # no change = 597 (out of 753 values in the df)


### total number of immigrations vs extinctions from 2019-2020
p_immigrations     <- colSums(immigrants20=="1") # immigrations per polygon
sp_immigrations    <- rowSums(immigrants20=="1") # immigrations per species
sum(colSums(immigrants20=="1")) # total number of immigrations in study area = 65

p_extinctions      <- colSums(immigrants20=="-1") # extinctions per polygon
sp_extinctions     <-rowSums(immigrants20=="-1") # extinctions per species
sum(colSums(immigrants20=="-1")) # total number of extinctions in study area = 15

sum(colSums(immigrants20=="0")) # no change = 655 (out of 753 values in the df)



# Polygon level stuff
#---------------------------------------
# pick up data made in script 1_tidythedata.R
pol <- read.csv('Polygondata.csv')
head(pol)
## how many species in each polygon? 
pol$no.species <- c(colSums(d[,2:16],na.rm = TRUE),rep('NA',25))

# turnover from 2012 to 2019       ---------- not really turnover because it might be that in a single polygon a species has appeared and disappeared several times, maybe by dispersal or from seeds

# Connectivity -- ALL species, 15 polygons
## relationship between connectivity and immigration or extinctions? Doesn't look like it.
plot(pol$S,pol$immigrations, pch= 16, ylim = c(0,70), xlim = c(3.6,4.2))
points(pol$S,pol$extinctions, pch=16, col=2) 
## relationship between S and no. of species? Doesn't look like it.
plot(pol$S, pol$no.sp.19)
points(pol$S, pol$no.sp.12,col=2)
## relationship between S and habitat specialists? Doesn't look like it.
plot(pol$S, pol$Hab.spes.12) 
#points(pol$S, pol$Hab.spes.19,col=2) # add when available! 
## relationship between S and RL, FA species? Doesn't look like it.
plot(pol$S, pol$RL.species.12, ylim = c(0,12))
points(pol$S, pol$FA.species.12,col=2)

## Regrowth in the polygons I have some data for, on NiN measuring scale A9 (35_... 2,6,7,11,12)
t12 <- c(3,0,0,0,0) # tree cover (transformed to NiN A9 scale) in 2012 in (average of) vegetation analysis plots 
b12 <- c(3,0,2,3,4) # shrub cover 2012, -"-
t19 <- c(4,3,2,2,3) # tree cover in 2019 on polygon level
b19 <- c(4,4,4,5,5) # shrub cover -"-
plot(jitter(t12), jitter(t19), ylim = c(0,5.5), xlim = c(0,5.5))
points(jitter(b12), jitter(b19), col=2)
cor(t12,t19) # = 0.8
t.test(t12,t19) # p=0.018, true difference between the years
cor(b12,b19) # = 0.66
t.test(b12,b19) # p=0.039, true difference between years
cor(c(t12,b12),c(t19,b19)) # = 0.8                                   ----------how reasonable is it to combine tree and shrub cover? 
t.test(c(t12,b12),c(t19,b19)) # p=0.004, true difference between years --------yes, there is regrowth happening?
    # How should regrowth be interpreted when the recording is made with the relatively crude and non.linear A9 scale? It cannot be back-transformed to percentage cover I think. 

## Is there a connection between the ecosystem types and the species count, regrowth and erosion?
T1 <- subset(pol, pol$T1 != 'NA')
T2 <- subset(pol, pol$T2 != 'NA')
T32 <- subset(pol, pol$T32 != 'NA')
T4 <- subset(pol, pol$T4 != 'NA') # one of the 4 records is T6 -----should rename the column to 'other'?

boxplot(T1$gain, xlim = c(0,4), ylim = c(-20,50), main = 'Species gain/loss') # 
boxplot(T2$gain, at = 2, add = TRUE)
boxplot(T32$gain, at = 3, add = TRUE)
axis(1,at = c(1,2,3), labels = c('T1','T2','T32'))
#boxplot(T4$gain, at = 4, add = TRUE) # removed because none of the 15 ARKO polygons are in this subset, hence no gain numbers!

### What is happening in T1? Is the type connected to regrowth, erosion etc? (Numbers represent ALL the polygons where T1 occurred >20%, no matter the amount or presence of other ecosystems!)
mean(T1$T1p) # = 31.42857 %
a1<-mean(na.omit(T1$no.sp.12)) # = 94.4
b1<-mean(na.omit(T1$no.sp.19)) # = 102.6
c1<-mean(na.omit(T1$gain)) # = 8.2
d1<-mean(as.numeric(T1$tree)) # = 3.785714
e1<-mean(as.numeric(T1$shrub)) # = 3.48571
f1<-mean(as.numeric(T1$RA.SJ)) # = 2.785714
g1<-mean(as.numeric(T1$SE)) # = 1.785714
(T1means<- c(a1,b1,c1,d1,e1,f1,g1))

### What is happening in T2? Is the type connected to regrowth, erosion etc? 
mean(T2$T2p) # = 40.37037 %
a2<-mean(na.omit(T2$no.sp.12)) # = 87.846154
b2<-mean(na.omit(T2$no.sp.19)) # = 93.769231
c2<-mean(na.omit(T2$gain)) # = 5.923077
d2<-mean(as.numeric(T2$tree)) # = 3.814815
e2<-mean(as.numeric(T2$shrub)) # = 3.629630
f2<-mean(as.numeric(T2$RA.SJ)) # = 2.370370
g2<-mean(as.numeric(T2$SE)) # = 1.629630
(T2means<- c(a2,b2,c2,d2,e2,f2,g2))
#### T2 very rough plots
plot(T2$T2p,T2$tree, ylim = c(0,7), xlim = c(0,100), main = 'NiN variables in the 27 polygons containing T2', ylab = 'Variable value', xlab = 'T2 cover (jittered %)')
points(jitter(T2$T2p),T2$shrub, col = 2)
points(jitter(T2$T2p),T2$RA.SJ, col = 3)
points(jitter(T2$T2p),T2$SE, col = 4)
abline(lm(T2$tree ~ T2$T2p)) # more T2, less trees?
abline(lm(T2$shrub ~ T2$T2p), col = 2) # more T2, same/less shrubs?
abline(lm(T2$RA.SJ ~ T2$T2p), col = 3) # more T2, less regrowth?
abline(lm(T2$SE ~ T2$T2p), col = 4)  # more T2, same/more erosion?
legend('topleft', c('tree canopy cover','shrub cover','Regrowth succession (7RA.SJ)','Erosion/wear(7SE)'), col = c(1,2,3,4), lty = 1)

### What is happening in T32? Is the type connected to regrowth, erosion etc? 
mean(T32$T32p) # = 65.58824 %
a3<-mean(na.omit(T32$no.sp.12)) # = 87.285714
b3<-mean(na.omit(T32$no.sp.19)) # = 96.0
c3<-mean(na.omit(T32$gain)) # = 8.714286 
d3<-mean(as.numeric(T32$tree)) # = 3.794118
e3<-mean(as.numeric(T32$shrub)) # = 3.852941
f3<-mean(as.numeric(T32$RA.SJ)) # = 2.676471
g3<-mean(as.numeric(T32$SE)) # = 1.470588
(T32means<- c(a3,b3,c3,d3,e3,f3,g3))
#### T32 very rough plots
plot(T32$T32p,T32$tree, ylim = c(0,7), xlim = c(0,100), main = 'NiN variables in the 34 polygons containing T32', ylab = 'Variable value', xlab = 'T32 cover (jittered %)')
points(jitter(T32$T32p),T32$shrub, col = 2)
points(jitter(T32$T32p),T32$RA.SJ, col = 3)
points(jitter(T32$T32p),T32$SE, col = 4)
abline(lm(T32$tree ~ T32$T32p)) # more T32, more trees?
abline(lm(T32$shrub ~ T32$T32p), col = 2) # more T32, more shrubs?
abline(lm(T32$RA.SJ ~ T32$T32p), col = 3) # more T32, no difference in regrowth?
abline(lm(T32$SE ~ T32$T32p), col = 4)  # more T32, less erosion?
legend('topleft', c('tree canopy cover','shrub cover','Regrowth succession (7RA.SJ)','Erosion/wear(7SE)'), col = c(1,2,3,4), lty = 1)

## How do the three types (T-1-2-32) compare?
(tab1<-data.frame(rbind(c('no.sp.12','no.sp.19','gain','tree','shrub','RA.SJ','SE'),T1means,T2means,T32means)))


## Is there a connection between regrowth and species gain/loss? -------no apparent association
plot(pol$RA.SJ,pol$gain) # regrowth stage
cor(as.numeric(pol$RA.SJ)[0:15],pol$gain[0:15]) # = 0.35616
plot(as.numeric(pol$tree),pol$gain) # tree cover
cor(as.numeric(pol$tree)[0:15],pol$gain[0:15]) # = -0.109
plot(as.numeric(pol$shrub),pol$gain) # shrub cover
cor(as.numeric(pol$shrub)[0:15],pol$gain[0:15]) # = 0.201769



# models with presence, polygon and trait data
#----------------------------------

# manage the data sets
library(tidyverse)
pol15 <- pol[1:15,]
df1 <- df %>%
  gather()




############################
# Analysis with vegan package?
library('vegan')



###########################
## Species( group) level ##
###########################

# S vs presences

