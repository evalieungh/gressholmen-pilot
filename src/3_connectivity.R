#########################################
##         PAPER II GRESSHOLMEN       ###
#########################################
##            CONNECTIVITY            ###
#########################################
#               Script 3                #

# nonater fra møte 2019 på NINA med Stefan, Marianne, Siri :
# de 'fine' polygonene utenfor ARKO-kartleggingen er naturbase-polygoner? -- dobbeltsjekk disse, se også om de har blitt endra senere!
# originalskript ligger på Stefans gamle brukerkonto, ikke tilgjengelig uten å gå via backups
# tot 157 polygoner i masterfil som er brukt i utregning
# det ser ut om de modellerte polygonene er blandet inn i 1997-filene hos meg -- de må skilles ut og tas med. Da blir det et mer riktig 
# modelled polygons -- area should be downweighted to 25 % , and only include the ones with rppp> 0.62
# finn ut hvilke polygoner som har rppp>0.62
# area m2 -- allerede nedvektet eller ikke? forsiktig med arealene for de modellerte polygonene
# legg til en kolonne i fila for modellert/ikke 1/0 og legg til en linje hvor de modellertes areal nedjusteres med 75%!
# Stefan tar kontakt med IT for å få tilgang til opprinnelige filer for å sikre arealberegningen
# masterfilen inneholder de polygonene som faktisk ble brukt
# Olav, ikke Anders KW, har prodiksjonsmodellen? Fikk polygoner fra Vegard/AKW?
# noen av shapefilene kan ha vært skadet av virus/program som ødela shapefiler for lenge siden -- csv-format mer stabil
# masterfil: riktige polygoner, bare tull i atributtabellen! 

library(tidyverse)
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/Paper 2 population dispersal')

# specify the correct values for comparison
arko <- data.frame(
          kode = c('35_1','35_2','35_3','35_4','35_5','35_6','35_7','35_8','35_9','35_10','35_11','35_12','35_13','35_14','35_15'),
          arkoS = c(3.841614196,3.839119398,4.014336668,3.904987988,3.93836881,3.863040367,3.924794147,4.002336024,4.034776991,3.749776402,3.781638369,3.853645296,3.862690816,3.95700375,3.713498034),
          areal = c(811,2302,368,1680,273,264,477,769,1663,1287,639,858,1358,1042,2382),
          no.pol.incl = c(52,58,63,57,53,57,70,67,67,61,53,55,65,63,53) # number of polygons included in calc, according to Stefans results
          )
head(arko)


# read the data (made by exporting from QGIS) 
mcentroids <- read.csv('Data/GIS/M_78_1km_centroids.csv') # Centroids made from all the Master file polygons within the 1km buffer -- yes they overlap with the existing Master centroids
head(mcentroids) # fid cat area_m2 Shape_Area X_Centroid Y_Centroid PolyID_AST Kode OBJECTID
### are the areas in area_m2 column corrected (downscaled by 75%)? Yes! Looks like it. Shape_Area contains the actual polygon area in GIS.
### make a new column to indicate source of the polygon: arko, natb (=naturbase) or pred (prediksjonsmodell)
mcentroids$source <- c(rep('arko', 15),rep('NA', 63))
mcentroids$source[16:78] <- ifelse(mcentroids$area_m2[16:78] != as.integer(mcentroids$Shape_Area[16:78]), 'pred', 'natb') # have not cheched that these are correct

## sort the rows by polygon code
mcentroids <- mcentroids %>%
  mutate(Kode =  factor(Kode, levels = arko$kode)) %>%
  arrange(Kode) # sort the polygons by code
head(mcentroids)

# Calculate connectivity with the 78 polygons/centroids within 1km in the master file
centroids <- data.frame(mcentroids$X_Centroid,mcentroids$Y_Centroid) # create df of center points of the polygons
distances <- as.matrix(dist(centroids, diag = FALSE, upper = TRUE))# distance matrix (upper & diag determines whether diagonal and upper triangle are printed)
A <- mcentroids$area_m2/10000 # divide by 10000 to make it hectares 
d <- distances/1000 # /1000 transform from meters to kilometers
d[d>1] <- NA # remove distances >1km
mcentroids$no.pol.incl <- as.vector(apply(d,1,function(x){length(x[!is.na(x)])})) # number of polygons to be included in the calculation
alpha <- 1 # define alpha as 1 for now ( alpha=1 used in original ARKO calculation)
mcentroids$S <- colSums(exp( -alpha * d)*A, na.rm = TRUE) # save the calculated connectivity S as new row 
## write a csv
#write.csv(mcentroids,'ELconnectivity.csv')
## extract the 15 ARKO polygons
df <- mcentroids[1:15,] # extract only the ARKO-polygons that I'm interested in (35_1 to 35_15)
df <- df %>%
  mutate(Kode =  factor(Kode, levels = arko$kode)) %>%
  arrange(Kode) # sort the polygons by code
## check if it's a match
print(cbind(df$Kode,df$no.pol.incl,df$S,arko))
(cbind(df$Kode,df$no.pol.incl,arko$no.pol.incl))
plot(df$S,arko$arkoS) # 
cor(df$S,arko$arkoS) # 
t.test(df$S,arko$arkoS) # 
sum(df$S)
sum(arko$arkoS)

difference <- data.frame(
  S = df$S - arko$arkoS,
  no.pol.incl = df$no.pol.incl - arko$no.pol.incl)
plot(difference$S,difference$no.pol.incl)
cor(difference$S,difference$no.pol.incl) # still missing polygons? 

(cbind(mcentroids$Kode,mcentroids$S))

plot(mcentroids$no.pol.incl, mcentroids$S)


# REPEAT THE CALCULATION FOR *ALL* OF THE ARKO POLYGONS
# read the data (made by exporting from QGIS) 
ALLarko <- read.csv('ALLarko_centroids.csv') # Centroids made from all of the GRUK polygons in the Master file polygons, no buffer
head(ALLarko) # 157 centroids, headers: cat area_m2 X_Centroid Y_Centroid PunktID_AS Kode OBJECTID

# Calculate connectivity with the 78 polygons/centroids within 1km in the master file
centroids <- data.frame(ALLarko$X_Centroid,ALLarko$Y_Centroid) # centroids only
distances <- as.matrix(dist(centroids, diag = FALSE, upper = TRUE))# distance matrix (upper & diag determines whether diagonal and upper triangle are printed)
A <- ALLarko$area_m2/10000 # divide by 10000 to make it hectares 
d <- distances/1000 # /1000 transform from meters to kilometers
d[d>1] <- NA # remove distances >1km
ALLarko$no.pol.incl <- as.vector(apply(d,1,function(x){length(x[!is.na(x)])})) # number of polygons to be included in the calculation
alpha <- 1 # alpha=1 used in original ARKO calculation
ALLarko$S <- colSums(exp( -alpha * d)*A, na.rm = TRUE) # save the calculated connectivity S as new row 

## extract the 15 Gressholmen polygons
subALLarko <- ALLarko[grep('^35_',rownames(ALLarko$Kode), value = TRUE),] # grep() searches vector for position of strings which begin with "^..."
library(tidyverse)
subALLarko <- ALLarko %>% select(starts_with('35_', vars = ALLarko$Kode))
