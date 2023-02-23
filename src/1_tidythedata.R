######################
#   Tidy the data    #
######################
#      Script 1      #
#      by Eva L      #

# skip this script and instead load the data from the folder below unless you're tidying something else

# 

# polygon data
pol <- read.csv('Data/Arkiv/PolygonNiN.csv', sep=';')
pol <- mutate_at(pol, vars(tree,shrub,RA.SJ,SE), as.factor) # treat the NiN variables as factors (instead of integers) 
head(pol)

# add data from "BRUK DENNE matrise - polygoner.xlsx" - 2012
pol$No.species.12	  <- c(84,	98,	73,	107,	79,	84,	89,	101,	91,	101,	86,	73,	74,	80,	68, rep('NA',25)) # number of species in total, 2012?
pol$Hab.spes.12	    <- c(20,	24,	17,	26,	22,	17,	28,	30,	25,	21,	25,	18,	20,	18,	15, rep('NA',25)) # number of habitat specialists, 2012 - is this correct? Needs to be double checked. 
pol$RL.species.12	  <- c(7,	7,	3,	7,	7,	6,	5,	7,	6,	3,	5,	5,	6,	9,	4, rep('NA',25)) # number of Red list species, 2012
pol$FA.species.12   <- c(6,	5,	3,	8,	3,	2,	1,	6,	7,	8,	7,	6,	6,	3,	6, rep('NA',25)) # "fremmedarter?"
pol$area            <- c(811,	2302,	368,	1680,	273,	264,	477,	769,	1663,	1287,	639,	858,	1358,	1042,	2382, rep('NA',25)) # polygon size
pol$konn.12         <- c(3.841614196,	3.839119398,	4.014336668,	3.904987988,	3.93836881,	3.863040366,	
                         3.924794147,	4.002336024,	4.034776991,	3.749776401,	3.781638369,	3.853645296,	
                         3.862690816,	3.95700375,	3.713498034, rep('NA',25)) # original utregnet konnektivitet

# Connectivity calculated by EL - picked up from script 3_connectivity.R
pol$S <- c(read.csv('Data/Arkiv/ELconnectivity.csv')[1:15,'S'], rep('NA',25))


# pick up objects from script 2_basics.R and add as new columns
pol$immigrations  <- c(p_immigrations,rep('NA',25))
pol$extinctions   <- c(p_extinctions,rep('NA',25))

# save/overwrite file
# write.csv(pol, file = 'Data/Polygondata.csv', row.names = FALSE)

# transform data format into long list for IPT data paper
# --------------------------------------------------------------
library(tidyverse)
d <- read.csv('Data/data.csv')
d <- d[,-3]
head(d)

d <- d %>% 
  group_by(year, species) %>%
  pivot_longer(3:17)

d$eventID <- paste(d$name,d$year,sep = '-')
d$occurrenceID <- c(1:2205)

d <- d %>% 
  group_by(species) %>%
  select(occurrenceID,
         eventID,
         scientificName = species,
         occurrenceStatus = value)
  
write.csv(d,'Data/data_list.csv')
