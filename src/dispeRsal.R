####################################
##     dispeRsal function test    ##
####################################

library(nlme)
library(tidyverse)

# Instructions and preparing data
#--------------------------------------------------------------------------------------------
# Your dataset should have species as rows and further data organized in columns.
# names of the columns and the attributes in them have to be exactly as described

 dispDat <- read.csv('HabspesTraits_clean.csv')
 dispDat <- subset(dispDat, yr=='2019') # remove species duplicates across years by just using one yr
 dispDat$canopy.height <- log10((dispDat$canopy.height)/100) # log10-transform variables, canopy height used as proxy for release height, *100 to get m
 dispDat$seed.mass <- log10(dispDat$seed.mass)
 dispDat$terminal.velocity <- log10(dispDat$terminal.velocity)
 dispDat <- dispDat %>%
   select(Species = Lat.navn,
          DS = dispSyndr,
          GF = growthForm,
          RH = canopy.height,
          SM = seed.mass,
          TV = terminal.velocity)
 dispDat$DS <- as.character(dispDat$DS);  dispDat$GF <- as.character(dispDat$GF)
 # change categories for growth form (tree, shrub, herb) and dispersal syndrome to match dispeRsal categories
 dispDat[which(dispDat$GF == 'gram'),3] <- 'herb'
 dispDat[which(dispDat$GF == 'woody'),3] <- 'tree' # this is wrong, some are shrubs
 dispDat[c(10,11,38),3] <- 'shrub' # change cotoneasters, rosa majalis to shrubs
 dispDat[which(dispDat$DS == 'ballistically'),2] <- 'ballistic' #
 dispDat[which(dispDat$DS == 'endozoochory'),2] <- 'animal'
 dispDat[which(dispDat$DS == 'exozoochory'),2] <- 'animal'
 dispDat[which(dispDat$DS == 'wind.1'),2] <- 'wind.special'
 dispDat[which(dispDat$DS == 'unassisted'),2] <- 'wind.none'
 # animal - all vertebrate dispersed seeds, without distinction between attached (epichorous) and ingested (endochorous) dispersal of seeds
 # ant - seeds ant-dispersed, often with the help of an elaiosome, i.e. a nutrient-rich seed appendage
 # ballistic - seeds are dispersed by some kind of ‘explosive’ mechansism of the motherplant
 # wind.none - seeds are dispersed usually by wind, but have no special adaptation for this dispersal vector
 # wind.special - seeds are dispersed by wind with the help of seed appendages such as wings and pappi

write.csv(dispDat,'testDispeRsalData.csv')
dispDat <- read.csv('testDispeRsalData.csv')

# load and test dispeRsal function
#------------------------------------------------------------------------------------------
load('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/Paper 2 population dispersal/dispeRsal function/dispeRsal.rda')
# 1 = model 1: dispersal distance = DS + GF + TV
# 2 = model 2: dispersal distance = DS + GF + SM + RH
# 3 = model 3: dispersal distance = DS + GF + RH
# 4 = model 4: dispersal distance = DS + GF + SM
# 5 = model 5: dispersal distance = DS + GF
dispeRsal(dispDat, 
          model = 1, # dispersal distance = DS + GF + TV
          CI = TRUE, # if TRUE, a lower and upper confidence limit are given for the predicted dispersal distances. Currently, this is only implemented for the predicted values from the fixed effects. 
          random = FALSE, # should the taxonomy of the species be modeled as a random variable in a linear mixed model (TRUE)?
          tax = "family", # when random = TRUE, defines if only the order ("order") or order and family ("family") are considered
          write.result = TRUE)  # when TRUE, writes the output data frame that contains the predicted
                                 # dispersal distances to a file called predictedDD.txt in your working
                                 # directory. If a mixed effects model is chosen, a second file called
                                 # unmatched.txt is written to the working directory. This file contains the
                                 # species names of the species that could not be matched to the taxonomy on
                                 # theplantlist org and have therefore been dismissed from the prediction.
out1 <- read.delim('predictedDD.txt',sep = ' ') # 34 species
write.csv(out1,'predictedDDmodel1.csv')

dispeRsal(dispDat, 
          model = 2, # dispersal distance = DS + GF + SM + RH
          CI = TRUE, 
          random = FALSE, 
          tax = "family", 
          write.result = TRUE) 
out2 <- read.delim('predictedDD.txt',sep = ' ') # 45 species
write.csv(out2,'predictedDDmodel2.csv')

# explore the output
#------------------------------------------------------------------------------------------
out2 <- read.csv('predictedDDmodel2.csv')
out2$maxdist <- exp(out2$log10MDD)
par(bg=NA,family = 'mono')
boxplot(out2$maxdist~out2$DS, 
        main = 'Dispersal distance per vector', 
        ylab = 'modeled 99th percentile distance (m)',
        xlab = 'Dispersal vector',
        col = 'white'
        )
head(out2)





