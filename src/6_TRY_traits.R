##########################
##    TRY traits data   ##
##########################
#        Script 6       #

# look at and summarise trait data from TRY request no 8210
library(tidyverse)
traits <- read.delim('TRY/8210.txt')

# clean the data
#-------------------------------------------------
traits <- subset(traits, traits$LastName != 'Onoda') # remove the datasets with restrictions/requested coauthorships
traits <- subset(traits, traits$LastName != 'Sheremetev')
traits <- traits %>%
  select(-Reference,-Dataset,-ObsDataID,-ObservationID,
         -Replicates,-FirstName,-AccSpeciesName,-ValueKindName,-ErrorRisk,-X) %>% # remove some columns that I don't need right now
  mutate(shortName = substr(SpeciesName,1,17)) # clean up the species names in a new column to prepare for conversion to Gen_spe format

## transform long and differing species names to short Gen_spe format
genSpe <- lapply(traits$shortName, FUN = function(x){
  res = unlist(strsplit(x,'[ ]'))
  res = paste(substr(res[1],1,3),substr(res[2],1,3), sep = '_')
  return(res)
  })
traits$Latnavn <- tolower(unlist(genSpe)) # add as new column
traits <- traits %>% 
  select(-shortName,-SpeciesName) %>% # remove old name columns
  select(Latnavn, AccSpeciesID, TraitName, TraitID, everything()) %>% # reorder columns
  group_by(Latnavn) %>% # order by species
  arrange(Latnavn)

traits$OrigValueStr <- as.vector(traits$OrigValueStr)
traits <- subset(traits, traits$TraitID != 'NA') # remove columns where trait values are comments or references (not trait values)
length(unique(traits$Latnavn)) # 30 species covered
## write a csv version
write.csv(traits,'TRY/TRY_traits.csv')

#traits <- read.csv('TRY/TRY_traits.csv')

# extract trait values
#-------------------------------------------------
## start with the ones already in the data set: clonality (no/short/far), pollination (insect, wind, selfing), 
 # dispersal (unassisted,wind, endozoochory, exozoochory, water, ants, ballistic), light (Ellenberg), moisture (Ellenberg), 
 # pH (Ellenberg), nitrogen (Ellenberg), SLA, LMDC, Leaf.size,	leaf.mass,	sl.index,	seed.mass,	seed.number,	terminal.velocity
 # vegetation height
print(unique(traits$TraitName)) # traits with records in the data set(s)
length(unique(traits$LastName)) # number of data sets = 30

### clonality (no/short/far)
  # relevant traits: Plant clonal growth form
clonality <- subset(traits,traits$TraitName=="Plant clonal growth form")
t<-as.data.frame.matrix(table(clonality$Latnavn,clonality$OrigValueStr))
print(names(t))
(t$rowsums <- rowSums(t)) # sum up across columns per species - they all have some clonality (but which traits should count? Which are no/little, short and far)
t$no <- t$bulb # "no/little" clonality, defined as "little or no vegetative spread"
t$short <- # "short" creeping vegetative spread, includes tussock forming graminoids, shortly creeping rhizomes/shortly creeping and rooting at nodes, and tuberous or bulbous 
t$far <-  # "far" creeping vegetative spread, includes rhizome far creeping, extensively creeping and rooting at nodes, stolons far creeping
write.csv(t,'TRY/traitTables/clonality.csv')


### pollination 
  # relevant traits: Pollination syndrome (ARKO categories insect, wind, selfing) 
pollination <- subset(traits,traits$TraitName=="Pollination syndrome")
t<-as.data.frame.matrix(table(pollination$Latnavn,pollination$OrigValueStr))
print(names(t))
(t$i <- t$Animals + t$`bees, bumble bees, wasps, bombylides, syrphids` + t$`bees, bumblebees, wasps, bombylides, syrphids` +
       t$`beetles, flies, syrphids, wasps, medium tongued bees` + t$`bumble bees` + t$`flies, beetles` + t$hymenopteres +
       t$insect + t$`insects always` + t$`insects possible` + t$`insects the rule` + t$`pollination animals` + 
       t$`short tongued bees, syrphids, flies, beetles` + t$`syrphids, bees`)# insect/animal pollinated 
(t$w <- t$wind + t$`wind always` )# wind pollinated
(t$s <- t$`autogamous/entomogamous` + t$`geitonogamy possible` + t$selfed + t$`selfing always` + 
        t$`selfing at failure of outcrossing` + t$`selfing often` + t$`selfing possible` + t$`selfing rare` + t$`selfing the rule`) # selfing
t <- t %>% select(i,w,s)
write.csv(t,'TRY/traitTables/pollination.csv')
  # also maybe relevant: 'Flower pollinator and type of reward' 
  # pollination2<- subset(traits,traits$TraitName=="Flower pollinator and type of reward")
  # print(unique(pollination2$OrigValueStr))


### dispersal (unassisted,wind, endozoochory, exozoochory, water, ants, ballistic)
  # relevant traits: Dispersal syndrome
dispersal <- subset(traits,traits$TraitName=="Dispersal syndrome")
t<-as.data.frame.matrix(table(dispersal$Latnavn,dispersal$OrigValueStr))
print(names(t))
(t$u <- t$unspecialised + t$unassisted + t$autochor + t$autochory + t$Barochory) #  unassisted
(t$w <- t$anemochory + t$`Anemochory: Small seeds with pappus or very light seeds (ex,  Crepis sp. or Orchis sp.)` + 
        t$`Anemochory: Stems move with the wind, helping for seed dispersion (ex,  Papaver sp.)` + t$`Diaspore is blown by wind` +
        t$chamaechor + t$meteorochor + t$wind + t$Wind) # wind
(t$endo <- t$`Diaspore is eaten intentionally` + t$`Dispersal endozoochorous` + t$dysochor + t$`eaten by birds` + t$endozoochor + 
           t$endozoochory + t$`Endo-zoochory`) # endozoochory
(t$exo <- t$`carried by mammals` + t$deer + t$bighorn + t$`Diaspore is carried accidentally` + t$donkey + t$epizoochor + t$fox + 
          t$goat + t$mouse + t$sheep + t$squirrel + t$`wild boar`) # exozoochory
(t$waterEVA <- t$nautochor + t$ombrochor + t$water) # water
(t$ant <- t$ant) # ants
(t$b <- t$`Explosive mechanism` + t$ballochor) # ballistic
t <- t %>% select(u,w,endo,exo,waterEVA,ant,b)
write.csv(t,'TRY/traitTables/dispersal.csv')
temp<-subset(dispersal,dispersal$OrigValueStr=='nautochor')


### Ellenberg light
light <- subset(traits,traits$TraitName=="Species environmental indicator value according to Ellenberg: light")
light <- light  %>% 
  group_by(Latnavn) %>%
  summarise(mean = mean(as.numeric(OrigValueStr)),
            median = median(as.numeric(OrigValueStr)),
            n = n())
write.csv(light,'TRY/traitTables/light.csv')
### Ellenberg moisture
moisture <- subset(traits,traits$TraitName=="Species environmental indicator value according to Ellenberg: moisture")
moisture <- moisture  %>% 
  group_by(Latnavn) %>%
  summarise(mean = mean(as.numeric(OrigValueStr)),
            median = median(as.numeric(OrigValueStr)),
            n = n())
write.csv(moisture,'TRY/traitTables/moisture.csv')
### Ellenberg nitrogen
nitrogen <- subset(traits,traits$TraitName=="Species environmental indicator value according to Ellenberg: nitrogen")
nitrogen2 <- nitrogen  %>% 
  group_by(Latnavn) %>%
  summarise(mean = mean(as.numeric(OrigValueStr)),
            median = median(as.numeric(OrigValueStr)),
            n = n())
write.csv(nitrogen,'TRY/traitTables/nitrogen.csv')
write.csv(nitrogen2,'TRY/traitTables/nitrogen2.csv')


### SLA (ARKO def: fresh leaf area/leaf dry mass (mm2/mg))
sla <- as.data.frame(subset(traits,traits$TraitName=="Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded"))
sla$OrigValueStr <- as.numeric(sla$OrigValueStr)
t <- sla  %>% 
  group_by(Latnavn) %>%
  summarise(mean = mean(as.numeric(OrigValueStr)),
            median = median(as.numeric(OrigValueStr)),
            n = n())
write.csv(t,'TRY/traitTables/slaMeans')
write.csv(sla,'TRY/traitTables/slaRaw.csv')


### seed.mass
seedMass <- subset(traits,traits$TraitName=="Seed dry mass"|traits$TraitName=="Dispersal unit dry mass")
seedMass <- seedMass %>% 
  group_by(Latnavn) %>%
  summarise(mean = mean(as.numeric(OrigValueStr)),
            median = median(as.numeric(OrigValueStr)),
            n = n())
write.csv(seedMass,'TRY/traitTables/seedMass.csv')

### seed.number
seedNumber <- subset(traits,traits$TraitName=="Seed number per ramet"|traits$TraitName=="Seed number per flower"|traits$TraitName=="Plant biomass and allometry: Seed number per plant"|traits$TraitName=="Seed number per inflorescence (total, fertile, infertile)"|traits$TraitName=="Seed number per stem")
seedNumber2 <- seedNumber %>% 
  group_by(Latnavn) %>%
  summarise(mean = mean(as.numeric(na.omit(OrigValueStr))),
            median = median(as.numeric(na.omit(OrigValueStr))),
            n = n())
write.csv(seedNumber,'TRY/traitTables/seedNumber.csv')
write.csv(seedNumber2,'TRY/traitTables/seedNumber2.csv')


### terminal.velocity
terminalVelocity <- subset(traits,traits$TraitName=="Seed terminal velocity")
terminalVelocity <- terminalVelocity %>% 
  group_by(Latnavn) %>%
  summarise(mean = mean(as.numeric(OrigValueStr)),
            median = median(as.numeric(OrigValueStr)),
            n = n())
write.csv(terminalVelocity,'TRY/traitTables/terminalVelocity.csv')

### vegetation height
height <- subset(traits,traits$TraitName=="Seed releasing height") # includes only one species: Inu.sal
height <- height %>% 
  group_by(Latnavn) %>%
  summarise(mean = mean(as.numeric(OrigValueStr)),
            median = median(as.numeric(OrigValueStr)),
            n = n())
write.csv(height,'TRY/traitTables/height.csv')

### Ellenberg pH: 0 observations
### LDMC (ARKO def: tissue density; dry leaf mass/fresh leaf mass (mg/g) ) - not there
### Leaf.size (ARKO def one-sided projected surface area of a leaf (mm2)) - not there
### leaf.mass - not there
### sl.index - not there
#---------------------------------------------------
