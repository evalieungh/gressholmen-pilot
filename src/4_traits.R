###############
#   Traits    #
###############
#   Script 4  #

# script by Eva Lieungh

# data. Trait origins explained in the .xlsx file of same name, plus trait documentation from ARKO project
dt  <- read.csv('Data/traits.csv') # columns=traits, rows=species
dpa <- read.csv('Data/data.csv') # columns=presence/absence per polygon, year, rows=species

#