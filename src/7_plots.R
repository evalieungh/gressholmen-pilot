############################
#           Plots          #
############################
#         Script 7         #

library(treemap)
library(RColorBrewer)
library(extrafont)
font_import()
# loadfonts(device="win")
fonts()


# treemaps
#----------------------------------------
df <- read.csv('HabspesPresencePolygonData.csv')
df12 <- df[df$yr == '2012',]
df19 <- df[df$yr == '2019',]
head(df)
table(df$dispSyndr,df$presence)
table(df$dispSyndr)
vector <- c('ballistic','endozoochory','exozoochory','unassisted','wind')
presences <- c('137','129','37','190','137')
potential <- c(210,270,120,450,420)
presrel <- round(100*c(137/210,129/270,37/120,190/450,137/420))  # presences relative to number of species with that dispDyndr ()
df1 <- data.frame(vector,presences,potential, presrel)
treemap(df1, index = 'vector', 
        vSize = 'potential', 
        title = 'Dispersal syndromes', # Commonality of dispersal syndromes among my 49 species
        palette = 'YlGn',
        bg.labels=c("transparent"), 
        border.col="white",             
        border.lwds= 2,
        type = 'index')
treemap(df1, index = 'vector', 
        vSize = 'presrel', 
        title = 'Persistence', # presences of species with different dispersal syndromes relative to the potential number of presences based on the number of species with that dispersal mechanism
        palette = 'YlGn',
        bg.labels=c("transparent"), 
        border.col="white",             
        border.lwds= 2,
        type = 'index')


# Distances between polygons
#----------------------------------------
d <- as.matrix(read.csv('DistanceMatrix.csv'))
d <- d[,-1]
d[d>1000] <- NA # remove distances over 1000 m
diag(d) <- NA
d[lower.tri(d)] <- NA
d <- na.omit(as.vector(d))

min(d)
max(d)
hist(sort(d),breaks = 1685,
     xlab = 'distances between patch centroids (m)')


# output distances from dispeRsal function/script
#------------------------------------------------------------------------------------------
out2 <- read.csv('predictedDDmodel2.csv')
out2$maxdist <- exp(out2$log10MDD)
par(bg=NA)
boxplot(out2$maxdist~out2$DS, 
        main = ' ', # Dispersal distance per vector
        ylab = 'modeled 99th percentile distance (m)',
        xlab = 'Dispersal vector',
        col = 'white', cex = 1.5, cex.axis = 1.5, cex.lab = 1.5,
        lwd = 2, frame.plot = FALSE,
        family = 'Arial')

        
# boxplots of traits per groups
#----------------------------------------

