############################
#   Trait-connectivity     #
############################
#         Script 5         #
#         by Eva L         #

library(tidyverse)

# Make some basic models to explore the relationship between traits, presences and connectivity
#---------------------------------------------------------------------------------------------------------------------------
df <- read.csv('HabspesPresencePolygonData.csv')
names(df) # NB! general models below include multiple testing and pseudoreplicates (species P/A both years in same locations)

# make some subsets to analyse avoiding pseudoreplicates
df12 <- df[df$yr == '2012',]
df19 <- df[df$yr == '2019',]

# null model for presence - similar intercept around -0.3
m0 <- glm(df$presence ~ +1, family = 'binomial')
summary(m0)
m012 <- glm(df12$presence ~ +1, family = 'binomial')
summary(m012)
m019 <- glm(df19$presence ~ +1, family = 'binomial')
summary(m019)

# presence by connectivity and area
mS <- glm(df$presence ~ df$S, family = 'binomial')
summary(mS) # nope
mA <- glm(df$presence ~ df$area, family = 'binomial')
summary(mA) # almost a weak positive signal

# presence by traits
## log-transforming continuous variables for normality
m1 <- glm(df12$presence ~ df12$dispSyndr, family = 'binomial')
summary(m1) # weak signals of some dispersal modes! 
plot(m1)
table(df12$presence,df12$dispSyndr) # ballists seem to be present more often

m2 <- glm(df$presence ~ log(df$canopy.height), family = 'binomial') # weak positive signal - taller plants do better? Maybe should remove the trees and/or shrubs and try again in case they affect it a lot
summary(m2)
m212 <- glm(df12$presence ~ log(df12$canopy.height), family = 'binomial') # no signal
summary(m212)

m3 <- glm(df$presence ~ log(df$SLA), family = 'binomial') # weak positive signal ?
summary(m3)
m312 <- glm(df12$presence ~ log(df12$SLA), family = 'binomial') # almost weak positive signal ?
summary(m312)
m319 <- glm(df19$presence ~ log(df19$SLA), family = 'binomial') # weak positive signal ?
summary(m319)

m4 <- glm(df$presence ~ log(df$seed.mass), family = 'binomial') # weak positive signal
summary(m4)
m412 <- glm(df12$presence ~ log(df12$seed.mass), family = 'binomial') # almost weak positive signal
summary(m412)
m419 <- glm(df19$presence ~ log(df19$seed.mass), family = 'binomial') # weak positive signal
summary(m419)

m5 <- glm(df$presence ~ log(df$seed.number), family = 'binomial') # almost a weak positive signal
summary(m5)

m6 <- glm(df$presence ~ log(df$terminal.velocity), family = 'binomial') # weak positive signal
summary(m6)
m612 <- glm(df12$presence ~ log(df12$terminal.velocity), family = 'binomial') # weak positive signal
summary(m612)
m619 <- glm(df19$presence ~ log(df19$terminal.velocity), family = 'binomial') # weak positive signal
summary(m619)

m7 <- glm(df$presence ~ df$growthForm, family = 'binomial') # 'herb' and 'woody' have weak positive signals!
summary(m7)
m712 <- glm(df12$presence ~ df12$growthForm, family = 'binomial') # same
summary(m712)
m719 <- glm(df19$presence ~ df19$growthForm, family = 'binomial') # same-ish but weaker for woody
summary(m719)

# presence by polygon attributes
m8 <- glm(df$presence ~ df$T1p, family = 'binomial')
summary(m8)
m9 <- glm(df$presence ~ df$T2p, family = 'binomial') # weak positive signal
summary(m9)
m10 <- glm(df$presence ~ df$T32p, family = 'binomial') # weak positive signal
summary(m10)
m11 <- glm(df$presence ~ df$tree, family = 'binomial')
summary(m11)
m12 <- glm(df$presence ~ df$shrub, family = 'binomial') # almost a weak negative signal
summary(m12)
m13 <- glm(df$presence ~ df$RA.SJ, family = 'binomial') # weak signal
summary(m13)
m14 <- glm(df$presence ~ df$SE, family = 'binomial') # weak signal
summary(m14)

# full model after Tamme et al.
mFwind <- glm(df$presence ~ df$growthForm + df$dispSyndr + log(df$terminal.velocity), family = 'binomial') # NB not same growth form or disp syndrome categories as Tamme et al.
summary(mFwind) # 
mFall <- glm(df$presence ~ df$growthForm + df$dispSyndr, family = 'binomial')
summary(mFall) #
anova(m0,mFall, test = 'F') # better than chance! 


# connections between turnover and traits? 
#---------------------------------------------------------------------------------------------------------------------------
# NB! Too many degrees of freedom because species have 15 rows (at least?) each. Somehow summarise by species and try again? 
# read the data (from 1_tidythedata script)
df <- read.csv('HabspesTraits_clean_2019turnover.csv')
names(df)

# model turnover by traits
m0 <- glm(df$turnover ~ +1) # turnover column can have three values: +1, -1 and 0. Sort of a count, but Poisson must be positive and I'm not sure it makes sense to transform 0's to 1's to get it on a 0-1-2-scale.
summary(m0) # intercept -0.022

m1 <- glm(df$turnover ~ df$dispSyndr) # weak negative ?
summary(m1)
# dispsynd canopy height sla, seed mass, number, terminal velocity, growth form
m2 <- glm(df$turnover ~ log10(df$canopy.height)) # weak positive - taller plants do better (influenced by longevity of my woody species, perhaps?)
dfNoWoody <- subset(df,df$growthForm != 'woody')
m2 <- glm(dfNoWoody$turnover ~ log10(dfNoWoody$canopy.height)) # stronger positive signal
summary(m2)

m3 <- glm(df$turnover ~ df$SLA) # should SLA be log-transformed? Maybe not, I might expect it to be normally distributed to begin with
summary(m3) # weak negative signal

m4 <- glm(df$turnover ~ log10(df$seed.mass)) # weak positive
summary(m4)

m5 <- glm(df$turnover ~ log10(df$seed.number)) # weak positive
summary(m5)

m6 <- glm(df$turnover ~ log10(df$terminal.velocity)) # not sign.
summary(m6)

m7 <- glm(df$turnover ~ df$growthForm) # herb and woody weak positive
summary(m7)



# Is it possible to predict 2019-presences from 2012-presences combined with traits? 
#---------------------------------------------------------------------------------------------------------------------------






# Calculate and analyse connectivity for the different species-traits-groups
#---------------------------------------------------------------------------------------------------------------------------
## (gammel) Plan: re-calculate connectivity of the Gressholmen polygons with a WALD dispersal kernel for the wind dispersed species!
## wind WALD
#x<- as.numeric(c(1:1000))    # x represents a vector of distances in meters. I'm making it roughly the same length (1000 m) as the greatest distance of coastline points on opposite sides of the island
#h<- (sum(0.125,0.35, 0.17, 0.605, 0.51))/5; h     # average seed release height for the 5 species
#tv<- 1.074      # average seed terminal velocity for the 5 species
#u<- (h*v)/tv    # 0.6555
#lambda <- (h/sigma)^2
## alpha <- integrate(wald.b, 0, 1000) # =1
## i is the polygon I'm calculating the connectivity(S) for. The distance d[ij] is between polygon i and another polygon j. 



library(rjags)
library(DHARMa)
# one possibility: Hierarchical, bayesian? model of Connectivity
# do we need different models for the groups, or can we make a general one that works regardless of dispersal mechanism?

## First level in hierarchy: Connectivity
S <- colSums(exp( -alpha * d)*A, na.rm = TRUE)

## Second level in hierarchy
#alpha <- #some term describing dispersal distance with traits! 
  
  
# Copy and modify JAGS model code from #LygraBayes course
#--------------------------------------------------------------------------------------------------------------------------
# Model specification
sink(ConnectivityModel.txt) # makes model specification file in working dir
cat("
  model{
  # Connectivity model
  for(i in 1:n.dat){
  # Model level 1: Explaining a presence p(Presence|connectivity)
  
    Presence[i] ~ S

  # Alpha model
  alpha <- function(x) {
  x = ((lambda/(2*pi*x^3))^0.5)*exp(-((lambda*(x-u)^2)/(2*u^2*x)))
  return(x)
  }  

}")
sink()



# code not changed below this line! 

# 2) Set up a list that contains all the necessary data.
# (here including parameters for the prior distribution)
Data = list(N = N, Y = Y, a = a0, b = b0) # ikke endret

# 3) Specify a function to generate inital values for the parameters
inits.fn <- function() list(p = rbeta(1,1,1)) # ikke endret

# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel <- jags.model(file= "Urn_model.txt", data=Data, 
                        init = inits.fn, n.chains = 3, 
                        n.adapt= 1000)
# Specify parameters for which posterior samples are saved
para.names <- c("p")
# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel, variable.names = para.names, 
                        n.iter = 5000)

# The output of coda.samples() is a mcmc.list object:
str(Samples)

# Plot the mcmc chain and the posterior sample for p
plot(Samples)

# Statistical summaries of the posterior sample for p
summary(Samples)

# For further analyses it is often useful to
# transform mcmc.list object to a matrix
Pars.Mat <- as.matrix(Samples)
hist(Pars.Mat[,'p'], freq = FALSE, col = 'blue', breaks = 30)
# Typical statistics we want to calculate for the
# posterior sample are the median and the 
# limits of the 95% confidence interval
p.med <- median(Pars.Mat[,'p'])
abline(v = p.med, lty = 2, lwd = 2)
CI <- quantile(Pars.Mat[,'p'], prob = c(0.025,0.975))
abline(v = CI, lty = 2)

# Compare to Analytical solution: pi ~ Beta(a+y, b + N-y)
p.vec <- seq(0,1,length.out=100)
lines(p.vec,Post.fn(p.vec,Y,N,a0,b0),col='red')


