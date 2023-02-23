#############################
# Estimating parameters for
# model p* = 1 - e/c      
#############################

# Code by Eva Lieungh

# p* = 
# e = extinction rate
# c = colonisation rate

d <- read.csv('Data/data.csv')

# estimating e parameter
alld<-colSums(d['yr'=='2012',3:17])

me <- glm()

































####################    IKKE FERDIG
##  JAGS stuff    ##    henta fra C:\Users\evaler\OneDrive - Universitetet i Oslo\Eva\PHD\Kurs\LygraBayes\2-Lectures\1.2-JAGS_Introduction\1b_Urn_Experiment_JAGS.r og ...\2.3-Generalized_Linear_Models\3eLizard_Abundance_Occ
####################
library(rjags)
library(DHARMa)

# 1) Save a description of the model in JAGS syntax to working directory
sink("Connectivitymodel.txt")
cat(
  "model{
   for(i in 1:n.dat){
  # Model level 1: Explaining a presence p(Presence|??)
    Presence[i] ~ Already[i] * Dispersed[i]     # Presence expresses some form of probability of occurrence in polygon i

    # Model level 2: Already= species already present in polygon i, living of from seed bank
      Already[i] ~ dbin(survival[i],1)

    # Model level 2: Dispersed= individuals arrived in polygon i from elsewhere
      Dispersed[i] <- Source[i] * Relocation[i] * Destination[i]      # 
  
      # Model level 3: Source,  simple for now (does it work to just specify a number?), representing seed production and release at source
        Source[i] ~ 1

      # Model level 3: Relocation, based on Connectivity (from ARKO papers), representing probability that a wind-dispersed species traveled the distance?
        Relocation[i] <- 
        
          # Model level 4: WALD wind dispersal
            wald <- function(x) {
                                  x = ((lambda/(2*pi*x^3))^0.5)*exp(-((lambda*(x-u)^2)/(2*u^2*x)))
                                  return(x)
                                }

      # Model level 3: Destination, representing germination in polygon i
        Destination[i] ~ 
  }
  
  # Prior distributions
  survival[i] ~ dbin(1,1)  # 
  }
  ",fill = TRUE)

sink()

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

####################################################################
