# MetaLandSim

# Code and text snippets copied from: https://www.r-bloggers.com/2019/04/simulating-metapopulation-occupation-in-a-landscape/
# MetaLandSim’s main objectives are to 
# i) simulate the occupation of a habitat network suffering some sort of change (but static landscapes work too); 
# ii) simulate range expansion by a species with a metapopulation-like spatial strategy.
# MetaLandSim simulates the stochastic occupancy of the landscape by a given species using Stochastic Patch Occupancy Models, like the Incidence Function Model, developed by Hanski (1994).

# see also https://esajournals.onlinelibrary.wiley.com/doi/10.1890/09-2402.1 
# https://www.sciencedirect.com/science/article/abs/pii/S1364815216300718 

library(MetaLandSim)

# create a landscape of patches (different every time even with set.seed())
rl1 <- rland.graph(mapsize=1000, # landscape size 1000*1000 m
									 dist_m=20, # minimum distance between patches
									 areaM=0.05, # mean area, in hectares
									 areaSD=0.02, # standard deviation of the patch areas
									 Npatch=15, # number of patches
									 disp=300, # mean dispersal ability of the species -- as a threshold to aggregate habitat patches
									 plotG=TRUE)

# occupy the landscape with a species
sp1 <- species.graph(rl=rl1, 
										 method="number", # click - individually select the patches with occurrence of the species by clicking on the map. Use only for individual landscape simulations. percentage - percentage of the patches to be occupied by the species. number - number of patches to be occupied by the species
										 parm=7, # Parameter to specify the species occurrence - either percentage of occupied patches or number of occupied patches, depending on the method chosen.
										 nsew="none", # directional point of entry of the species in the landscape
										 plotG=TRUE) # occupied patches in green

# create a list containing this landscape through 100 time steps
span1 <- span.graph(rl=rl1, span=100, # number of time steps
										par1="none", # Parameter determining the dynamism type (various habitat destruction and stochasticity to choose from!)
										par2=NULL, par3=NULL, par4=NULL, par5=NULL)

# define the species parameters, defining its metapopulational dynamics. (NOT THE RECOMMENDED APPROACH - read paper for better alternative)
param1 <- create.parameter.df(alpha=0.0045, # relating extinction with distance
															x=0.5, # scaling extinction risk with patch area
															y=2, # colonization probability
															e=0.04) # extinction probability

# simulate the species occupation (considering the parameters just defined) through the time steps
sim1 <- simulate_graph(rl=sp1,
											 rlist=span1,
											 simulate.start=FALSE,
											 method=NULL,
											 parm=NULL,
											 nsew="none",
											 succ = "none",
											 param_df=param1,
											 kern="op1",
											 conn="op1",
											 colnz="op1",
											 ext="op1",
											 beta1=NULL,
											 b=1,
											 c1=NULL,
											 c2=NULL,
											 z=NULL,
											 R=NULL
)

# plot a selection of the time steps
par(mfrow=c(2,2))
plotL.graph(rl=sp1, rlist=sim1, #time step 1
						nr=1, species=TRUE, links=FALSE)
plotL.graph(rl=sp1, rlist=sim1, #time step 33
						nr=33, species=TRUE, links=FALSE)
plotL.graph(rl=sp1, rlist=sim1, #time step 66
						nr=66, species=TRUE, links=FALSE)
plotL.graph(rl=sp1, rlist=sim1, #time step 100
						nr=100, species=TRUE, links=FALSE)



# MetaLandSim’s simulations should be repeated many times (to allow the results to stabilize). This one-run simulation was an example (for more on this, check the iterate.graph function).