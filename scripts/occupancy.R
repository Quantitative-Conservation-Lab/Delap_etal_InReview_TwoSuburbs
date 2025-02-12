library(here)
library(nimble)
library(MCMCvis)
library(coda)

#run the early parts of analysis.R to get bird.binary.noCorrRes 
data1 <- bird.binary.noCorrRes 

#set up sites, years, species 
Sites <- unique(data1$Site)
Years <- unique(data1$Year)
Species <- c(9:61) 

#dim = years, sites, species, reps 
array2 <- array(NA,dim= c(7,12,53,32))
for(j in 1:length(Sites)){
  for(y in 1:length(Years)){
    for(k in 1:length(Species)){
      nums <- intersect(which(data1$Site == Sites[j]),which(data1$Year == Years[y]))
      if(length(nums)>0){
        array2[j,y,k,1:length(nums)] <- data1[nums,Species[k]]
      }
    }
  }
}
#effort array 
array.eff <- array(1,dim= c(7,12,53,32))
array.eff[is.na(array2==TRUE)] <- 0 

array2[is.na(array2==TRUE)] <- 0

##NIMBLE code 
occ1 <- nimbleCode( { 

#Likelihood
for(j in 1:n.sites) {
  for(y in 1:n.years){
    for(k in 1:n.species){
    # State Process  
      z[j,y,k] ~ dbern(psi[j,y,k])
      # Observation Process  
      for(n in 1:n.reps) {
        obs[j,y,k,n] ~ dbern(z[j,y,k] * p[j,y,k] * array.eff[j,y,k,n])
      }#n replicate
    }#k species
  }#y year
}#j site 

#model on detection 
for(j in 1:n.sites) {
  for(y in 1:n.years){
    for(k in 1:n.species){
      logit(p[j,y,k]) <- mu.p #p.ran.sitesyears[j,y] + p.ran.species[k]
    }
  }
}
#for(j in 1:n.sites){
#  for(y in 1:n.years){  
#    p.ran.sitesyears[j,y] ~ dnorm(0,sd=sd.p.sitesyears)
#  }
#}  
#for(k in 1:n.species){  
#  p.ran.species[k] ~ dnorm(0,sd = sd.p.species)
#}

#model on occupancy 
#first year - no autoregressive term
#for(j in 1:n.sites){
#  for(k in 1:n.species){
#    logit(psi[j,1,k]) <- int.psi # + psi.ran.species[k]
#  }
#}
#all additional years - with autoregressive term 
for(j in 1:n.sites){
  for(y in 1:n.years){
    for(k in 1:n.species){
#      m[j,y,k] <- getFocalOcc(z[j,y-1,k])
      
      logit(psi[j,y,k]) <- int.psi #+ psi.ran.species[k]   # + m[j,y,k]*autoreg.psi[k]
    }
  }
}   
#for(k in 1:n.species){
#  psi.ran.species[k] ~ dnorm(0,sd = sd.psi.species)
 # autoreg.psi[k] ~ dnorm(0,sd = sd.psi.autoreg)
#}
  
#Priors 
mu.p ~ dnorm(0,sd=1)
#sd.p.sitesyears ~ dgamma(0.1,0.1)
#sd.p.species ~ dgamma(0.1,0.1)

int.psi ~ dnorm(0,sd=1)
#sd.psi.species ~ dgamma(0.1,0.1)
#sd.psi.autoreg ~ dgamma(0.1,0.1)

})

#nimble function 
getFocalOcc<-nimbleFunction(
  run=function(z=double()){
    if(z==1) return(1)
    else return(0)
    returnType(integer())
  })


# Bundle data
data <- list(obs=array2)

constants <- list(array.eff=array.eff,n.sites = dim(array2)[1],n.years = dim(array2)[2],n.species=dim(array2)[3],n.reps=dim(array2)[4])

######################################################################
#                                                                    #  
#                             Inits                                  #
#                                                                    #    
######################################################################

inits <- function(){list()}  

######################################################################
#                                                                    #  
#                         Run the model                              #
#                                                                    #    
######################################################################

# Parameters monitored
params <- c("p","psi")

# MCMC settings
ni <- 25000
nt <- 1
nb <- 10000
nc <- 3

## run model 
samples.occ1 <- nimbleMCMC(
  code = occ1,  
  data = data,
  constants = constants, 
  inits = inits,
  monitors = params,
  niter = ni,
  nburnin = nb,
  nchains = nc,
  thin = nt,
  summary = TRUE,
  samplesAsCodaMCMC = TRUE)

#check diagnostics
MCMCtrace(object = samples.occ1$samples,
          pdf = FALSE,
          ind = TRUE)

#summary 
samples.occ1$summary$all.chains 

#Gelman-Rubin diagnostic
gelman.diag(samples.occ1$samples,multivariate=FALSE)






