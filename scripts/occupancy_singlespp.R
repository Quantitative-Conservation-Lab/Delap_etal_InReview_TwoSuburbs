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

Sppnames <- colnames(data1)[9:61]

guilds <- c("exploiter","adapter","adapter","exploiter","exploiter","adapter","adapter","exploiter","adapter","adapter","avoider","adapter",
          "avoider","adapter","adapter","avoider","adapter","adapter","avoider","exploiter","adapter","avoider","avoider","exploiter", 
          "exploiter","avoider","adapter","adapter","adapter","avoider","adapter","avoider","avoider","avoider","avoider","avoider","exploiter",
          "exploiter","adapter","adapter","adapter","avoider","avoider","avoider","adapter","adapter","adapter","adapter","avoider", 
          "adapter","adapter","avoider","adapter")
            

guilds.num <- guilds 
guilds.num[which(guilds == "avoider")] <- 1
guilds.num[which(guilds == "adapter")] <- 2
guilds.num[which(guilds == "exploiter")] <- 3
guilds.num <- as.numeric(guilds.num)
        
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


#now pull out data for single species analysis

spp.i <- array2[,,i,]
eff.i <- array.eff[,,i,]


##NIMBLE code 
occ1 <- nimbleCode( { 

#Likelihood
for(s in 1:n.sites) {
  for(y in 1:n.years){
    # State Process  
      z[s,y] ~ dbern(psi[s,y])
      # Observation Process  
      for(n in 1:n.reps) {
        obs[s,y,n] ~ dbern(z[s,y] * p[s,y] * array.eff[s,y,n])
      }#n replicate
  }#y year
}#j site 


#model on occupancy - all additional years - autoregressive term 
#model on detection
for(j in 1:n.sites){
  for(y in 1:n.years){

    logit(p[j,y,k]) <- int.p + p.ran.site[j] + p.ran.site[y]
      
    logit(psi[j,y,k]) <- int.psi
  }
}   
#Random effects 

#Priors 
for(k in 1:3){
  int.p[k] ~ dnorm(0,sd=1)
  int.psi[k] ~ dnorm(0,sd=1)
  sd.psi.species[k] ~ dgamma(0.1,0.1)
}
#sd.psi.autoreg ~ dgamma(0.1,0.1)
#sd.p.sitesyears ~ dgamma(0.1,0.1)

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


n.sites <- dim(array2)[1]
n.years <- dim(array2)[2]
n.species <- dim(array2)[3]
n.reps <- dim(array2)[4]

constants <- list(array.eff=array.eff,guilds.num=guilds.num,n.sites=n.sites,n.years=n.years,n.species=n.species,n.reps=n.reps)

######################################################################
#                                                                    #  
#                             Inits                                  #
#                                                                    #    
######################################################################

z.init <- array(0,dim=c(n.sites,n.years,n.species))
for(s in 1:n.sites){
  for(y in 1:n.years){
    for(k in 1:n.species){
      if(any(array2[s,y,k,]==1)){
        z.init[s,y,k] <- 1 
      }
    }
  }
}
inits <- list(z=z.init)

######################################################################
#                                                                    #  
#                         Run the model                              #
#                                                                    #    
######################################################################

# Parameters monitored
params <- c("int.p","int.psi")

# MCMC settings
ni <- 2500
nt <- 1
nb <- 1000
nc <- 3

Rmodel1 <- nimbleModel(code = occ1, constants = constants, data = data,
                       check = FALSE, calculate = FALSE, inits = inits)
conf1 <- configureMCMC(Rmodel1, monitors = params, thin = nt, useConjugacy = FALSE)
Rmcmc1 <- buildMCMC(conf1)
Cmodel1 <- compileNimble(Rmodel1, showCompilerOutput = FALSE)
Cmcmc1 <- compileNimble(Rmcmc1, project = Rmodel1)

## Run MCMC ####
out <- runMCMC(Cmcmc1, niter = ni, nburnin = nb , nchains = nc, inits = inits,
               setSeed = FALSE, progressBar = TRUE, samplesAsCodaMCMC = TRUE)


#check diagnostics
#MCMCtrace(object = samples.occ1$samples,
#          pdf = FALSE,
#          ind = TRUE)

#summary 
#samples.occ1$summary$all.chains 

#Gelman-Rubin diagnostic
#gelman.diag(samples.occ1$samples,multivariate=FALSE)






