library(here)
library(nimble)
library(MCMCvis)
library(coda)

#select the guild you want to model here 
GUILD <- "exploiters"  #options are "avoiders", "adapters", "exploiters", "community"

start.time <- Sys.time() 

# MCMC settings
ni <- 40000
nt <- 1
nb <- 5000
nc <- 3

#import data 
data1 <- read.csv("data/bird.binary.noCorrRes.csv") 

#set up consecutive points 
data1$new.point <- data1$Point
for(i in 1:nrow(data1)){
  if(data1$Site[i] == "CN"){
    data1$new.point[i] <- data1$Point[i]
  }else if(data1$Site[i] == "IH"){
    data1$new.point[i] <- data1$Point[i] + 8 
  }else if(data1$Site[i] == "MO"){
    data1$new.point[i] <- data1$Point[i] + 16 
  }else if(data1$Site[i] == "RR"){
    data1$new.point[i] <- data1$Point[i] + 24 
  }else if(data1$Site[i] == "SR"){
    data1$new.point[i] <- data1$Point[i] + 32 
  }else if(data1$Site[i] == "TE"){
    data1$new.point[i] <- data1$Point[i] + 40 
  }else if(data1$Site[i] == "UP"){
    data1$new.point[i] <- data1$Point[i] + 45
  }
}

#set up points, site type, years, species 
point <- sort(unique(data1$new.point))
#site type - note that CD sites are 1 and PCD sites are 2
site <- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,5),rep(7,8))
site.type <- rep(2,length(site))
for(i in 1:length(site)){
  if(site[i] == 1){
    site.type[i] <- 1
  }else if (site[i] == 3){
    site.type[i] <- 1
  }else if (site[i] == 6){ 
    site.type[i] <- 1
  }else if (site[i] == 7){
    site.type[i] <- 1
  }}

#years - put in order 
year <- sort(unique(data1$Year))

#species 
species <- c(9:61)

#set up observation data dim = points, years, species, reps 
array2 <- array(NA,dim= c(53,12,53,4))
for(j in 1:length(point)){
  for(y in 1:length(year)){
    for(k in 1:length(species)){
      nums <- intersect(which(data1$new.point == point[j]),which(data1$Year == year[y]))
      if(length(nums)>0){
        array2[j,y,k,1:length(nums)] <- data1[nums,species[k]]
      }
    }
  }
}

#effort array - make effort 0 anytime the data are NA  
array.eff <- array(1,dim=c(53,12,53,4))
array.eff[is.na(array2==TRUE)] <- 0 
#now make the data that are NAs 0s 
array2[is.na(array2==TRUE)] <- 0

#take out species with a small sample size 
array.eff <- array.eff[,,-c(10,12,19,21,29,36,39,44,45,50),]
array2 <- array2[,,-c(10,12,19,21,29,36,39,44,45,50),]
#species names 
spp.names <- colnames(data1)[9:61][-c(10,12,19,21,29,36,39,44,45,50)]
#remaining species count 
species <- dim(array2)[3]

#array.guild
avoiders.ind <- c(10,11,14,18,19,22,25,27,28,29,30,35,36,40,42)
adapters.ind <- c(2,3,6,7,9,12,13,15,16,23,24,26,33,34,37,38,39,41,43)
exploiters.ind <- c(1,4,5,8,17,20,21,31,32)

#set up the data depending on the guild 
array.eff.avoiders <- array.eff[,,avoiders.ind,]
array2.avoiders <- array2[,,avoiders.ind,]
species.avoiders <- dim(array2.avoiders)[3]

array.eff.adapters <- array.eff[,,adapters.ind,]
array2.adapters <- array2[,,adapters.ind,]
species.adapters <- dim(array2.adapters)[3]

array.eff.exploiters <- array.eff[,,exploiters.ind,]
array2.exploiters <- array2[,,exploiters.ind,]
species.exploiters <- dim(array2.exploiters)[3]

if(GUILD == "avoiders"){
  array2 <- array2.avoiders
  array.eff <- array.eff.avoiders
  species <- species.avoiders
}else if(GUILD == "adapters"){
  array2 <- array2.adapters
  array.eff <- array.eff.adapters
  species <- species.adapters
}else if(GUILD == "exploiters"){ 
  array2 <- array2.exploiters
  array.eff <- array.eff.exploiters
  species <- species.exploiters
}else if(GUILD == "community"){ 
  array2 <- array2
  array.eff <- array.eff
  species <- species    
}

#store gelman diagnostics 
R.hat <- rep(NA,species)

##NIMBLE code 
occ1 <- nimbleCode( { 

#Likelihood
for(s in 1:n.points){
  for(y in 1:n.years){
    for(k in 1:n.species){
      # State Process  
      z[s,y,k] ~ dbern(psi[s,y,k])
      # Observation Process  
      for(n in 1:n.reps) {
        array2[s,y,k,n] ~ dbern(z[s,y,k] * p[s,y,k] * array.eff[s,y,k,n])  #state * p * effort (0 or 1)
      }#n replicate
    
      #observation model
      logit(p[s,y,k]) <- int.p + p.rand.site[site[s]] + p.rand.year[y] + p.rand.species[k]
    
      #occupancy model 
      logit(psi[s,y,k]) <- int.psi + w.sitetype*beta.site.type[site.type[s]] + w.year*beta.year*year.norm[y] + w.year2*beta.year2*year2.norm[y] + psi.rand.species[k]
    }#y year
  }#k species  
}#j point 


#Random effects 
for(s in 1:n.sites){
  p.rand.site[s] ~ dnorm(0,sd = sd.p.site)
}
for(y in 1:n.years){
  p.rand.year[y] ~ dnorm(0,sd = sd.p.year)
}
for(k in 1:n.species){
  p.rand.species[k] ~ dnorm(0,sd = sd.p.species)
  psi.rand.species[k] ~ dnorm(0,sd = sd.psi.species)
}
    
#Priors 
int.p ~ dnorm(0,sd=1)
sd.p.site ~ dunif(0,1)
sd.p.year ~ dunif(0,1)
sd.p.species ~ dunif(0,1)

w.sitetype ~ dbern(1/2)
w.year ~ dbern(2/3)
w.yearsq ~ dbern(1/2)
w.year2 <- w.year*w.yearsq

int.psi ~ dnorm(0,sd = sd.param)
beta.site.type[1] <- 0
beta.site.type[2] ~ dnorm(0,sd = sd.param)
beta.year ~ dnorm(0,sd = sd.param)
beta.year2 ~ dnorm(0,sd = sd.param)
sd.psi.species ~ dunif(0,1)

tau.total ~ dgamma(shape = 3.289, rate = 7.8014)
var.total <- 1/tau.total 
K <- 1 + w.sitetype + w.year + w.year2
var.param <- var.total/K
sd.param <- pow(var.param,0.5)


})


######################################################################
#                                                                    #  
#                      Data and Constants                            #
#                                                                    #    
######################################################################

# Bundle data
data <- list(array2=array2)

#get constants 
n.points <- dim(array2)[1]
n.years <- dim(array2)[2]
n.species <- dim(array2)[3]
n.reps <- dim(array2)[4]
n.sites <- length(unique(site))
year.norm <- (c(1:12) - mean(c(1:12)))/sd(c(1:12))
year2.norm <- pow(year.norm,2)

constants <- list(array.eff=array.eff,site.type=site.type,site=site,year.norm=year.norm,year2.norm=year2.norm,n.sites=n.sites,n.points=n.points,n.years=n.years,n.species=n.species,n.reps=n.reps)

######################################################################
#                                                                    #  
#                             Inits                                  #
#                                                                    #    
######################################################################

z.init <- array(0,dim = c(n.points,n.years,n.species))
for(s in 1:n.points){
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
params <- c("int.p","sd.p.site","sd.p.year","sd.p.species","int.psi","beta.site.type","beta.year","beta.year2","sd.psi.species","w.sitetype","w.year","w.year2") 

Rmodel1 <- nimbleModel(code = occ1, constants = constants, data = data,
                       check = FALSE, calculate = FALSE, inits = inits)
conf1 <- configureMCMC(Rmodel1, monitors = params, thin = nt, useConjugacy = FALSE)
Rmcmc1 <- buildMCMC(conf1)
Cmodel1 <- compileNimble(Rmodel1, showCompilerOutput = FALSE)
Cmcmc1 <- compileNimble(Rmcmc1, project = Rmodel1)

## Run MCMC ####
out <- runMCMC(Cmcmc1, niter = ni, nburnin = nb , nchains = nc, inits = inits,
               setSeed = FALSE, progressBar = TRUE, samplesAsCodaMCMC = TRUE)

out.all <- rbind(out$chain1,out$chain2,out$chain3)

R.hat <- gelman.diag(out[,c(2,3,4,5,6,7,8)],multivariate=TRUE)$mpsrf

write.csv(out.all,paste("results/occ_run4.",GUILD, ".csv",sep=""))

(elapsed <- Sys.time() - start.time)

#print R.hat 
R.hat 







