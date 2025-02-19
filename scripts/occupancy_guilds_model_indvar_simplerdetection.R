library(here)
library(nimble)
library(MCMCvis)
library(coda)

GUILD <- "adapters"  #options are "avoiders", "adapters", "exploiters"
 
start.time <- Sys.time() 

# MCMC settings
ni <- 80000
nt <- 1
nb <- 10000
nc <- 3

#import data 
data1 <- read.csv("data/bird.binary.noCorrRes.csv") 

#set up sites, site type, years, species 
#sites - put in alphabetical order 
sites <- sort(unique(data1$Site))
#site type - note that CD sites are 1 and PCD sites are 2
site.type <- sites
for(s in 1:length(sites)){
  site.type[s] <- data1$Site.Type[max(which(data1$Site == sites[s]))]
}
site.type <- as.numeric(as.factor(site.type)) 
#years - put in order 
years <- sort(unique(data1$Year))
species <- c(9:61)

#set up observation data dim = sites, years, species, reps 
array2 <- array(NA,dim= c(7,12,53,32))
for(j in 1:length(sites)){
  for(y in 1:length(years)){
    for(k in 1:length(species)){
      nums <- intersect(which(data1$Site == sites[j]),which(data1$Year == years[y]))
      if(length(nums)>0){
        array2[j,y,k,1:length(nums)] <- data1[nums,species[k]]
      }
    }
  }
}

#effort array - make effort 0 anytime the data are NA  
array.eff <- array(1,dim=c(7,12,53,32))
array.eff[is.na(array2==TRUE)] <- 0 
#now make the data that are NAs 0s 
array2[is.na(array2==TRUE)] <- 0

#take out species with a small sample size 
array.eff <- array.eff[,,-c(10,12,19,21,29,36,39,44,45,50),]
array2 <- array2[,,-c(10,12,19,21,29,36,39,44,45,50),]

#species names 
spp.names <- colnames(data1)[9:61][-c(10,12,19,21,29,36,39,44,45,50)]

#array.guild
avoiders.ind <- c(10,11,14,18,19,22,25,27,28,29,30,35,36,40,42)
adapters.ind <- c(2,3,6,7,9,12,13,15,16,23,24,26,33,34,37,38,39,41,43)
exploiters.ind <- c(1,4,5,8,17,20,21,31,32)

array.eff.avoiders <- array.eff[,,avoiders.ind,]
array2.avoiders <- array2[,,avoiders.ind,]

array.eff.adapters <- array.eff[,,adapters.ind,]
array2.adapters <- array2[,,adapters.ind,]

array.eff.exploiters <- array.eff[,,exploiters.ind,]
array2.exploiters <- array2[,,exploiters.ind,]

species.avoiders <- dim(array2.avoiders)[3]
species.adapters <- dim(array2.adapters)[3]
species.exploiters <- dim(array2.exploiters)[3]

if(GUILD == "avoiders"){
  array2 <- array2.avoiders
  array.eff <- array2.avoiders
  species <- species.avoiders
}else if(GUILD == "adapters"){
  array2 <- array2.adapters
  array.eff <- array2.adapters
  species <- species.adapters
}else if(GUILD == "exploiters"){ 
  array2 <- array2.exploiters
  array.eff <- array2.exploiters
  species <- species.exploiters
}    
    
#store gelman diagnostics 
R.hat <- rep(NA,species)

##NIMBLE code 
occ1 <- nimbleCode( { 

#Likelihood
for(s in 1:n.sites) {
  for(y in 1:n.years){
    for(k in 1:n.species){
      # State Process  
        z[s,y,k] ~ dbern(psi[s,y,k])
        # Observation Process  
        for(n in 1:n.reps) {
          array2[s,y,k,n] ~ dbern(z[s,y,k] * p[s,y,k] * array.eff[s,y,k,n])  #state * p * effort (0 or 1)
        }#n replicate
      #model on detection
      logit(p[s,y,k]) <- int.p + p.site.type[site.type[s]] + p.ran.year[y] + p.ran.species[k]
      #model on occupancy 
      logit(psi[s,y,k]) <- int.psi + w.sitetype*beta.site.type[site.type[s]] + w.year*beta.year*year.norm[y] + w.year2*beta.year2*year2.norm[y] + psi.ran.species[k]
    }#k species
  }#y year
}#j site 


#Random effects 
for(y in 1:n.years){
  p.ran.year[y] ~ dnorm(0,var=var.p.year)
}
for(k in 1:n.species){
  p.ran.species[k] ~ dnorm(0,var=var.p.species)
  psi.ran.species[k] ~ dnorm(0,var=var.psi.species)
}
    
#Priors 
int.p ~ dnorm(0,sd=1)
p.site.type[1] ~ dnorm(0,sd=1)
p.site.type[2] <- -1*p.site.type[1]
var.p.year ~ dgamma(1,1)
var.p.species ~ dgamma(1,1)

w.sitetype ~ dbern(1/2)
w.year ~ dbern(2/3)
w.yearsq ~ dbern(1/2)
w.year2 <- w.year*w.yearsq

int.psi ~ dnorm(0,var.param)
beta.site.type[1] <- 0
beta.site.type[2] ~ dnorm(0,var.param)
beta.year ~ dnorm(0,var.param)
beta.year2 ~ dnorm(0,var.param)
var.psi.species ~ dgamma(1,1)

var.total ~ dgamma(3.289,7.8014)
K <- 1 + w.sitetype + w.year + w.year2
var.param <- var.total/K

})


######################################################################
#                                                                    #  
#                      Data and Constants                            #
#                                                                    #    
######################################################################

# Bundle data
data <- list(array2=array2)

#get constants 
n.sites <- dim(array2)[1]
n.years <- dim(array2)[2]
n.species <- dim(array2)[3]
n.reps <- dim(array2)[4]
year.norm <- (c(1:12) - mean(c(1:12)))/sd(c(1:12))
year2.norm <- pow(year.norm,2)

constants <- list(array.eff=array.eff,site.type=site.type,year.norm=year.norm,year2.norm=year2.norm,n.sites=n.sites,n.years=n.years,n.species=n.species,n.reps=n.reps)

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
params <- c("int.p","p.site.type","var.p.year","var.p.species","int.psi","beta.site.type","beta.year","beta.year2","var.psi.species","w.sitetype","w.year","w.year2") 

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

R.hat <- gelman.diag(out[,c(2,3,4,5,6,7,9,10,11,12)],multivariate=TRUE)$mpsrf

write.csv(out.all,paste("results/occ_run2.",GUILD,".csv",sep=""))


R.hat 









