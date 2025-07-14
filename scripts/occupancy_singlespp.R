library(here)
library(nimble)
library(MCMCvis)
library(coda)

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

#species counts 
species <- c(9:61)

#set up observation data dim = sites, years, species, reps 
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

#final species count for loop 
species <- dim(array2)[3]
#store gelman diagnostics 
R.hat <- rep(NA,species)

#loop through species 
for(i in 1:species){

#pull out data for single species analysis
spp.i <- array2[,,i,]
eff.i <- array.eff[,,i,]

##NIMBLE code 
occ1 <- nimbleCode( { 

#Likelihood
for(s in 1:n.points) {
  for(y in 1:n.years){
    # State Process  
      z[s,y] ~ dbern(psi[s,y])
      # Observation Process  
      for(n in 1:n.reps) {
        spp.i[s,y,n] ~ dbern(z[s,y] * p[s,y] * eff.i[s,y,n])  #state * p * effort (0 or 1)
      }#n replicate
    
    #observation model
    logit(p[s,y]) <- int.p + p.rand.site[site[s]] + p.rand.year[y]
    
    #occupancy model 
    logit(psi[s,y]) <- int.psi + w.sitetype*beta.site.type[site.type[s]] + w.year*beta.year*year.norm[y] + w.year2*beta.year2*year2.norm[y] 
  }#y year
}#j site 


#Random effects 
for(s in 1:n.sites){
  p.rand.site[s] ~ dnorm(0,sd = sd.p.site)
}
for(y in 1:n.years){
  p.rand.year[y] ~ dnorm(0,sd = sd.p.year)
}
  
#Priors 
int.p ~ dnorm(0,sd=1)
sd.p.site ~ dunif(0,1)
sd.p.year ~ dunif(0,1)

w.sitetype ~ dbern(1/2)
w.year ~ dbern(2/3)
w.yearsq ~ dbern(1/2)
w.year2 <- w.year*w.yearsq

int.psi ~ dnorm(0,var = sd = sd.param)
beta.site.type[1] <- 0
beta.site.type[2] ~ dnorm(0,sd = sd.param)
beta.year ~ dnorm(0,sd = sd.param)
beta.year2 ~ dnorm(0,sd = sd.param)

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
data <- list(spp.i=spp.i)

#get constants 
n.points <- dim(spp.i)[1]
n.years <- dim(spp.i)[2]
n.reps <- dim(spp.i)[3]
n.sites <- length(unique(site))
year.norm <- (c(1:12) - mean(c(1:12)))/sd(c(1:12))
year2.norm <- pow(year.norm,2)

constants <- list(eff.i=eff.i,site.type=site.type,site=site,year.norm=year.norm,year2.norm=year2.norm,n.sites=n.sites,n.points=n.points,n.years=n.years,n.reps=n.reps)

######################################################################
#                                                                    #  
#                             Inits                                  #
#                                                                    #    
######################################################################

z.init <- matrix(0,nrow=n.points,ncol=n.years)
for(s in 1:n.points){
  for(y in 1:n.years){
    if(any(spp.i[s,y,]==1)){
      z.init[s,y] <- 1 
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
params <- c("int.p","sd.p.site","sd.p.year","int.psi","beta.site.type","beta.year","beta.year2","w.sitetype","w.year","w.year2") 

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

R.hat[i] <- gelman.diag(out[,c(2,3,4,5,6,7,8)],multivariate=TRUE)$mpsrf

write.csv(out.all,paste("results/occ_run4.",spp.names[i], ".csv",sep=""))

print(i)

}
(elapsed <- Sys.time() - start.time)

#print R.hat 
R.hat 

