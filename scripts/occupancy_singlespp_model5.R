library(here)
library(nimble)
library(MCMCvis)
library(coda)


start.time <- Sys.time() 

# MCMC settings
ni <- 40000
nt <- 3
nb <- 5000
nc <- 3

tot.samples <- floor((ni-nb)/nt)*nc

#import data 
data1 <- read.csv("data/bird.binary.noCorrRes.csv") 

#set up sites, site type, years, species 
#sites - put in alphabetical order 
Sites <- sort(unique(data1$Site))
#site type - note that CD sites are 1 and PCD sites are 2
site.type <- Sites
for(s in 1:length(Sites)){
  site.type[s] <- data1$Site.Type[max(which(data1$Site == Sites[s]))]
}
site.type <- as.numeric(as.factor(site.type)) 
#years - put in order 
Years <- sort(unique(data1$Year))
Species <- c(9:61)

#set up observation data dim = sites, years, species, reps 
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

#effort array - make effort 0 anytime the data are NA  
array.eff <- array(1,dim=c(7,12,53,32))
array.eff[is.na(array2==TRUE)] <- 0 
#now make the data that are NAs 0s 
array2[is.na(array2==TRUE)] <- 0

#set up to hold analysis results
#species are rows 
#columns are mean predictions site type CD 1:12, mean predictions site type PCD 1:12, sd predictions, LCI predictions, UCI predictions, WAIC, multivariate R-hat, coefficients, sd coefficience, LCI coefficients, UCI coefficients 
results.mat <- matrix(NA,nrow=length(Species),ncol = 114)

#save all the samples from stochastic realizations 
z.pred <- array(NA,dim=c(length(Species),tot.samples,24,10))


#loop through species 
for(i in 1:length(Species)){

#pull out data for single species analysis
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
        spp.i[s,y,n] ~ dbern(z[s,y] * p[s,y] * eff.i[s,y,n])  #state * p * effort (0 or 1)
      }#n replicate
  }#y year
}#j site 


#model on detection
#model on occupancy 
for(s in 1:n.sites){
  for(y in 1:n.years){

    logit(p[s,y]) <- int.p + p.ran.site[s] + p.ran.year[y]
      
    logit(psi[s,y]) <- int.psi + beta.site.type[site.type[s]] + beta.year*y
  }
}
  
for(s in 1:2){
  for(y in 1:n.years){
    psi.pred[s,y] <- 1/(1+exp(-(int.psi + beta.site.type[site.type[s]] + beta.year*y )))
  }
} 
  
#Random effects 
for(s in 1:n.sites){
  p.ran.site[s] ~ dnorm(0,sd = sd.site)
}
for(y in 1:n.years){
  p.ran.year[y] ~ dnorm(0,sd = sd.year)
}
  
#Priors 
int.p ~ dnorm(0,sd=1)
sd.site ~ dgamma(1,1)
sd.year ~ dgamma(1,1)

int.psi ~ dnorm(0,sd=1)
beta.site.type[1] <- 0
beta.site.type[2] ~ dnorm(0,sd=1)
beta.year ~ dnorm(0,sd=1)


})


######################################################################
#                                                                    #  
#                      Data and Constants                            #
#                                                                    #    
######################################################################

# Bundle data
data <- list(spp.i=spp.i)

#get constants 
n.sites <- dim(spp.i)[1]
n.years <- dim(spp.i)[2]
n.reps <- dim(spp.i)[3]

constants <- list(eff.i=eff.i,site.type=site.type,n.sites=n.sites,n.years=n.years,n.reps=n.reps)

######################################################################
#                                                                    #  
#                             Inits                                  #
#                                                                    #    
######################################################################

z.init <- matrix(0,nrow=n.sites,ncol=n.years)
for(s in 1:n.sites){
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
params <- c("psi.pred","int.p","int.psi","beta.site.type","beta.year")

Rmodel1 <- nimbleModel(code = occ1, constants = constants, data = data,
                       check = FALSE, calculate = FALSE, inits = inits)
conf1 <- configureMCMC(Rmodel1, monitors = params, thin = nt, useConjugacy = FALSE, enableWAIC=TRUE)
Rmcmc1 <- buildMCMC(conf1)
Cmodel1 <- compileNimble(Rmodel1, showCompilerOutput = FALSE)
Cmcmc1 <- compileNimble(Rmcmc1, project = Rmodel1)

## Run MCMC ####
out <- runMCMC(Cmcmc1, niter = ni, nburnin = nb , nchains = nc, inits = inits,
               setSeed = FALSE, progressBar = TRUE, samplesAsCodaMCMC = TRUE, WAIC=TRUE)

params <- c("psi.pred[1, 1]","psi.pred[1, 2]","psi.pred[1, 3]","psi.pred[1, 4]","psi.pred[1, 5]","psi.pred[1, 6]","psi.pred[1, 7]","psi.pred[1, 8]","psi.pred[1, 9]","psi.pred[1, 10]","psi.pred[1, 11]","psi.pred[1, 12]",
            "psi.pred[2, 1]","psi.pred[2, 2]","psi.pred[2, 3]","psi.pred[2, 4]","psi.pred[2, 5]","psi.pred[2, 6]","psi.pred[2, 7]","psi.pred[2, 8]","psi.pred[2, 9]","psi.pred[2, 10]","psi.pred[2, 11]","psi.pred[2, 12]",
            "int.p","int.psi","beta.site.type[2]","beta.year") 

out.all <- rbind(out$samples$chain1,out$samples$chain2,out$samples$chain3)

out.all <- out.all[,params]

#store results 

#int.p, int.psi mean
#psi.pred mean 
results.mat[i,1:24] <- apply(out.all[,1:24], 2, mean) 
#psi.pred sd 
results.mat[i,25:48] <- apply(out.all[,1:24], 2, sd) 
#psi.pred LCI 
results.mat[i,49:72] <- apply(out.all[,1:24], 2, function(x)quantile(x,probs=0.025)) 
#psi.pred LCI 
results.mat[i,73:96] <- apply(out.all[,1:24], 2, function(x)quantile(x,probs=0.975))

#coefficients mean
results.mat[i,97:100] <- apply(out.all[,25:28], 2, mean)  
#coefficients sd 
results.mat[i,101:104] <- apply(out.all[,25:28], 2, sd)
#coefficients LCI 
results.mat[i,105:108] <- apply(out.all[,25:28], 2, function(x)quantile(x,probs=0.025)) 
#coefficients UCI 
results.mat[i,109:112] <- apply(out.all[,25:28], 2, function(x)quantile(x,probs=0.975)) 

#WAIC
results.mat[i,113] <- out$WAIC$WAIC
#R-hat 
results.mat[i,114] <- gelman.diag(out$samples[,c(2,3,4,5)],multivariate=TRUE)$mpsrf

for(s in 1:nrow(out.all)){
  for(j in 1:24){
    for(k in 1:10){
      z.pred[i,s,j,k] <- rbinom(1,1,out.all[s,j])
    }
  }
}

print(i)
}

colnames(results.mat) <- c(paste(params[1:24],".mean",sep=""),paste(params[1:24],".sd",sep=""),paste(params[1:24],".LCI",sep=""),paste(params[1:24],".UCI",sep=""),paste(params[25:28],".mean",sep=""),paste(params[25:28],".sd",sep=""),paste(params[25:28],".LCI",sep=""),paste(params[25:28],".UCI",sep=""),"WAIC","R.hat")

all.species <- apply(z.pred,c(2,3,4),sum)


write.csv(results.mat,"results/occ_model5_results.csv")
write.csv(all.species,"results/occ_model5_allspp.csv")


end.time <- Sys.time() 

elapsed <- end.time - start.time 
elapsed

