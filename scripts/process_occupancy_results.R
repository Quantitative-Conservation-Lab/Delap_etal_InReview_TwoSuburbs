library(ggplot2)
library(ggpubr)
library(viridis)

#process occupancy results to get info for tables in paper and create figures 

#read in model results for species 
results <- array(NA,dim=c(375000,12,43))

#note that we used more samples from CEDW 

results[,,1] <- as.matrix(read.csv("results/occ_run_17Sept.AMCR.csv")) #AMCR - Exploiter 
results[,,2] <- as.matrix(read.csv("results/occ_run_17Sept.AMGO.csv")) #AMGO - Adapter
results[,,3] <- as.matrix(read.csv("results/occ_run_17Sept.AMRO.csv")) #AMRO - Adapter
results[,,4] <- as.matrix(read.csv("results/occ_run_17Sept.ANHU.csv")) #ANHU - Exploiter
results[,,5] <- as.matrix(read.csv("results/occ_run_17Sept.BARS.csv")) #BARS - Exploiter
results[,,6] <- as.matrix(read.csv("results/occ_run4.BCCH.csv")) #BCCH - Adapter
results[,,7] <- as.matrix(read.csv("results/occ_run4.BEWR.csv")) #BEWR - Adapter
results[,,8] <- as.matrix(read.csv("results/occ_run4.BHCO.csv")) #BHCO - Exploiter
results[,,9] <- as.matrix(read.csv("results/occ_run4.BHGR.csv")) #BHGR - Adapter
results[,,10] <- as.matrix(read.csv("results/occ_run4.BRCR.csv")) #BRCR - Avoider
results[,,11] <- as.matrix(read.csv("results/occ_run4.BTYW.csv")) #BTYW - Avoider
results[,,12] <- as.matrix(read.csv("results/occ_run4.BUSH.csv")) #BUSH - Adapter
results[,,13] <- as.matrix(read.csv("results/occ_run4.CAVI.csv")) #CAVI - Adapter
results[,,14] <- as.matrix(read.csv("results/occ_run4.CBCH.csv")) #CBCH - Avoider
results[,,15] <- as.matrix(read.csv("results/occ_run4.CBCH.csv")) #CBCH - Avoider
results[,,16] <- as.matrix(read.csv("results/occ_run4.DEJU.csv")) #DEJU - Adapter
results[,,17] <- as.matrix(read.csv("results/occ_run4.EUST.csv")) #EUST - Exploiter
results[,,18] <- as.matrix(read.csv("results/occ_run4.GCKI.csv")) #GCKI - Avoider 
results[,,19] <- as.matrix(read.csv("results/occ_run4.HAWO.csv")) #HAWO - Avoider
results[,,20] <- as.matrix(read.csv("results/occ_run4.HOFI.csv")) #HOFI - Exploiter
results[,,21] <- as.matrix(read.csv("results/occ_run4.HOSP.csv")) #HOSP - Exploiter
results[,,22] <- as.matrix(read.csv("results/occ_run4.HUVI.csv")) #HUVI - Avoider
results[,,23] <- as.matrix(read.csv("results/occ_run4.NOFL.csv")) #NOFL - Adapter
results[,,24] <- as.matrix(read.csv("results/occ_run4.OCWA.csv")) #OCWA - Adapter
results[,,25] <- as.matrix(read.csv("results/occ_run4.PAWR.csv")) #PAWR - Avoider
results[,,26] <- as.matrix(read.csv("results/occ_run4.PISI.csv")) #PISI - Adapter
results[,,27] <- as.matrix(read.csv("results/occ_run4.PSFL.csv")) #PSFL - Avoider 
results[,,28] <- as.matrix(read.csv("results/occ_run4.PUFI.csv")) #PUFI - Avoider
results[,,29] <- as.matrix(read.csv("results/occ_run4.RBNU.csv")) #RBNU - Avoider
results[,,30] <- as.matrix(read.csv("results/occ_run4.RBSA.csv")) #RBSA - Avoider
results[,,31] <- as.matrix(read.csv("results/occ_run4.ROPI.csv")) #ROPI - Exploiter
results[,,32] <- as.matrix(read.csv("results/occ_run4.RUHU.csv")) #RUHU - Exploiter
results[,,33] <- as.matrix(read.csv("results/occ_run4.SOSP.csv")) #SOSP - Adapter
results[,,34] <- as.matrix(read.csv("results/occ_run4.SPTO.csv")) #SPTO - Adapter
results[,,35] <- as.matrix(read.csv("results/occ_run4.STJA.csv")) #STJA - Avoider 
results[,,36] <- as.matrix(read.csv("results/occ_run4.SWTH.csv")) #SWTH - Avoider
results[,,37] <- as.matrix(read.csv("results/occ_run4.VGSW.csv")) #VGSW - Adapter
results[,,38] <- as.matrix(read.csv("results/occ_run4.WAVI.csv")) #WAVI - Adapter
results[,,39] <- as.matrix(read.csv("results/occ_run4.WCSP.csv")) #WCSP - Adapter
results[,,40] <- as.matrix(read.csv("results/occ_run4.WETA.csv")) #WETA - Avoider
results[,,41] <- as.matrix(read.csv("results/occ_run4.WIFL.csv")) #WIFL - Adapter
results[,,42] <- as.matrix(read.csv("results/occ_run4.WIWA.csv")) #WIWA - Avoider
results[,,43] <- as.matrix(read.csv("results/occ_run4.YRWA.csv")) #YRWA - Adapter

results <- results[,-c(1,2),] #get rid of unneccesary columns 
dimnames(results) = list(NULL,
                         c("beta.site.type.2","beta.year","beta.year2","int.p","int.psi","sd.p.site","sd.p.year","w.site.type","w.year","w.year2"),
                         c("AMCR","AMGO","AMRO","ANHU","BARS","BCCH","BEWR","BHCO","BHGR","BRCR","BTYW","BUSH","CAVI","CBCH","CEDW","DEJU","EUST","GCKI","HAWO","HOFI","HOSP","HUVI","NOFL","OCWA","PAWR","PISI","PISFL","PUFI","RBNU","RBSA","ROPI","RUHU","SOSP","SPTO","STJA","SWTH","VGSW","WAVI","WCSP","WETA","WIFL","WIWA","YRWA"))

species.list <-  c("AMCR","AMGO","AMRO","ANHU","BARS","BCCH","BEWR","BHCO","BHGR","BRCR","BTYW","BUSH","CAVI","CBCH","CEDW","DEJU","EUST","GCKI","HAWO","HOFI","HOSP","HUVI","NOFL","OCWA","PAWR","PISI","PISFL","PUFI","RBNU","RBSA","ROPI","RUHU","SOSP","SPTO","STJA","SWTH","VGSW","WAVI","WCSP","WETA","WIFL","WIWA","YRWA")

#indicators for guilds 
avoiders.ind <- c(10,11,14,18,19,22,25,27,28,29,30,35,36,40,42)
adapters.ind <- c(2,3,6,7,9,12,13,15,16,23,24,26,33,34,37,38,39,41,43)
exploiters.ind <- c(1,4,5,8,17,20,21,31,32)

#put together the model weights table 
mod.sel <- function(results){
  model.selection <- matrix(NA,nrow=dim(results)[3],ncol=6)
  rownames(model.selection) <- unlist(dimnames(results)[3])
  colnames(model.selection) <- c("null","site.type","site.type + year","site.type + year2","year","year2")
  for(i in 1:nrow(model.selection)){
  temp <- paste(results[,"w.site.type",i], results[,"w.year",i], results[,"w.year2",i])
  model.selection[i,1] <- length(which(temp == "0 0 0"))
  model.selection[i,2] <- length(which(temp == "1 0 0"))
  model.selection[i,3] <- length(which(temp == "1 1 0"))
  model.selection[i,4] <- length(which(temp == "1 1 1"))
  model.selection[i,5] <- length(which(temp == "0 1 0"))
  model.selection[i,6] <- length(which(temp == "0 1 1"))
  }
  return(model.selection)
}

#model frequency, model weights, best model 
models <- mod.sel(results)
models.pr <- apply(models,1,function(x){x/sum(x)})
best.model <- apply(models.pr,2,which.max)

models.pr.avoiders <- models.pr[,avoiders.ind] #print this for paper 
best.model.avoiders <- best.model[avoiders.ind]

models.pr.adapters <- models.pr[,adapters.ind] #print this for paper 
best.model.adapters <- best.model[adapters.ind]

models.pr.exploiters <- models.pr[,exploiters.ind] #print this for paper 
best.model.exploiters <- best.model[exploiters.ind]

#get the parameters from the best model 
best.params <- function(best.model,results){
  params.best.model <- array(NA,dim=dim(results))
  for(i in 1:length(best.model)){
    if(best.model[i] == 1){ 
      temp <- c("0 0 0")
    }else if(best.model[i] == 2){
      temp <- c("1 0 0")
    }else if(best.model[i] == 3){
      temp <- c("1 1 0")
    }else if(best.model[i] == 4){
      temp <- c("1 1 1")
    }else if(best.model[i] == 5){
      temp <- c("0 1 0")
    }else {temp <- c("0 1 1")}
    model.out <- paste(results[,"w.site.type",i], results[,"w.year",i], results[,"w.year2",i])
    rows <- which(model.out == temp)
    params.best.model[1:length(rows),,i] <- results[rows,,i] 
  }
  return(params.best.model)
}
best.params.all <- best.params(best.model,results)

best.params.avoiders <- best.params.all[,c(5,1,2,3),avoiders.ind]
best.params.adapters <- best.params.all[,c(5,1,2,3),adapters.ind]
best.params.exploiters <- best.params.all[,c(5,1,2,3),exploiters.ind]
dimnames(best.params.all) <- dimnames(results) 

output.params <- function(best.params){
  means <- round(apply(best.params,c(2,3),function(x)mean(x,na.rm=TRUE)),dig=2)
  lci <- round(apply(best.params,c(2,3),function(x)quantile(x,probs=0.025,na.rm=TRUE)),dig=2)
  uci <- round(apply(best.params,c(2,3),function(x)quantile(x,probs=0.975,na.rm=TRUE)),dig=2)
  best.out <- means
  for(i in 1:nrow(means)){
    for(j in 1:ncol(means)){
      best.out[i,j] <- paste(means[i,j]," (",lci[i,j],", ",uci[i,j],")",sep="")
    }
  }
  return(best.out)
}
#these are for paper 
output.avoiders <- output.params(best.params.avoiders)
output.adapters <- output.params(best.params.adapters)
output.exploiters <- output.params(best.params.exploiters)

#Probability that the coefficient is negative 
prob.neg <- function(best.parms){
  prob.neg <- rep(NA,20)
  for(i in 1:dim(best.parms)[3]){
    best.parms <- best.parms[which(is.na(best.parms[,2,i])==FALSE),,]
    prob.neg[i] <- (length(which(best.parms[,2,i]<0))/length(best.parms[,2,i]))
  }
  return(prob.neg)
}
prob.neg(best.params.avoiders)
prob.neg(best.params.adapters)
prob.neg(best.params.exploiters)

#make predictions 
year.norm <- (c(1:12) - mean(c(1:12)))/sd(c(1:12))
year2.norm <- year.norm^2
covars <- cbind(c(year.norm,year.norm),c(year2.norm,year2.norm),c(rep(1,12),rep(2,12)))
colnames(covars) <- c("year","year2","site.type")

best.preds <- function(best.model,results,covars){
preds <- array(NA,dim=c(dim(results)[1],24,dim(results)[3]))
  for(i in 1:length(best.model)){
    for(j in 1:24){
      if(best.model[i]==1){
        preds[1:dim(results)[1],j,i] <- 1/(1+exp(-( results[1:dim(results)[1],"int.psi",i] )))
      }else if(best.model[i]==2){
        preds[1:dim(results)[1],j,i] <- 1/(1+exp(-( results[1:dim(results)[1],"int.psi",i] + results[1:dim(results)[1],"beta.site.type.2",i]*(covars[j,"site.type"]-1) )))
      }else if(best.model[i]==3){
        preds[1:dim(results)[1],j,i] <- 1/(1+exp(-( results[1:dim(results)[1],"int.psi",i] + results[1:dim(results)[1],"beta.site.type.2",i]*(covars[j,"site.type"]-1) + results[1:dim(results)[1],"beta.year",i]*covars[j,"year"] )))
      }else if(best.model[i]==4){
        preds[1:dim(results)[1],j,i] <- 1/(1+exp(-( results[1:dim(results)[1],"int.psi",i] + results[1:dim(results)[1],"beta.site.type.2",i]*(covars[j,"site.type"]-1) + results[1:dim(results)[1],"beta.year",i]*covars[j,"year"] + results[1:dim(results)[1],"beta.year2",i]*covars[j,"year2"] )))
      }else if(best.model[i]==5){
        preds[1:dim(results)[1],j,i] <- 1/(1+exp(-( results[1:dim(results)[1],"int.psi",i] + results[1:dim(results)[1],"beta.year",i]*covars[j,"year"] )))
      }else if(best.model[i]==6){
        preds[1:dim(results)[1],j,i] <- 1/(1+exp(-( results[1:dim(results)[1],"int.psi",i] + results[1:dim(results)[1],"beta.year",i]*covars[j,"year"] + results[1:dim(results)[1],"beta.year2",i]*covars[j,"year2"] )))
      }
    }
  }
  return(preds)
}
preds <- best.preds(best.model,best.params.all,covars)

#predictions for individual species 
preds.new <- array(NA,dim = c(dim(results)[3],dim(results)[1],24))
for(i in 1:43){
  preds.new[i,,] <- preds[,,i] 
}

#create all of the summaries of the data 
pv.mean <- apply(preds.new,c(3,1),function(x)mean(x,na.rm=TRUE))
pv.lowr <- apply(preds.new,c(3,1),function(x)quantile(x,p=0.025,na.rm=TRUE))
pv.uppr <- apply(preds.new,c(3,1),function(x)quantile(x,p=0.975,na.rm=TRUE))
pv.mean.cd <- pv.mean[1:12,]; pv.mean.pcd <- pv.mean[13:24,]
pv.lowr.cd <- pv.lowr[1:12,]; pv.lowr.pcd <- pv.lowr[13:24,]
pv.uppr.cd <- pv.uppr[1:12,]; pv.uppr.pcd <- pv.uppr[13:24,]

#name all the individual summaries 
c.mean.cd <- c.mean.pcd <- c.lowr.cd <- c.lowr.pcd <- c.uppr.cd <- c.uppr.pcd <- rep(NA,43)
for(i in 1:43){
  c.mean.cd[i] <- paste(dimnames(results)[[3]][i],".mean.cd",sep="")
  c.lowr.cd[i] <- paste(dimnames(results)[[3]][i],".lowr.cd",sep="")
  c.uppr.cd[i] <- paste(dimnames(results)[[3]][i],".uppr.cd",sep="")
  c.mean.pcd[i] <- paste(dimnames(results)[[3]][i],".mean.pcd",sep="")
  c.lowr.pcd[i] <- paste(dimnames(results)[[3]][i],".lowr.pcd",sep="")
  c.uppr.pcd[i] <- paste(dimnames(results)[[3]][i],".uppr.pcd",sep="")
}
colnames(pv.mean.cd) <- c.mean.cd; colnames(pv.mean.pcd) <- c.mean.pcd
colnames(pv.lowr.cd) <- c.lowr.cd; colnames(pv.lowr.pcd) <- c.lowr.pcd 
colnames(pv.uppr.cd) <- c.uppr.cd; colnames(pv.uppr.pcd) <- c.uppr.pcd
Year <- c(1:12)

#put everything in a data frame 
#this data frame has the mean for each species for cd, then the lower for each species for cd, then the upper for each species for cd, and so on for pcd
all.plot <- data.frame(pv.mean.cd,pv.lowr.cd,pv.uppr.cd,pv.mean.pcd,pv.lowr.pcd,pv.uppr.pcd,Year)

#these are the species that will go in each plot type 
#those with site type in the best model
plot_type_1 <- which(best.model==2 | best.model==3 | best.model==4)

#those without site type in the best model 
plot_type_2 <- which(best.model==1 | best.model==5 | best.model==6)



#set up the plotting functions - with site type
plot_type1 <- function(i){
  ggplot(data = all.plot) +
    geom_line(aes(x=Year,y = all.plot[,i],col="CD")) + #this is CD 
    geom_ribbon(aes(x=Year,ymin = all.plot[,i+43],ymax = all.plot[,i+43*2]), fill = viridis(4,option = "inferno")[1], alpha = 0.4) +
    geom_line(aes(x=Year,y = all.plot[,i+43*3],col="PCD")) +  #this is PCD
    geom_ribbon(aes(x=Year,ymin = all.plot[,i+43*4],ymax = all.plot[,i+43*5]), fill = viridis(4,option = "inferno")[3], alpha = 0.4) +
    geom_line(aes(x=Year,y = rep(-4,12),col="Combined")) +  #this is Junk
    scale_x_continuous(breaks=seq(1,12,2), limits = c(1,12)) +
    scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
    geom_text(x=7, y=0.1, size = 1.75, label=dimnames(preds.new)[[1]][i]) +
    scale_color_manual(name = "",breaks = c("CD","PCD","Combined"),values = c("CD" = viridis(4,option = "inferno")[1], "PCD" = viridis(4,option = "inferno")[3], "Combined" = viridis(4,option = "mako")[3]), drop = FALSE) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size = 12),legend.text = element_text(size=12),legend.key.width = unit(1.5,"cm"),legend.key = element_blank()) 
}

#without site type 
plot_type2 <- function(i){
  ggplot(data = all.plot) +
    geom_line(aes(x=Year,y = rep(-4,12),col="CD"), show.legend=TRUE) + #this is junk 
    geom_line(aes(x=Year,y = rep(-4,12),col="PCD"), show.legend=TRUE) +  #this is junk
    geom_line(aes(x=Year,y = all.plot[,i],col="Combined"), show.legend=TRUE) + #this is CD 
    geom_ribbon(aes(x=Year,ymin = all.plot[,i+43],ymax = all.plot[,i+43*2]), fill = viridis(4,option = "mako")[3], alpha = 0.4) +
    scale_x_continuous(breaks=seq(1,12,2), limits = c(1,12)) +
    scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
    geom_text(x=7, y=0.1, size = 1.75, label=dimnames(preds.new)[[1]][i]) +
    scale_color_manual(name = "", breaks = c("CD","PCD","Combined"), values = c("CD" = viridis(4,option = "inferno")[1], "PCD" = viridis(4,option = "inferno")[3], "Combined" = viridis(4,option = "mako")[3]), drop = FALSE) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size = 12),legend.text = element_text(size=12),legend.key.width = unit(1.5,"cm"),legend.key = element_blank()) 
}

#add best model to names 
dimnames(preds.new)[[1]] <- c("American Crow (5)","American Goldfinch (6)","American Robin (6)","Anna's Hummingbird (6)","Barn Swallow (2)","Black-capped Chickadee (2)", 
                       "Bewick's Wren (2)","Brown-headed Cowbird (4)","Black-headed Grosbeak (3)","Brown Creeper (4)",
                       "Black-throated Gray Warbler (3)","Bushtit (2)","Cassin's Vireo (4)","Chestnut-backed Chickadee (4)","Cedar Waxwing (6)","Dark-eyed Junco (1)", 
                       "European Starling (6)","Golden-crowned Kinglet (3)","Hairy Woodpecker (2)","House Finch (3)", 
                       "House Sparrow (3)","Hutton's Vireo (4)","Northern Flicker (4)","Orange-crowned Warbler (4)","Pacific Wren (4)", 
                       "Pine Siskin (4)","Pacific-slope Flycatcher (3)","Purple Finch (5)","Red-breasted Nuthatch (2)","Red-breasted Sapsucker (4)",
                       "Rock Pigeon (6)","Rufous Hummingbird (2)","Song Sparrow (4)","Spotted Towhee (2)","Steller's Jay (3)", 
                       "Swainson's Thrush (3)","Violet-green Swallow (6)","Warbling Vireo (3)","White-crowned Sparrow (5)", 
                       "Western Tanager (6)","Willow Flycatcher (6)","Wilson's Warbler (3)","Yellow-rumped Warbler (5)")


#######################################################################
#                                                                     #
#                         AVOIDER PLOT                                #
#                                                                     #
#######################################################################

#include best model with names 
avoider.spp <- c("Brown Creeper (3)","Black-throated Gray Warbler (3)","Chestnut-backed Chickadee (4)","Golden-crowned Kinglet (3)","Hairy Woodpecker (2)","Hutton's Vireo (4)",
                 "Pacific Wren (4)","Pacific-slope Flycatcher (3)","Purple Finch (5)","Red-breasted Nuthatch (2)","Red-breasted Sapsucker (4)",
                  "Steller's Jay (3)","Swainson's Thrush (3)","Western Tanager (6)","Wilson's Warbler (3)")


##CREATE AVOIDER.MEMBER AND THEN USE IT TO BUILD AVOIDER.GG 
avoider.member <- rep(NA,length(avoider.spp))
for(i in 1:length(avoider.spp)){
  if(is.element(avoider.spp[i],names(plot_type_1))==TRUE){
    avoider.member[i] <- paste(1,which(dimnames(pv)[[1]] == avoider.spp[i]),sep="-")
  }else if(is.element(avoider.spp[i],names(plot_type_2))==TRUE){
    avoider.member[i] <- paste(2,which(dimnames(pv)[[1]] == avoider.spp[i]),sep="-")
  }else avoider.member[i] <- NA
}

avoider.gg <- ggarrange(plot_type1(10),plot_type1(11),plot_type1(14),
                        plot_type1(18),plot_type1(19),plot_type1(22),
                        plot_type1(25),plot_type1(27),plot_type2(28),
                        plot_type1(29),plot_type1(30),plot_type1(35),
                        plot_type1(36),plot_type2(40),plot_type1(42),
                        common.legend=TRUE,show.legend=TRUE, legend = "right")

annotate_figure(avoider.gg,
                left = text_grob("Occupancy Probability", color = "black", size = 16, rot = 90),
                bottom = text_grob("Year",color = "black",size = 16))


#######################################################################
#                                                                     #
#                         ADAPTER PLOT                                #
#                                                                     #
#######################################################################

#include best model with names 
adapter.spp <- c("American Goldfinch (6)","American Robin (6)","Black-capped Chickadee (2)","Bewick's Wren (2)","Black-headed Grosbeak (3)","Bushtit (2)","Cassin's Vireo (4)",
                 "Cedar Waxwing (4)","Dark-eyed Junco (1)","Northern Flicker (4)","Orange-crowned Warbler (4)","Pine Siskin (4)","Song Sparrow (4)","Spotted Towhee (2)",
                 "Violet-green Swallow (6)","Warbling Vireo (3)","White-crowned Sparrow (5)","Willow Flycatcher (6)","Yellow-rumped Warbler (5)")


##CREATE adapter.MEMBER AND THEN USE IT TO BUILD EXPLOITER.GG 
adapter.member <- rep(NA,length(adapter.spp))
for(i in 1:length(adapter.spp)){
  if(is.element(adapter.spp[i],names(plot_type_1))==TRUE){
    adapter.member[i] <- paste(1,which(dimnames(pv)[[1]] == adapter.spp[i]),sep="-")
  }else if(is.element(adapter.spp[i],names(plot_type_2))==TRUE){
    adapter.member[i] <- paste(2,which(dimnames(pv)[[1]] == adapter.spp[i]),sep="-")
  }else adapter.member[i] <- NA
}

adapter.gg <- ggarrange(plot_type2(2),plot_type2(3),plot_type1(6),
                        plot_type1(7),plot_type1(9),plot_type1(12),
                        plot_type1(13),plot_type1(15),plot_type2(16),
                        plot_type1(23),plot_type1(24),plot_type1(26),
                        plot_type1(33),plot_type1(34),plot_type2(37),
                        plot_type1(38),plot_type2(39),plot_type2(41),
                        plot_type2(43),
                        common.legend=TRUE,show.legend=TRUE, legend = "right")

annotate_figure(adapter.gg,
                left = text_grob("Occupancy Probability", color = "black", size = 16, rot = 90),
                bottom = text_grob("Year",color = "black",size = 16))



#######################################################################
#                                                                     #
#                         EXPLOITER PLOT                              #
#                                                                     #
#######################################################################

#include best model with names 
exploiter.spp <- c("American Crow (5)","Anna's Hummingbird (6)","Barn Swallow (2)","Brown-headed Cowbird (4)",
                   "European Starling (6)","House Finch (3)","House Sparrow (3)","Rock Pigeon (6)","Rufous Hummingbird (2)")

##CREATE EXPLOITER.MEMBER AND THEN USE IT TO BUILD EXPLOITER.GG 
exploiter.member <- rep(NA,length(exploiter.spp))
for(i in 1:length(exploiter.spp)){
  if(is.element(exploiter.spp[i],names(plot_type_1))==TRUE){
    exploiter.member[i] <- paste(1,which(dimnames(pv)[[1]] == exploiter.spp[i]),sep="-")
  }else if(is.element(exploiter.spp[i],names(plot_type_2))==TRUE){
    exploiter.member[i] <- paste(2,which(dimnames(pv)[[1]] == exploiter.spp[i]),sep="-")
  }else exploiter.member[i] <- NA
}

exploiter.gg <- ggarrange(plot_type2(1),plot_type2(4),plot_type1(5),
                          plot_type1(8),plot_type2(17),plot_type1(20),
                          plot_type1(21),plot_type2(31),plot_type1(32),
                          common.legend=TRUE,show.legend=TRUE, legend = "right")

annotate_figure(exploiter.gg,
                left = text_grob("Occupancy Probability", color = "black", size = 16, rot = 90),
                bottom = text_grob("Year",color = "black",size = 16))


#######################################################################
#                                                                     #
#                           GUILD PLOT                                #
#                                                                     #
#######################################################################

#bring in guild results 
results.guilds <- array(NA,dim=c(105000,14,4))
results.guilds[,,1] <- as.matrix(read.csv("results/occ_run4.community.csv"))
results.guilds[,,2] <- as.matrix(read.csv("results/occ_run4.avoiders.csv"))
results.guilds[,,3] <- as.matrix(read.csv("results/occ_run4.adapters.csv"))
results.guilds[,,4] <- as.matrix(read.csv("results/occ_run4.exploiters.csv")) 

#get rid of unnecessary columns 
results.guilds <- results.guilds[,-c(1,2),]
dimnames(results.guilds) = list(NULL,
                                c("beta.site.type.2","beta.year","beta.year2","int.p","int.psi","sd.p.site","sd.p.species","sd.p.year","sd.psi.species","w.site.type","w.year","w.year2"),
                                c("community","avoiders","adapters","exploiters"))

#get model selection results 
models.guild <- mod.sel(results.guilds)
models.guild.pr <- apply(models.guild,1,function(x){x/sum(x)}) #print this for paper 
best.model.guild <- apply(models.guild.pr,2,which.max)

#get best params 
best.params.guild <- best.params(best.model.guild,results.guilds)
dimnames(best.params.guild) <- dimnames(results.guilds) 

#keep only parameters on psi 
best.params.guild <- best.params.guild[,c(5,1,2,3),]

#print best params values 
output.guild <- output.params(best.params.guild)
prob.neg(best.params.guild)

#get predictions 
preds.guild <- best.preds(best.model.guild,best.params.guild,covars)
preds.guild.new <- array(NA,dim = c(4,105000,24))
for(i in 1:4){
  preds.guild.new[i,,] <- preds.guild[,,i] 
}

#create all of the summaries of the data 
pv.guild.mean <- apply(preds.guild.new,c(3,1),function(x)mean(x,na.rm=TRUE))
pv.guild.lowr <- apply(preds.guild.new,c(3,1),function(x)quantile(x,p=0.025,na.rm=TRUE))
pv.guild.uppr <- apply(preds.guild.new,c(3,1),function(x)quantile(x,p=0.975,na.rm=TRUE))
pv.guild.mean.cd <- pv.guild.mean[1:12,]; pv.guild.mean.pcd <- pv.guild.mean[13:24,]
pv.guild.lowr.cd <- pv.guild.lowr[1:12,]; pv.guild.lowr.pcd <- pv.guild.lowr[13:24,]
pv.guild.uppr.cd <- pv.guild.uppr[1:12,]; pv.guild.uppr.pcd <- pv.guild.uppr[13:24,]

#labels for guilds 
label <- c("Community (4)", "Avoiders (3)", "Adapters (4)", "Exploiters (6)")

#name all the individual summaries 
c.guild.mean.cd <- c.guild.mean.pcd <- c.guild.lowr.cd <- c.guild.lowr.pcd <- c.guild.uppr.cd <- c.guild.uppr.pcd <- rep(NA,4)
for(i in 1:4){
  c.guild.mean.cd[i] <- paste(dimnames(results.guilds)[[3]][i],".mean.cd",sep="")
  c.guild.lowr.cd[i] <- paste(dimnames(results.guilds)[[3]][i],".lowr.cd",sep="")
  c.guild.uppr.cd[i] <- paste(dimnames(results.guilds)[[3]][i],".uppr.cd",sep="")
  c.guild.mean.pcd[i] <- paste(dimnames(results.guilds)[[3]][i],".mean.pcd",sep="")
  c.guild.lowr.pcd[i] <- paste(dimnames(results.guilds)[[3]][i],".lowr.pcd",sep="")
  c.guild.uppr.pcd[i] <- paste(dimnames(results.guilds)[[3]][i],".uppr.pcd",sep="")
}
colnames(pv.guild.mean.cd) <- c.guild.mean.cd; colnames(pv.guild.mean.pcd) <- c.guild.mean.pcd
colnames(pv.guild.lowr.cd) <- c.guild.lowr.cd; colnames(pv.guild.lowr.pcd) <- c.guild.lowr.pcd 
colnames(pv.guild.uppr.cd) <- c.guild.uppr.cd; colnames(pv.guild.uppr.pcd) <- c.guild.uppr.pcd
Year <- c(1:12)

all.plot.guild <- data.frame(pv.guild.mean.cd,pv.guild.lowr.cd,pv.guild.uppr.cd,pv.guild.mean.pcd,pv.guild.lowr.pcd,pv.guild.uppr.pcd,Year)

#set up the plotting functions 
plot_guilds1 <- function(i){
  ggplot(data = all.plot.guild) +
    geom_line(aes(x=Year,y = all.plot.guild[,i],col="CD")) + #this is CD 
    geom_ribbon(aes(x=Year,ymin = all.plot.guild[,i+4],ymax = all.plot.guild[,i+4*2]), fill = viridis(4,option = "inferno")[1], alpha = 0.3) +
    geom_line(aes(x=Year,y = all.plot.guild[,i+4*3],col="PCD")) +  #this is PCD
    geom_ribbon(aes(x=Year,ymin = all.plot.guild[,i+4*4],ymax = all.plot.guild[,i+4*5]), fill = viridis(4,option = "inferno")[3], alpha = 0.3) +
    geom_line(aes(x=Year,y = rep(-4,12),col="Combined")) + #this is junk 
    scale_x_continuous(breaks=seq(1,12,2), limits = c(1,12)) +
    scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
    geom_text(x=7, y=0.1, size = 4, label = label[i]) + # label=dimnames(pv.guild)[[1]][i]) +
    scale_color_manual(name = "", breaks = c("CD","PCD","Combined"), values = c("CD" = viridis(4,option = "inferno")[1], "PCD" = viridis(4,option = "inferno")[3], "Combined" = viridis(4,option = "mako")[3]), drop = FALSE) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size = 12),legend.text = element_text(size=12),legend.key.width = unit(1.5,"cm"),legend.key = element_blank()) 
  
}

#set up the plotting functions 
plot_guilds2 <- function(i){
  ggplot(data = all.plot.guild) +
    geom_line(aes(x=Year,y = rep(-4,12),col="CD")) + #this is junk 
    geom_line(aes(x=Year,y = rep(-4,12),col="PCD")) +  #this is junk
    geom_line(aes(x=Year,y = all.plot[,i],col="Combined"),show.legend=TRUE) + #this is CD 
    geom_ribbon(aes(x=Year,ymin = all.plot[,i+43],ymax = all.plot[,i+43*2]), fill = viridis(4,option = "mako")[3], alpha = 0.4) +
    scale_x_continuous(breaks=seq(1,12,2), limits = c(1,12)) +
    scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
    geom_text(x=7, y=0.1, size = 4, label = label[i]) + # label=dimnames(pv.guild)[[1]][i]) +
    scale_color_manual(name = "", breaks = c("CD","PCD","Combined"), values = c("CD" = viridis(4,option = "inferno")[1], "PCD" = viridis(4,option = "inferno")[3], "Combined" = viridis(4,option = "mako")[3]), drop = FALSE) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size = 12),legend.text = element_text(size=12),legend.key.width = unit(1.5,"cm"),legend.key = element_blank()) 
  
}


guilds <- ggarrange(plot_guilds1(1),plot_guilds1(2),plot_guilds1(3),plot_guilds2(4),common.legend=TRUE,show.legend=TRUE,legend = "right") 

annotate_figure(guilds,
                left = text_grob("Occupancy Probability", color = "black", size = 16, rot = 90),
                bottom = text_grob("Year",color = "black",size = 16))



