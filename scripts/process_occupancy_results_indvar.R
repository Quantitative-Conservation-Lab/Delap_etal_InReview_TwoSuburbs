#read in all model results
results <- array(NA,dim=c(105000,13,43))

results[,,1] <- as.matrix(read.csv("results/occ_run2.AMCR.csv")) #AMCR - Exploiter 
results[,,2] <- as.matrix(read.csv("results/occ_run2.AMGO.csv")) #AMGO - Adapter
results[,,3] <- as.matrix(read.csv("results/occ_run2.AMRO.csv")) #AMRO - Adapter
results[,,4] <- as.matrix(read.csv("results/occ_run2.ANHU.csv")) #ANHU - Exploiter
results[,,5] <- as.matrix(read.csv("results/occ_run2.BARS.csv")) #BARS - Exploiter
results[,,6] <- as.matrix(read.csv("results/occ_run2.BCCH.csv")) #BCCH - Adapter
results[,,7] <- as.matrix(read.csv("results/occ_run2.BEWR.csv")) #BEWR - Adapter
results[,,8] <- as.matrix(read.csv("results/occ_run2.BHCO.csv")) #BHCO - Exploiter
results[,,9] <- as.matrix(read.csv("results/occ_run2.BHGR.csv")) #BHGR - Adapter
results[,,10] <- as.matrix(read.csv("results/occ_run2.BRCR.csv")) #BRCR - Avoider
results[,,11] <- as.matrix(read.csv("results/occ_run2.BTYW.csv")) #BTYW - Avoider
results[,,12] <- as.matrix(read.csv("results/occ_run2.BUSH.csv")) #BUSH - Adapter
results[,,13] <- as.matrix(read.csv("results/occ_run2.CAVI.csv")) #CAVI - Adapter
results[,,14] <- as.matrix(read.csv("results/occ_run2.CBCH.csv")) #CBCH - Avoider
results[,,15] <- as.matrix(read.csv("results/occ_run2.CEDW.csv")) #CEDW - Adapter
results[,,16] <- as.matrix(read.csv("results/occ_run2.DEJU.csv")) #DEJU - Adapter
results[,,17] <- as.matrix(read.csv("results/occ_run2.EUST.csv")) #EUST - Exploiter
results[,,18] <- as.matrix(read.csv("results/occ_run2.GCKI.csv")) #GCKI - Avoider 
results[,,19] <- as.matrix(read.csv("results/occ_run2.HAWO.csv")) #HAWO - Avoider
results[,,20] <- as.matrix(read.csv("results/occ_run2.HOFI.csv")) #HOFI - Exploiter
results[,,21] <- as.matrix(read.csv("results/occ_run2.HOSP.csv")) #HOSP - Exploiter
results[,,22] <- as.matrix(read.csv("results/occ_run2.HUVI.csv")) #HUVI - Avoider
results[,,23] <- as.matrix(read.csv("results/occ_run2.NOFL.csv")) #NOFL - Adapter
results[,,24] <- as.matrix(read.csv("results/occ_run2.OCWA.csv")) #OCWA - Adapter
results[,,25] <- as.matrix(read.csv("results/occ_run2.PAWR.csv")) #PAWR - Avoider
results[,,26] <- as.matrix(read.csv("results/occ_run2.PISI.csv")) #PISI - Adapter
results[,,27] <- as.matrix(read.csv("results/occ_run2.PSFL.csv")) #PSFL - Avoider 
results[,,28] <- as.matrix(read.csv("results/occ_run2.PUFI.csv")) #PUFI - Avoider
results[,,29] <- as.matrix(read.csv("results/occ_run2.RBNU.csv")) #RBNU - Avoider
results[,,30] <- as.matrix(read.csv("results/occ_run2.RBSA.csv")) #RBSA - Avoider
results[,,31] <- as.matrix(read.csv("results/occ_run2.ROPI.csv")) #ROPI - Exploiter
results[,,32] <- as.matrix(read.csv("results/occ_run2.RUHU.csv")) #RUHU - Exploiter
results[,,33] <- as.matrix(read.csv("results/occ_run2.SOSP.csv")) #SOSP - Adapter
results[,,34] <- as.matrix(read.csv("results/occ_run2.SPTO.csv")) #SPTO - Adapter
results[,,35] <- as.matrix(read.csv("results/occ_run2.STJA.csv")) #STJA - Avoider 
results[,,36] <- as.matrix(read.csv("results/occ_run2.SWTH.csv")) #SWTH - Avoider
results[,,37] <- as.matrix(read.csv("results/occ_run2.VGSW.csv")) #VGSW - Adapter
results[,,38] <- as.matrix(read.csv("results/occ_run2.WAVI.csv")) #WAVI - Adapter
results[,,39] <- as.matrix(read.csv("results/occ_run2.WCSP.csv")) #WCSP - Adapter
results[,,40] <- as.matrix(read.csv("results/occ_run2.WETA.csv")) #WETA - Avoider
results[,,41] <- as.matrix(read.csv("results/occ_run2.WIFL.csv")) #WIFL - Adapter
results[,,42] <- as.matrix(read.csv("results/occ_run2.WIWA.csv")) #WIWA - Avoider
results[,,43] <- as.matrix(read.csv("results/occ_run2.YRWA.csv")) #YRWA - Adapter

avoiders.ind <- c(10,11,14,18,19,22,25,27,28,29,30,35,36,40,42)
adapters.ind <- c(2,3,6,7,9,12,13,15,16,23,24,26,33,34,37,38,39,41,43)
exploiters.ind <- c(1,4,5,8,17,20,21,31,32)

results <- results[,-c(1,2,9),]
dimnames(results) = list(NULL,
                     c("beta.site.type.2","beta.year","beta.year2","int.p","int.psi","p.site.type.1","sd.year","w.site.type","w.year","w.year2"),
                     c("AMCR","AMGO","AMRO","ANHU","BARS","BCCH","BEWR","BHCO","BHGR","BRCR","BTYW","BUSH","CAVI","CBCH","CEDW","DEJU","EUST","GCKI","HAWO","HOFI","HOSP","HUVI","NOFL","OCWA","PAWR","PISI","PISFL","PUFI","RBNU","RBSA","ROPI","RUHU","SOSP","SPTO","STJA","SWTH","VGSW","WAVI","WCSP","WETA","WIFL","WIWA","YRWA"))

#put together the model weights selection table 
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

models.pr.avoiders <- models.pr[,avoiders.ind]
best.model.avoiders <- best.model[avoiders.ind]

models.pr.adapters <- models.pr[,adapters.ind]
best.model.adapters <- best.model[adapters.ind]

models.pr.exploiters <- models.pr[,exploiters.ind]
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
best.params <- best.params(best.model,results)

best.params.avoiders <- best.params[,c(5,1,2,3),avoiders.ind]
best.params.adapters <- best.params[,c(5,1,2,3),adapters.ind]
best.params.exploiters <- best.params[,c(5,1,2,3),exploiters.ind]
dimnames(best.params) <- dimnames(results) 

output.params <- function(best.params){
  means <- round(apply(best.params,c(2,3),function(x)mean(x,na.rm=TRUE)),dig=3)
  lci <- round(apply(best.params,c(2,3),function(x)quantile(x,probs=0.025,na.rm=TRUE)),dig=3)
  uci <- round(apply(best.params,c(2,3),function(x)quantile(x,probs=0.975,na.rm=TRUE)),dig=3)
  best.out <- means
  for(i in 1:nrow(means)){
    for(j in 1:ncol(means)){
      best.out[i,j] <- paste(means[i,j]," (",lci[i,j],", ",uci[i,j],")",sep="")
    }
  }
  return(best.out)
}
output.avoiders <- output.params(best.params.avoiders)
output.adapters <- output.params(best.params.adapters)
output.exploiters <- output.params(best.params.exploiters)

#make predictions 

year.norm <- (c(1:12) - mean(c(1:12)))/sd(c(1:12))
year2.norm <- pow(year.norm,2)
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
        preds[1:dim(results)[1],,i] <- 1/(1+exp(-( results[1:dim(results)[1],"int.psi",i] + results[1:dim(results)[1],"beta.site.type.2",i]*(covars[j,"site.type"]-1) + results[1:dim(results)[1],"beta.year",i]*covars[j,"year"] )))
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
preds <- best.preds(best.model,best.params,covars)

preds.out <- matrix(NA,12,dim(preds)[3]*6+1)
preds.out[,1:43] <- apply(preds,c(2,3),function(x)mean(x,na.rm=TRUE))[1:12,]
preds.out[,44:86] <- apply(preds,c(2,3),function(x)quantile(x,probs=0.025,na.rm=TRUE))[1:12,]
preds.out[,87:129] <- apply(preds,c(2,3),function(x)quantile(x,probs=0.975,na.rm=TRUE))[1:12,]
preds.out[,130:172] <- apply(preds,c(2,3),function(x)mean(x,na.rm=TRUE))[13:24,]
preds.out[,173:215] <- apply(preds,c(2,3),function(x)quantile(x,probs=0.025,na.rm=TRUE))[13:24,]
preds.out[,216:258] <- apply(preds,c(2,3),function(x)quantile(x,probs=0.975,na.rm=TRUE))[13:24,]
preds.out[,259] <- c(1:12)

###PLOTTING CODE 

#these are the species that will go in each plot type 
#those with site type in the best model
plot_type_1 <- which(best.model==2 | best.model==3 | best.model==4)

#those without site type in the best model 
plot_type_2 <- which(best.model==1 | best.model==5 | best.model==6)

#set up the plotting functions 
plot_type1 <- function(i){
  ggplot(data = all.plot) +
    geom_line(aes(x=Year,y = all.plot[,i],col="CD")) + #this is CD 
    geom_ribbon(aes(x=Year,ymin = all.plot[,i+43],ymax = all.plot[,i+43*2]), fill = viridis(4,option = "inferno")[1], alpha = 0.4) +
    geom_line(aes(x=Year,y = all.plot[,i+43*3],col="PCD")) +  #this is PCD
    geom_ribbon(aes(x=Year,ymin = all.plot[,i+43*4],ymax = all.plot[,i+43*5]), fill = viridis(4,option = "inferno")[3], alpha = 0.4) +
    geom_line(aes(x=Year,y = rep(-4,12),col="Combined")) +  #this is Junk
    scale_x_continuous(breaks=seq(1,12,2), limits = c(1,12)) +
    scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
    geom_text(x=7, y=0.9, size = 3.5, label=dimnames(pv)[[1]][i]) +
    scale_color_manual(name = "", breaks = c("CD", "PCD", "Combined"), values = c("CD" = viridis(4,option = "inferno")[1], "PCD" = viridis(4,option = "inferno")[3], "Combined" = viridis(4,option = "mako")[3])) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size = 16),legend.text = element_text(size=16),legend.key.width = unit(2,"cm"),legend.key = element_blank()) 
}


plot_type2 <- function(i){
  ggplot(data = all.plot) +
    geom_line(aes(x=Year,y = all.plot[,i],col="Combined")) + #this is CD 
    geom_ribbon(aes(x=Year,ymin = all.plot[,i+43],ymax = all.plot[,i+43*2]), fill = viridis(4,option = "mako")[3], alpha = 0.4) +
    scale_x_continuous(breaks=seq(1,12,2), limits = c(1,12)) +
    scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
    geom_text(x=7, y=0.9, size = 3.5, label=dimnames(pv)[[1]][i]) +
    scale_color_manual(name = "", breaks = c("CD", "PCD", "Combined"), values = c("CD" = viridis(4,option = "inferno")[1], "PCD" = viridis(4,option = "inferno")[3], "Combined" = viridis(4,option = "mako")[3])) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size = 16),legend.text = element_text(size=16),legend.key.width = unit(2,"cm"),legend.key = element_blank()) 
}

#add best model to names 
dimnames(pv)[[1]] <- c("American Crow (6)","American Goldfinch (6)","American Robin (8)","Anna's Hummingbird (4)","Barn Swallow (8)","Black-capped Chickadee (2)", 
                       "Bewick's Wren (8)","Brown-headed Cowbird (6)","Black-headed Grosbeak (2)","Brown Creeper (8)", 
                       "Black-throated Gray Warbler (8)","Bushtit (2)","Cassin's Vireo (6)","Chestnut-backed Chickadee (8)","Cedar Waxwing (8)","Dark-eyed Junco (4)", 
                       "European Starling (8)","Golden-crowned Kinglet (8)","Hairy Woodpecker (5)","House Finch (8)", 
                       "House Sparrow (4)","Hutton's Vireo (8)","Northern Flicker (8)","Orange-crowned Warbler (8)","Pacific Wren (8)", 
                       "Pine Siskin (8)","Pacific-slope Flycatcher (7)","Purple Finch (8)","Red-breasted Nuthatch (6)","Red-breasted Sapsucker (4)", 
                       "Rock Pigeon (6)","Rufous Hummingbird (6)","Song Sparrow (6)","Spotted Towhee (8)","Steller's Jay (8)", 
                       "Swainson's Thrush (8)","Violet-green Swallow (8)","Warbling Vireo (5)","White-crowned Sparrow (8)", 
                       "Western Tanager (8)","Willow Flycatcher (6)","Wilson's Warbler (5)","Yellow-rumped Warbler (3)")


#######################################################################
#                                                                     #
#                         AVOIDER PLOT                                #
#                                                                     #
#######################################################################

avoider.spp <- c("Brown Creeper (8)","Black-throated Gray Warbler (8)","Chestnut-backed Chickadee (8)","Golden-crowned Kinglet (8)","Hairy Woodpecker (5)","Hutton's Vireo (8)","Pacific Wren (8)","Pacific-slope Flycatcher (7)","Purple Finch (8)","Red-breasted Nuthatch (6)","Red-breasted Sapsucker (4)","Ruby-crowned Kinglet (2)","Steller's Jay (8)","Swainson's Thrush (8)","Western Tanager (8)","Wilson's Warbler (5)")

##CREATE EXPLOITER.MEMBER AND THEN USE IT TO BUILD EXPLOITER.GG 
#avoider.spp <- c("Brown Creeper","Black-throated Gray Warbler","Chestnut-backed Chickadee","DOWO","Golden-crowned Kinglet","Hairy Woodpecker","Hutton's Vireo","Pacific Wren","Pacific-slope Flycatcher","Purple Finch","Red-breasted Nuthatch","Red-breasted Sapsucker","Ruby-crowned Kinglet","Steller's Jay","Swainson's Thrush","TOWA","Western Tanager","Wilson's Warbler")
avoider.member <- rep(NA,length(avoider.spp))
for(i in 1:length(avoider.spp)){
  if(is.element(avoider.spp[i],names(plot_type_1))==TRUE){
    avoider.member[i] <- paste(1,which(dimnames(pv)[[1]] == avoider.spp[i]),sep="-")
  }else if(is.element(avoider.spp[i],names(plot_type_2))==TRUE){
    avoider.member[i] <- paste(2,which(dimnames(pv)[[1]] == avoider.spp[i]),sep="-")
  }else avoider.member[i] <- NA
}

avoider.gg <- ggarrange(plot_type1(11),plot_type1(13),plot_type1(16),plot_type1(22),plot_type1(23),plot_type1(26),plot_type1(30),plot_type1(32),plot_type1(33),plot_type1(34),plot_type2(35),plot_type1(36),plot_type1(42),plot_type1(43),plot_type1(49),plot_type1(52),common.legend=TRUE, legend = "right")

annotate_figure(avoider.gg,
                left = text_grob("Pr(observing spp)", color = "black", size = 18, rot = 90),
                bottom = text_grob("Year",color = "black",size = 18))


#######################################################################
#                                                                     #
#                         ADAPTER PLOT                                #
#                                                                     #
#######################################################################


adaptor.spp <- c("American Goldfinch (6)","American Robin (8)","Black-capped Chickadee (2)","Bewick's Wren (8)","Black-headed Grosbeak (2)","Bushtit (2)","Cassin's Vireo (6)","Cedar Waxwing (8)","Dark-eyed Junco (4)","Northern Flicker (8)","Orange-crowned Warbler (8)","Pine Siskin (8)","Song Sparrow (6)","Spotted Towhee (8)","Violet-green Swallow (8)","Warbling Vireo (5)","White-crowned Sparrow (8)","Willow Flycatcher (6)","Yellow-rumped Warbler (3)")

##CREATE ADAPTOR.MEMBER AND THEN USE IT TO BUILD EXPLOITER.GG 
#adaptor.spp <- c("American Goldfinch","American Robin","Black-capped Chickadee","Bewick's Wren","Black-headed Grosbeak","BRBL","BTPI","Bushtittit","CAVI","Cedar Waxwing","Dark-eyed Junco","EVGR","Northern Flicker","Orange-crowned Warbler","OSFL","Pine Siskin","Savannah Sparrow","Song Sparrow","Spotted Towhee","VASW","Violet-green Swallow","Warbling Vireo","White-crowned Sparrow","WEWP","Willow Flycatcher","Yellow-rumped Warbler")
adaptor.member <- rep(NA,length(adaptor.spp))
for(i in 1:length(adaptor.spp)){
  if(is.element(adaptor.spp[i],names(plot_type_1))==TRUE){
    adaptor.member[i] <- paste(1,which(dimnames(pv)[[1]] == adaptor.spp[i]),sep="-")
  }else if(is.element(adaptor.spp[i],names(plot_type_2))==TRUE){
    adaptor.member[i] <- paste(2,which(dimnames(pv)[[1]] == adaptor.spp[i]),sep="-")
  }else adaptor.member[i] <- NA
}

adaptor.gg <- ggarrange(plot_type1(2),plot_type1(3),plot_type1(6),plot_type1(7),plot_type1(9),plot_type1(14),plot_type1(17),plot_type2(18),plot_type1(27),plot_type1(28),plot_type1(31),plot_type2(39),plot_type1(40),plot_type1(41),plot_type1(46),plot_type1(47),plot_type1(48),plot_type1(51),plot_type2(53),common.legend=TRUE, legend = "right")

annotate_figure(adaptor.gg,
                left = text_grob("Pr(observing spp)", color = "black", size = 18, rot = 90),
                bottom = text_grob("Year",color = "black",size = 18))



#######################################################################
#                                                                     #
#                         EXPLOITER PLOT                              #
#                                                                     #
#######################################################################

exploiter.spp <- c("American Crow (6)","Anna's Hummingbird (4)","Barn Swallow (8)","Brown-headed Cowbird (6)","European Starling (8)","House Finch (8)", "House Sparrow (4)", "Rock Pigeon (6)","Rufous Hummingbird (6)")


##CREATE EXPLOITER.MEMBER AND THEN USE IT TO BUILD EXPLOITER.GG 
#exploiter.spp <- c("American Crow","Anna's Hummingbird","Barn Swallow","Brown-headed Cowbird","European Starling","House Finch","House Sparrow","Rock Pigeon","Rufous Hummingbird")
exploiter.member <- rep(NA,length(exploiter.spp))
for(i in 1:length(exploiter.spp)){
  if(is.element(exploiter.spp[i],names(plot_type_1))==TRUE){
    exploiter.member[i] <- paste(1,which(dimnames(pv)[[1]] == exploiter.spp[i]),sep="-")
  }else if(is.element(exploiter.spp[i],names(plot_type_2))==TRUE){
    exploiter.member[i] <- paste(2,which(dimnames(pv)[[1]] == exploiter.spp[i]),sep="-")
  }else exploiter.member[i] <- NA
}

exploiter.gg <- ggarrange(plot_type1(1),plot_type2(4),plot_type1(5),plot_type1(8),plot_type1(20),plot_type1(24),plot_type2(25),plot_type1(37),plot_type1(38),common.legend=TRUE, legend = "right")

annotate_figure(exploiter.gg,
                left = text_grob("Pr(observing spp)", color = "black", size = 18, rot = 90),
                bottom = text_grob("Year",color = "black",size = 18))
