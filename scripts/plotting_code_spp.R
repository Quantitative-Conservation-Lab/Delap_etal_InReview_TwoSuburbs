library(here)
library(ggplot2)
library(ggpubr)
library(viridis)

boot.samps <- 1000 
load(here("scripts","allBoots_15April.RData"))

#########################################################################
#                                                                       #
#                 SET UP FOR SPP-SPECIFIC PLOTS                         #
#                                                                       #
#########################################################################

#which of the species will and won't be plotted (bootstrap performance)   
length.bad <- rep(NA,dim(pv)[1])
for(i in 1:dim(pv)[1]){
  length.bad[i] <- length(which(is.na(pv[i,,]==TRUE)))
}
#requiring 20% success rate or better 
success.number <- (boot.samps*0.2)*24         
which.good <- which(length.bad < success.number)
which.bad <- which(length.bad > (success.number-1))

#create all of the summaries of the data 
pv.mean <- apply(pv,c(3,1),function(x)mean(x,na.rm=TRUE))
pv.lowr <- apply(pv,c(3,1),function(x)quantile(x,p=0.025,na.rm=TRUE))
pv.uppr <- apply(pv,c(3,1),function(x)quantile(x,p=0.975,na.rm=TRUE))
pv.mean.cd <- pv.mean[1:12,]; pv.mean.pcd <- pv.mean[13:24,]
pv.lowr.cd <- pv.lowr[1:12,]; pv.lowr.pcd <- pv.lowr[13:24,]
pv.uppr.cd <- pv.uppr[1:12,]; pv.uppr.pcd <- pv.uppr[13:24,]

#name all the individual summaries 
c.mean.cd <- c.mean.pcd <- c.lowr.cd <- c.lowr.pcd <- c.uppr.cd <- c.uppr.pcd <- rep(NA,53)
for(i in 1:53){
  c.mean.cd[i] <- paste(dimnames(pv)[[1]][i],".mean.cd",sep="")
  c.lowr.cd[i] <- paste(dimnames(pv)[[1]][i],".lowr.cd",sep="")
  c.uppr.cd[i] <- paste(dimnames(pv)[[1]][i],".uppr.cd",sep="")
  c.mean.pcd[i] <- paste(dimnames(pv)[[1]][i],".mean.pcd",sep="")
  c.lowr.pcd[i] <- paste(dimnames(pv)[[1]][i],".lowr.pcd",sep="")
  c.uppr.pcd[i] <- paste(dimnames(pv)[[1]][i],".uppr.pcd",sep="")
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
plot_type_1 <- which(best.mods.out[1,]==2 | best.mods.out[1,]==5 | best.mods.out[1,]==6 | best.mods.out[1,]==7 | best.mods.out[1,]==8)
res1 <- vector()
for(i in 1:length(plot_type_1)){res1[i] <- is.element(names(plot_type_1)[i],colnames(store.AIC)[which.bad])}
plot_type_1 <- plot_type_1[-c(which(res1 == TRUE))] 

#those without site type in the best model 
plot_type_2 <- which(best.mods.out[1,]==1 | best.mods.out[1,]==3 | best.mods.out[1,]==4)
res2 <- vector()
for(i in 1:length(plot_type_2)){res2[i] <- is.element(names(plot_type_2)[i],colnames(store.AIC)[which.bad])}
plot_type_2 <- plot_type_2[-c(which(res2 == TRUE))] 

#set up the plotting functions 
plot_type1 <- function(i){
  ggplot(data = all.plot) +
  geom_line(aes(x=Year,y = all.plot[,i],col="CD")) + #this is CD 
  geom_ribbon(aes(x=Year,ymin = all.plot[,i+53],ymax = all.plot[,i+53*2]), fill = viridis(4,option = "inferno")[1], alpha = 0.4) +
  geom_line(aes(x=Year,y = all.plot[,i+53*3],col="PCD")) +  #this is PCD
  geom_ribbon(aes(x=Year,ymin = all.plot[,i+53*4],ymax = all.plot[,i+53*5]), fill = viridis(4,option = "inferno")[3], alpha = 0.4) +
  geom_line(aes(x=Year,y = rep(-4,12),col="Combined")) +  #this is Junk
  scale_x_continuous(breaks=seq(1,12,2), limits = c(1,12)) +
  scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
  geom_text(x=4, y=0.9, size = 4, label=dimnames(pv)[[1]][i]) +
  scale_color_manual(name = "", breaks = c("CD", "PCD", "Combined"), values = c("CD" = viridis(4,option = "inferno")[1], "PCD" = viridis(4,option = "inferno")[3], "Combined" = viridis(4,option = "mako")[3])) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size = 16),legend.text = element_text(size=16),legend.key.width = unit(2,"cm"),legend.key = element_blank()) 
}


plot_type2 <- function(i){
  ggplot(data = all.plot) +
    geom_line(aes(x=Year,y = all.plot[,i],col="Combined")) + #this is CD 
    geom_ribbon(aes(x=Year,ymin = all.plot[,i+53],ymax = all.plot[,i+53*2]), fill = viridis(4,option = "mako")[3], alpha = 0.4) +
    scale_x_continuous(breaks=seq(1,12,2), limits = c(1,12)) +
    scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
    geom_text(x=4, y=0.9, size = 4, label=dimnames(pv)[[1]][i]) +
    scale_color_manual(name = "", breaks = c("CD", "PCD", "Combined"), values = c("CD" = viridis(4,option = "inferno")[1], "PCD" = viridis(4,option = "inferno")[3], "Combined" = viridis(4,option = "mako")[3])) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size = 16),legend.text = element_text(size=16),legend.key.width = unit(2,"cm"),legend.key = element_blank()) 
}

#######################################################################
#                                                                     #
#                         EXPLOITER PLOT                              #
#                                                                     #
#######################################################################


##CREATE EXPLOITER.MEMBER AND THEN USE IT TO BUILD EXPLOITER.GG 
exploiter.spp <- c("AMCR","ANHU","BARS","BHCO","EUST","HOFI","HOSP","ROPI","RUHU")
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

#######################################################################
#                                                                     #
#                         ADAPTER PLOT                                #
#                                                                     #
#######################################################################


##CREATE ADAPTOR.MEMBER AND THEN USE IT TO BUILD EXPLOITER.GG 
adaptor.spp <- c("AMGO","AMRO","BCCH","BEWR","BHGR","BRBL","BTPI","BUSH","CAVI","CEDW","DEJU","EVGR","NOFL","OSFL","OCWA","PISI","SAVS","SOSP","SPTO","VASW","VGSW","WAVI","WCSP","WEWP","WIFL","YRWA")
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
#                         AVOIDER PLOT                                #
#                                                                     #
#######################################################################


##CREATE EXPLOITER.MEMBER AND THEN USE IT TO BUILD EXPLOITER.GG 
avoider.spp <- c("BRCR","BTYW","CBCH","DOWO","GCKI","HAWO","HUVI","PAWR","PSFL","PUFI","RBNU","RBSA","RCKI","STJA","SWTH","TOWA","WETA","WIWA")
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


