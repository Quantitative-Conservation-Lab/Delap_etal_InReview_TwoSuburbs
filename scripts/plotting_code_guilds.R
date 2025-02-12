library(here)
library(ggplot2)
library(ggpubr)
library(viridis)

boot.samps <- 1000 
load(here("results","allBoots_15April.RData"))

#########################################################################
#                                                                       #
#                           PLOT GUILDS                                 #
#                                                                       #
#########################################################################

#create all of the summaries of the data 
pv.guild.mean <- apply(pv.guild,c(3,1),function(x)mean(x,na.rm=TRUE))
pv.guild.lowr <- apply(pv.guild,c(3,1),function(x)quantile(x,p=0.025,na.rm=TRUE))
pv.guild.uppr <- apply(pv.guild,c(3,1),function(x)quantile(x,p=0.975,na.rm=TRUE))
pv.guild.mean.cd <- pv.guild.mean[1:12,]; pv.guild.mean.pcd <- pv.guild.mean[13:24,]
pv.guild.lowr.cd <- pv.guild.lowr[1:12,]; pv.guild.lowr.pcd <- pv.guild.lowr[13:24,]
pv.guild.uppr.cd <- pv.guild.uppr[1:12,]; pv.guild.uppr.pcd <- pv.guild.uppr[13:24,]


label <- c("Avoiders (8)", "Adapters (8)", "Exploiters (8)", "Community (8)")



#name all the individual summaries 
c.guild.mean.cd <- c.guild.mean.pcd <- c.guild.lowr.cd <- c.guild.lowr.pcd <- c.guild.uppr.cd <- c.guild.uppr.pcd <- rep(NA,4)
for(i in 1:4){
  c.guild.mean.cd[i] <- paste(dimnames(pv.guild)[[1]][i],".mean.cd",sep="")
  c.guild.lowr.cd[i] <- paste(dimnames(pv.guild)[[1]][i],".lowr.cd",sep="")
  c.guild.uppr.cd[i] <- paste(dimnames(pv.guild)[[1]][i],".uppr.cd",sep="")
  c.guild.mean.pcd[i] <- paste(dimnames(pv.guild)[[1]][i],".mean.pcd",sep="")
  c.guild.lowr.pcd[i] <- paste(dimnames(pv.guild)[[1]][i],".lowr.pcd",sep="")
  c.guild.uppr.pcd[i] <- paste(dimnames(pv.guild)[[1]][i],".uppr.pcd",sep="")
}
colnames(pv.guild.mean.cd) <- c.guild.mean.cd; colnames(pv.guild.mean.pcd) <- c.guild.mean.pcd
colnames(pv.guild.lowr.cd) <- c.guild.lowr.cd; colnames(pv.guild.lowr.pcd) <- c.guild.lowr.pcd 
colnames(pv.guild.uppr.cd) <- c.guild.uppr.cd; colnames(pv.guild.uppr.pcd) <- c.guild.uppr.pcd
Year <- c(1:12)

all.plot.guild <- data.frame(pv.guild.mean.cd,pv.guild.lowr.cd,pv.guild.uppr.cd,pv.guild.mean.pcd,pv.guild.lowr.pcd,pv.guild.uppr.pcd,Year)

scale.max <- c(0.50, 0.50, 0.50, 0.50)
interval <- c(0.05,0.05,0.05,0.05)
text.loc <- c(0.45,0.45,0.45,0.45)


#set up the plotting functions 
plot_guilds <- function(i){
  ggplot(data = all.plot.guild) +
    geom_line(aes(x=Year,y = all.plot.guild[,i],col="CD")) + #this is CD 
    geom_ribbon(aes(x=Year,ymin = all.plot.guild[,i+4],ymax = all.plot.guild[,i+4*2]), fill = viridis(4,option = "inferno")[1], alpha = 0.3) +
    geom_line(aes(x=Year,y = all.plot.guild[,i+4*3],col="PCD")) +  #this is PCD
    geom_ribbon(aes(x=Year,ymin = all.plot.guild[,i+4*4],ymax = all.plot.guild[,i+4*5]), fill = viridis(4,option = "inferno")[3], alpha = 0.3) +
    scale_x_continuous(breaks=seq(1,12,2), limits = c(1,12)) +
    scale_y_continuous(breaks=seq(0,scale.max[i],interval[i]), limits = c(0,scale.max[i])) +
    geom_text(x=4, y=text.loc[i], size = 6, label = label[i]) + # label=dimnames(pv.guild)[[1]][i]) +
    scale_color_manual(name = "", breaks = c("CD", "PCD"), values = c("CD" = viridis(4,option = "inferno")[1], "PCD" = viridis(4,option = "inferno")[3])) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size = 16),legend.text = element_text(size=16),legend.key.width = unit(2,"cm"),legend.key = element_blank()) 
  
}

guilds <- ggarrange(plot_guilds(1),plot_guilds(2),plot_guilds(3),plot_guilds(4),common.legend=TRUE,legend = "right") 

annotate_figure(guilds,
                left = text_grob("Proportion of Guild Observed", color = "black", size = 18, rot = 90),
                bottom = text_grob("Year",color = "black",size = 18))

