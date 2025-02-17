library(here)
library(unmarked)
library(tidyr)

#import data 
data1 <- read.csv("data/bird.binary.noCorrRes.csv") 

temp1 <- paste("Point",data1$Point,sep=" ") 
temp2 <- paste("Round",data1$Round,sep=" ")
data1$Point.Round <- paste(temp1,temp2,sep=",")

data.fact <- data.frame(data1$Year,data1$Site,data1$Site.Type,data1$Point.Round)
data.spp <- data1[,c(9:61)]

for(i in 1:ncol(data.spp)){
  
data.fit <- cbind(data.fact,data.spp[,i])
colnames(data.fit) <- c("Year","Site","Site.Type","Point.Round","obs")

data.new <- pivot_wider(data.fit,names_from=Point.Round,values_from=obs)
data.new <- as.data.frame(data.new)
data.oframe <- data.new[,c(4:35)]

site.Covars <- data.frame(Year=data.new$Year,Site=data.new$Site,Site.Type=data.new$Site.Type)
obs.Covars <- 


#UMF <- unmarkedFrameOccu(data.new[,c(4:35)])
unm_cov <- unmarkedFrameOccu(y=data.oframe,siteCovs=site.Covars) 




occu(formula = ~Site + Year ~ Site.Type, data = unm_cov)


}
