library(here)
library(tidyr)
library(lme4)
library(stringr)


#########################################################################
#                                                                       #
#                              DATA SET-UP                              #
#                                                                       #
#########################################################################

#read in the data 
bird.data <- read.csv(here("data","Bird_Data_SC_Stats.csv")) 

#clean some things up 
bird.data$Site[which(bird.data$Site == "Issaquah highlands")] <- "Issaquah Highlands"
bird.data$Site.Type[which(bird.data$Site.Type == "reserve")] <- "Reserve"
bird.data$Point.Type[which(bird.data$Point.Type == "Forest ")] <- "Forest"
bird.data$Initials.Site[intersect(which(bird.data$Site == "Cougar Mountain Park"),which(bird.data$Initials.Site == "LF"))] <- "CM"

#create a new clean data frame with no duplicate columns 
bird.clean <- data.frame(bird.data$Year, bird.data$Initials.Site, bird.data$Site.Type, bird.data$Point, bird.data$Point.Type, bird.data$Round, bird.data$Species, bird.data$Total)
colnames(bird.clean) <- c("Year","Site","Site.Type","Point","Point.Type","Round","Species","Total")

################################################################
#explore the duplicated rows in the dataset 
#create a dataset that contains all the duplicates 
# bird.clean.test <- bird.clean
# total.vect <- 0 
# blank.mat <- rep(NA,3000)
# bird.clean.con <- bird.clean.con.noTotal <- rep(NA,nrow(bird.clean.test)) 
# start <- 1 
# for(i in 1:length(bird.clean.con)){
#   bird.clean.con.noTotal[i] <- paste(bird.clean.test[i,1],bird.clean.test[i,2],bird.clean.test[i,3],bird.clean.test[i,4],bird.clean.test[i,5],bird.clean.test[i,6],bird.clean.test[i,7],sep = "-") 
#   bird.clean.con[i] <- paste(bird.clean.test[i,1],bird.clean.test[i,2],bird.clean.test[i,3],bird.clean.test[i,4],bird.clean.test[i,5],bird.clean.test[i,6],bird.clean.test[i,7],bird.clean.test[i,8],sep = "-") 
# }
# for(j in 1:length(bird.clean.con)){
#   vect <- which(bird.clean.con.noTotal == bird.clean.con.noTotal[j])
#   for(h in 1:length(vect)){
#     if(length(vect)>1 && is.element(vect[h],total.vect)==FALSE){
#         blank.mat[(start+h-1)] <- bird.clean.con[vect[h]]
#       total.vect <- c(total.vect,vect[h])
#     }
#   } 
#   start <- min(which(is.na(blank.mat==TRUE)))
# }
# total.vect <- total.vect[-c(1)]
# repeat.data <- matrix(NA,nrow=length(blank.mat),ncol=9)
# for(i in 1:length(blank.mat)){
#     for(g in 1:ncol(repeat.data)){
#       repeat.data[i,g] <- str_split(blank.mat[i], "-")[[1]][g]
#     }
#   repeat.data[i,9] <- total.vect[i]
# }
# 
# #write out repeat data 
# colnames(repeat.data) <- c(colnames(bird.clean),"line_from_data")
# write.csv(repeat.data,file= here("data","repeated_data_output.csv"),row.names = FALSE)
################################################################

#we think we can just take the sum because things weren't summed before data entry   
fix.dups <- aggregate(Total ~ Year + Site + Site.Type + Point + Point.Type + Round + Species,data = bird.clean,FUN=sum)

#create wide form 
bird.wide <- pivot_wider(data = fix.dups, id_cols = c("Year","Site","Site.Type","Point","Point.Type","Round"), names_from = "Species", values_from = "Total")
#convert to a data frame 
bird.wide <- as.data.frame(bird.wide)

#replace NA with 0 
bird.wide[is.na(bird.wide)] <- 0

################################################################
#look at the overall counts across all species
# bird.all <- c(bird.wide[,7],bird.wide[,8],bird.wide[,9],bird.wide[,10],bird.wide[,11]
#               ,bird.wide[,12],bird.wide[,13],bird.wide[,14],bird.wide[,15],bird.wide[,16]
#               ,bird.wide[,17],bird.wide[,18],bird.wide[,19],bird.wide[,20],bird.wide[,21]
#               ,bird.wide[,22],bird.wide[,23],bird.wide[,24],bird.wide[,25],bird.wide[,26]
#               ,bird.wide[,27],bird.wide[,28],bird.wide[,29],bird.wide[,30],bird.wide[,31]
#               ,bird.wide[,32],bird.wide[,33],bird.wide[,34],bird.wide[,35],bird.wide[,36]
#               ,bird.wide[,37],bird.wide[,38],bird.wide[,39],bird.wide[,40],bird.wide[,41]
#               ,bird.wide[,42],bird.wide[,43],bird.wide[,44],bird.wide[,45],bird.wide[,46]
#               ,bird.wide[,47],bird.wide[,48],bird.wide[,49],bird.wide[,50],bird.wide[,51]
#               ,bird.wide[,52],bird.wide[,53],bird.wide[,54],bird.wide[,55],bird.wide[,56]
#               ,bird.wide[,57],bird.wide[,58],bird.wide[,59],bird.wide[,60],bird.wide[,61]
#               ,bird.wide[,62],bird.wide[,63])
# table(bird.all)
################################################################

#seems reasonable to treat the data as binary (very few counts are >1)
bird.binary <- bird.wide
for(i in 1:nrow(bird.wide)){
  for(j in 7:63){
    if(bird.wide[i,j]> 1){bird.binary[i,j] <- 1} 
  }
}

#now remove everything but CD/PCD sites 
bird.binary.noCorrRes <- bird.binary[-c(which(bird.binary$Site.Type == "Correlational"),which(bird.binary$Site.Type == "Reserve")),]

#create year variable for modeling 
Year.new <- (bird.binary.noCorrRes$Year - mean(bird.binary.noCorrRes$Year))/sd(bird.binary.noCorrRes$Year)
Year.new2 <- Year.new^2

#put it all into a new dataframe 
bird.binary.noCorrRes <- data.frame(bird.binary.noCorrRes[,c(1)],Year.new,Year.new2,bird.binary.noCorrRes[,c(2:6)],bird.binary.noCorrRes[,c(7:63)])
colnames(bird.binary.noCorrRes) <- c("Year",colnames(bird.binary.noCorrRes[c(2:65)]))

#we start with 57 species, but 53 species are in the guild list from dissertation 
#the guild list excludes HAFL, MODO, RECR, TOSO, so remove these species
bird.binary.noCorrRes <- subset(bird.binary.noCorrRes, select = -c(HAFL,MODO,RECR,TOSO))

#########################################################################
#                                                                       #
#                           SET UP FOR ANALYSIS                         #
#                                                                       #
#########################################################################

store.AIC <- matrix(NA,nrow = 8,ncol = 53)
colnames(store.AIC) <- colnames(bird.binary.noCorrRes)[c(9:ncol(bird.binary.noCorrRes))]
store.AIC <- as.data.frame(store.AIC)

cntrl <- glmerControl(optimizer = "bobyqa", tol = 1e-4, optCtrl=list(maxfun=100000))

#########################################################################
#                                                                       #
#                            SPECIES MODELS                             #
#  CHECK ON THE INDEX YOU WANT TO RUN - NEED TO ADJUST FOR POOR MODELS  #
#                                                                       #
#########################################################################

#choose the input species 
for(i in 26:38){ #run this from 1 to 53 

bird.binary.noCorrRes$this.input <- bird.binary.noCorrRes[,(8+i)]
analysis <- colnames(bird.binary.noCorrRes[8+i])

# #models   
m1.mod <- glmer(this.input ~ 1 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m2.mod <- glmer(this.input ~ Site.Type + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m3.mod <- glmer(this.input ~ Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m4.mod <- glmer(this.input ~ Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m5.mod <- glmer(this.input ~ Site.Type + Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m6.mod <- glmer(this.input ~ Site.Type + Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m7.mod <- glmer(this.input ~ Site.Type + Year.new + Site.Type*Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m8.mod <- glmer(this.input ~ Site.Type + Year.new + Year.new2 + Site.Type*Year.new + Site.Type*Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)

#get rid of the input object 
bird.binary.noCorrRes <- subset(bird.binary.noCorrRes, select = -c(this.input))

#store results - AIC value  
store.AIC[1,i] <- as.numeric(AIC(m1.mod))
store.AIC[2,i] <- as.numeric(AIC(m2.mod))
store.AIC[3,i] <- as.numeric(AIC(m3.mod))
store.AIC[4,i] <- as.numeric(AIC(m4.mod))
store.AIC[5,i] <- as.numeric(AIC(m5.mod))
store.AIC[6,i] <- as.numeric(AIC(m6.mod))
store.AIC[7,i] <- as.numeric(AIC(m7.mod))
store.AIC[8,i] <- as.numeric(AIC(m8.mod))

} 

#Create NA for all the models that weren't estimable
store.AIC[c(4,8),10] <- NA        #i <- 10 (BRBL - only 8 detections)
store.AIC[8,12] <- NA             #i <- 12 (BTPI - only 20 detections)
store.AIC[8,15] <- NA             #i <- 15 (CAVI - only 38 detections)
store.AIC[c(1:8),19] <- NA        #i <- 19 (DOWO - only 26 detections)
store.AIC[c(2,5,6,7,8),25] <- NA  #i <- 25 (HOSP - only 75 detections)
store.AIC[c(2,5,6,7,8),39] <- NA  #i <- 39 (SAVS - only 27 detections)
store.AIC[c(1:8),44] <- NA        #i <- 44 (TOWA - only 28 detections)
store.AIC[c(1:8),45] <- NA        #i <- 45 (VASW - only 22 detections)
store.AIC[8,50] <- NA             #i <- 50 (WEWP - only 13 detections)   

#get the best model for each species and store results 
best.mods.out <- matrix(NA,nrow=25,ncol=ncol(store.AIC))
for(i in 1:ncol(store.AIC)){
  if(length(which(is.na(store.AIC[,i]) == TRUE))<8){
    best.mods.out[1,i] <- which.min(store.AIC[,i])
  }
  else(best.mods.out[1,i] <- 0)
}  

colnames(best.mods.out) <- colnames(store.AIC)

#########################################################################
#                                                                       #
#                            GUILDS MODELS                              #
#                                                                       #
#########################################################################

store.AIC.guild <- matrix(NA,nrow = 8,ncol = 4)

avoider.list <- c(which(colnames(store.AIC)=="BRCR"),which(colnames(store.AIC)=="BTYW"),which(colnames(store.AIC)=="CBCH"),which(colnames(store.AIC)=="DOWO"),which(colnames(store.AIC)=="GCKI"),which(colnames(store.AIC)=="HAWO"),
                  which(colnames(store.AIC)=="HUVI"),which(colnames(store.AIC)=="PAWR"),which(colnames(store.AIC)=="PSFL"),which(colnames(store.AIC)=="PUFI"),which(colnames(store.AIC)=="RBNU"),which(colnames(store.AIC)=="RBSA"),
                  which(colnames(store.AIC)=="RCKI"),which(colnames(store.AIC)=="STJA"),which(colnames(store.AIC)=="SWTH"),which(colnames(store.AIC)=="TOWA"),which(colnames(store.AIC)=="WETA"),which(colnames(store.AIC)=="WIWA"))

adaptor.list <- c(which(colnames(store.AIC)=="AMGO"),which(colnames(store.AIC)=="AMRO"),which(colnames(store.AIC)=="BCCH"),which(colnames(store.AIC)=="BEWR"),which(colnames(store.AIC)=="BHGR"),which(colnames(store.AIC)=="BRBL"),
                  which(colnames(store.AIC)=="BTPI"),which(colnames(store.AIC)=="BUSH"),which(colnames(store.AIC)=="CAVI"),which(colnames(store.AIC)=="CEDW"),which(colnames(store.AIC)=="DEJU"),which(colnames(store.AIC)=="EVGR"),
                  which(colnames(store.AIC)=="NOFL"),which(colnames(store.AIC)=="OCWA"),which(colnames(store.AIC)=="OSFL"),which(colnames(store.AIC)=="PISI"),which(colnames(store.AIC)=="SAVS"),which(colnames(store.AIC)=="SOSP"),
                  which(colnames(store.AIC)=="SPTO"),which(colnames(store.AIC)=="VASW"),which(colnames(store.AIC)=="VGSW"),which(colnames(store.AIC)=="WAVI"),which(colnames(store.AIC)=="WCSP"),which(colnames(store.AIC)=="WEWP"),
                  which(colnames(store.AIC)=="WIFL"),which(colnames(store.AIC)=="YRWA"))

exploiter.list <- c(which(colnames(store.AIC)=="AMCR"),which(colnames(store.AIC)=="ANHU"),which(colnames(store.AIC)=="BARS"),which(colnames(store.AIC)=="BHCO"),which(colnames(store.AIC)=="EUST"),which(colnames(store.AIC)=="HOFI"),
                    which(colnames(store.AIC)=="HOSP"),which(colnames(store.AIC)=="ROPI"),which(colnames(store.AIC)=="RUHU"))

bird.binary.noCorrRes$avoider <- rowSums(bird.binary.noCorrRes[avoider.list+8])
bird.binary.noCorrRes$adaptor <- rowSums(bird.binary.noCorrRes[adaptor.list+8])
bird.binary.noCorrRes$exploiter <- rowSums(bird.binary.noCorrRes[exploiter.list+8])
bird.binary.noCorrRes$community <- bird.binary.noCorrRes$avoider + bird.binary.noCorrRes$adaptor + bird.binary.noCorrRes$exploiter

bird.binary.noCorrRes$samp.avoider <- rep(18,nrow(bird.binary.noCorrRes))
bird.binary.noCorrRes$samp.adaptor <- rep(26,nrow(bird.binary.noCorrRes))
bird.binary.noCorrRes$samp.exploiter <- rep(9,nrow(bird.binary.noCorrRes))
bird.binary.noCorrRes$samp.community <- rep(53,nrow(bird.binary.noCorrRes))

#models   
m1.avoider <- glmer(cbind(avoider,samp.avoider-avoider) ~ 1 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m2.avoider <- glmer(cbind(avoider,samp.avoider-avoider) ~ Site.Type + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m3.avoider <- glmer(cbind(avoider,samp.avoider-avoider) ~ Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m4.avoider <- glmer(cbind(avoider,samp.avoider-avoider) ~ Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m5.avoider <- glmer(cbind(avoider,samp.avoider-avoider) ~ Site.Type + Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m6.avoider <- glmer(cbind(avoider,samp.avoider-avoider) ~ Site.Type + Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m7.avoider <- glmer(cbind(avoider,samp.avoider-avoider) ~ Site.Type + Year.new + Site.Type*Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m8.avoider <- glmer(cbind(avoider,samp.avoider-avoider) ~ Site.Type + Year.new + Year.new2 + Site.Type*Year.new + Site.Type*Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)

m1.adaptor <- glmer(cbind(adaptor,samp.adaptor-adaptor) ~ 1 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m2.adaptor <- glmer(cbind(adaptor,samp.adaptor-adaptor) ~ Site.Type + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m3.adaptor <- glmer(cbind(adaptor,samp.adaptor-adaptor) ~ Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m4.adaptor <- glmer(cbind(adaptor,samp.adaptor-adaptor) ~ Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m5.adaptor <- glmer(cbind(adaptor,samp.adaptor-adaptor) ~ Site.Type + Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m6.adaptor <- glmer(cbind(adaptor,samp.adaptor-adaptor) ~ Site.Type + Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m7.adaptor <- glmer(cbind(adaptor,samp.adaptor-adaptor) ~ Site.Type + Year.new + Site.Type*Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m8.adaptor <- glmer(cbind(adaptor,samp.adaptor-adaptor) ~ Site.Type + Year.new + Year.new2 + Site.Type*Year.new + Site.Type*Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)

m1.exploiter <- glmer(cbind(exploiter,samp.exploiter-exploiter) ~ 1 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m2.exploiter <- glmer(cbind(exploiter,samp.exploiter-exploiter) ~ Site.Type + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m3.exploiter <- glmer(cbind(exploiter,samp.exploiter-exploiter) ~ Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m4.exploiter <- glmer(cbind(exploiter,samp.exploiter-exploiter) ~ Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m5.exploiter <- glmer(cbind(exploiter,samp.exploiter-exploiter) ~ Site.Type + Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m6.exploiter <- glmer(cbind(exploiter,samp.exploiter-exploiter) ~ Site.Type + Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m7.exploiter <- glmer(cbind(exploiter,samp.exploiter-exploiter) ~ Site.Type + Year.new + Site.Type*Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m8.exploiter <- glmer(cbind(exploiter,samp.exploiter-exploiter) ~ Site.Type + Year.new + Year.new2 + Site.Type*Year.new + Site.Type*Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)

m1.community <- glmer(cbind(community,samp.community-community) ~ 1 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m2.community <- glmer(cbind(community,samp.community-community) ~ Site.Type + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m3.community <- glmer(cbind(community,samp.community-community) ~ Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m4.community <- glmer(cbind(community,samp.community-community) ~ Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m5.community <- glmer(cbind(community,samp.community-community) ~ Site.Type + Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m6.community <- glmer(cbind(community,samp.community-community) ~ Site.Type + Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m7.community <- glmer(cbind(community,samp.community-community) ~ Site.Type + Year.new + Site.Type*Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
m8.community <- glmer(cbind(community,samp.community-community) ~ Site.Type + Year.new + Year.new2 + Site.Type*Year.new + Site.Type*Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)

#save results 
store.AIC.guild[1,1] <- AIC(m1.avoider)
store.AIC.guild[2,1] <- AIC(m2.avoider)
store.AIC.guild[3,1] <- AIC(m3.avoider)
store.AIC.guild[4,1] <- AIC(m4.avoider)
store.AIC.guild[5,1] <- AIC(m5.avoider)
store.AIC.guild[6,1] <- AIC(m6.avoider)
store.AIC.guild[7,1] <- AIC(m7.avoider)
store.AIC.guild[8,1] <- AIC(m8.avoider)

store.AIC.guild[1,2] <- AIC(m1.adaptor)
store.AIC.guild[2,2] <- AIC(m2.adaptor)
store.AIC.guild[3,2] <- AIC(m3.adaptor)
store.AIC.guild[4,2] <- AIC(m4.adaptor)
store.AIC.guild[5,2] <- AIC(m5.adaptor)
store.AIC.guild[6,2] <- AIC(m6.adaptor)
store.AIC.guild[7,2] <- AIC(m7.adaptor)
store.AIC.guild[8,2] <- AIC(m8.adaptor)

store.AIC.guild[1,3] <- AIC(m1.exploiter)
store.AIC.guild[2,3] <- AIC(m2.exploiter)
store.AIC.guild[3,3] <- AIC(m3.exploiter)
store.AIC.guild[4,3] <- AIC(m4.exploiter)
store.AIC.guild[5,3] <- AIC(m5.exploiter)
store.AIC.guild[6,3] <- AIC(m6.exploiter)
store.AIC.guild[7,3] <- AIC(m7.exploiter)
store.AIC.guild[8,3] <- AIC(m8.exploiter)

store.AIC.guild[1,4] <- AIC(m1.community)
store.AIC.guild[2,4] <- AIC(m2.community)
store.AIC.guild[3,4] <- AIC(m3.community)
store.AIC.guild[4,4] <- AIC(m4.community)
store.AIC.guild[5,4] <- AIC(m5.community)
store.AIC.guild[6,4] <- AIC(m6.community)
store.AIC.guild[7,4] <- AIC(m7.community)
store.AIC.guild[8,4] <- AIC(m8.community)

#get the best model for each guild and store results 
best.mods.out.guild <- matrix(NA,nrow=25,ncol=4)
for(i in 1:ncol(store.AIC.guild)){
  if(length(which(is.na(store.AIC.guild[,i]) == TRUE))<8){
    best.mods.out.guild[1,i] <- which.min(store.AIC.guild[,i])
  }
  else(best.mods.out.guild[1,i] <- 0)
}  

colnames(store.AIC.guild) <- colnames(best.mods.out.guild) <- c("avoider","adaptor","exploiter","community")

#save a workspace with the following 
#adaptor.list
#avoider.list
#best.mods.out
#best.mods.out.guild
#bird.binary.noCorrRes
#exploiter.list
#store.AIC
#store.AIC.guild

##load(here("scripts","allSpp_AIC.RData"))

#########################################################################
#                                                                       #
#                     SET UP FOR BOOTSTRAPPING                          #
#                                                                       #
#########################################################################

#viable models for bootstrapping - ELIMINATE SPECIES WITH NO VIABLE MODEL OR SAMPLE SIZE < 20 
all.nonviable <- c(which(best.mods.out[1,]==0),which(colSums(bird.binary.noCorrRes[,c(9:61)])<20))
non.viable.ind <- rep(NA,length(all.nonviable))
for(i in 1:length(all.nonviable)){
  non.viable.ind[i] <- which(colnames(bird.binary.noCorrRes[,c(9:61)])==names(all.nonviable)[i])
}
viable.ind <- setdiff(c(1:53),non.viable.ind)

##prepare for bootstrapped predictions 
Site.Type <- c(rep("Prepost_CD",12),rep("Prepost_PCD",12))
Year.new <- rep(sort(unique(bird.binary.noCorrRes$Year.new)),2)
Year.new2 <- Year.new^2
newdata <- data.frame(Site.Type,Year.new,Year.new2)
levs <- nrow(newdata)

#bootstrapped samples 
boot.samps <- 1000

#place to store results 
pv <- array(NA,dim=c(ncol(store.AIC),boot.samps,levs))
dimnames(pv)[[1]] <- colnames(store.AIC)
pv.guild <- array(NA,dim=c(4,boot.samps,levs))
dimnames(pv.guild)[[1]] <- colnames(store.AIC.guild)

cntrl <- glmerControl(optimizer = "bobyqa", tol = 1e-4, optCtrl=list(maxfun=100000))

#########################################################################
#                                                                       #
#                     BOOTSTRAP SPECIES                                 #
#                                                                       #
#########################################################################

###For the best model for each response variable, we will have to set up and run the code below here (with the correct model structure) 
for(i in viable.ind){

  bird.binary.noCorrRes$this.input <- bird.binary.noCorrRes[,(8+i)]
    
  if(best.mods.out[1,i]==1){
    m.best <- glmer(this.input ~ 1 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
  }else if(best.mods.out[1,i]==2){
    m.best <- glmer(this.input ~ Site.Type + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
  }else if(best.mods.out[1,i]==3){
    m.best <- glmer(this.input ~ Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
  }else if(best.mods.out[1,i]==4){
    m.best <- glmer(this.input ~ Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
  }else if(best.mods.out[1,i]==5){
    m.best <- glmer(this.input ~ Site.Type + Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
  }else if(best.mods.out[1,i]==6){
    m.best <- glmer(this.input ~ Site.Type + Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
  }else if(best.mods.out[1,i]==7){
    m.best <- glmer(this.input ~ Site.Type + Year.new + Site.Type*Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
  }else if(best.mods.out[1,i]==8){
    m.best <- glmer(this.input ~ Site.Type + Year.new + Year.new2 + Site.Type*Year.new + Site.Type*Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
  }

    #store the parameter estimates for reporting 
    to.store <- summary(m.best)$coef[,1]
    fin.row <- 1+length(to.store)
    best.mods.out[c(2:fin.row),i] <- to.store
    to.store <- summary(m.best)$coef[,2]
    start.row <- 1+length(to.store)+1
    fin.row <- start.row + length(to.store)-1
    best.mods.out[c(start.row:fin.row),i] <- to.store
   
    ###Bootstrap coefficients 
    for(j in 1:boot.samps){
      y <- unlist(simulate(m.best))
      if(best.mods.out[1,i]==1){
        b.mod <- glmer(y ~ 1 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
      }else if(best.mods.out[1,i]==2){
        b.mod <- glmer(y ~ Site.Type + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
      }else if(best.mods.out[1,i]==3){
        b.mod <- glmer(y ~ Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
      }else if(best.mods.out[1,i]==4){
        b.mod <- glmer(y ~ Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
      }else if(best.mods.out[1,i]==5){
        b.mod <- glmer(y ~ Site.Type + Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
      }else if(best.mods.out[1,i]==6){
        b.mod <- glmer(y ~ Site.Type + Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
      }else if(best.mods.out[1,i]==7){
        b.mod <- glmer(y ~ Site.Type + Year.new + Site.Type*Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
      }else if(best.mods.out[1,i]==8){
        b.mod <- glmer(y ~ Site.Type + Year.new + Year.new2 + Site.Type*Year.new + Site.Type*Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
      }

  
    if(is.null(summary(b.mod)$optinfo$conv$lme4$messages)==TRUE){
      RE.sd <- as.data.frame(VarCorr(b.mod))$sdcor[1]
      pv[i,j,] <- (1/(1+exp(-predict(b.mod, re.form = ~0, newdata) + rnorm(1,sd=RE.sd))))
    }else if(is.null(summary(b.mod)$optinfo$conv$lme4$messages)==FALSE){
      pv[i,j,] <- rep(NA,levs)
    }
  }
  print(i)
}
  
#########################################################################
#                                                                       #
#                     BOOTSTRAP GUILDS                                  #
#                                                                       #
#########################################################################

for(i in 1:4){
  
  bird.binary.noCorrRes$this.input <- bird.binary.noCorrRes[,(61+i)]
  bird.binary.noCorrRes$this.input2 <- bird.binary.noCorrRes[,(65+i)] - bird.binary.noCorrRes[,(61+i)]
  
  #same best model in all cases   
  m.best <- glmer(cbind(this.input,this.input2) ~ Site.Type + Year.new + Year.new2 + Site.Type*Year.new + Site.Type*Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)

  #store the parameter estimates for reporting 
  to.store <- summary(m.best)$coef[,1]
  fin.row <- 1+length(to.store)
  best.mods.out.guild[c(2:fin.row),i] <- to.store
  to.store <- summary(m.best)$coef[,2]
  start.row <- 1+length(to.store)+1
  fin.row <- start.row + length(to.store)-1
  best.mods.out.guild[c(start.row:fin.row),i] <- to.store
  
  ###Bootstrap coefficients 
  for(j in 1:boot.samps){
    all <- simulate(m.best)
    y <- all$sim_1[,1]
    z <- all$sim_1[,2]
    if(best.mods.out.guild[1,i]==1){
      b.mod <- glmer(cbind(y,z) ~ 1 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
    }else if(best.mods.out.guild[1,i]==2){
      b.mod <- glmer(cbind(y,z) ~ Site.Type + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
    }else if(best.mods.out.guild[1,i]==3){
      b.mod <- glmer(cbind(y,z) ~ Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
    }else if(best.mods.out.guild[1,i]==4){
      b.mod <- glmer(cbind(y,z) ~ Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
    }else if(best.mods.out.guild[1,i]==5){
      b.mod <- glmer(cbind(y,z) ~ Site.Type + Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
    }else if(best.mods.out.guild[1,i]==6){
      b.mod <- glmer(cbind(y,z) ~ Site.Type + Year.new + Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
    }else if(best.mods.out.guild[1,i]==7){
      b.mod <- glmer(cbind(y,z) ~ Site.Type + Year.new + Site.Type*Year.new + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
    }else if(best.mods.out.guild[1,i]==8){
      b.mod <- glmer(cbind(y,z) ~ Site.Type + Year.new + Year.new2 + Site.Type*Year.new + Site.Type*Year.new2 + (1|Round:Site:Point), bird.binary.noCorrRes, family = binomial,nAGQ = 20, control = cntrl)
    }
    
    
    if(is.null(summary(b.mod)$optinfo$conv$lme4$messages)==TRUE){
      RE.sd <- as.data.frame(VarCorr(b.mod))$sdcor[1]
      pv.guild[i,j,] <- (1/(1+exp(-predict(b.mod, re.form = ~0, newdata) + rnorm(1,sd=RE.sd))))
    }else if(is.null(summary(b.mod)$optinfo$conv$lme4$messages)==FALSE){
      pv.guild[i,j,] <- rep(NA,levs)
    }
  }
  print(i)
}

#here, keep an environment with everything above plus 
#adaptor.list
#avoider.list
#best.mods.out
#best.mods.out.guild
#bird.binary.noCorrRes
#exploiter.list
#pv
#pv.guild 
#store.AIC
#store.AIC.guild

#load the data up through here 
#load(here("scripts","all_Boots.RData"))

#########################################################################
#                                                                       #
#                         PRINT RESULTS                                 #
#                                                                       #
#########################################################################

round(apply(store.AIC.guild,2,fun <- function(x){x - min(x,na.rm=TRUE)}),dig=2)

round(apply(store.AIC,2,fun <- function(x){x - min(x,na.rm=TRUE)}),dig=2)

round(best.mods.out.guild,dig=2)

round(best.mods.out,dig=2)

print(which.bad) ## those species with bootstrap failure 