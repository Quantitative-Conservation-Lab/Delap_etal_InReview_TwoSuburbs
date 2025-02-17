#read in results 

model1 <- as.matrix(read.csv("results/occ_model1_results.csv"))[,-c(1)]
model2 <- as.matrix(read.csv("results/occ_model2_results.csv"))[,-c(1)]
model3 <- as.matrix(read.csv("results/occ_model3_results.csv"))[,-c(1)]
model4 <- as.matrix(read.csv("results/occ_model4_results.csv"))[,-c(1)]
model5 <- as.matrix(read.csv("results/occ_model5_results.csv"))[,-c(1)]
model6 <- as.matrix(read.csv("results/occ_model6_results.csv"))[,-c(1)]
model7 <- as.matrix(read.csv("results/occ_model7_results.csv"))[,-c(1)]
model8 <- as.matrix(read.csv("results/occ_model8_results.csv"))[,-c(1)]

#read in data - where is sample size perhaps too small? 
data1 <- read.csv("data/bird.binary.noCorrRes.csv")
data.birds <- data1[,c(9:61)]
colnames(data.birds) <- colnames(data1)[9:61]
totobs <- colSums(data.birds)
data.poor <- which(totobs < 30)

#guilds 
avoider.list <- c(which(colnames(data.birds)=="BRCR"),which(colnames(data.birds)=="BTYW"),which(colnames(data.birds)=="CBCH"),which(colnames(data.birds)=="DOWO"),which(colnames(data.birds)=="GCKI"),which(colnames(data.birds)=="HAWO"),
                  which(colnames(data.birds)=="HUVI"),which(colnames(data.birds)=="PAWR"),which(colnames(data.birds)=="PSFL"),which(colnames(data.birds)=="PUFI"),which(colnames(data.birds)=="RBNU"),which(colnames(data.birds)=="RBSA"),
                  which(colnames(data.birds)=="RCKI"),which(colnames(data.birds)=="STJA"),which(colnames(data.birds)=="SWTH"),which(colnames(data.birds)=="TOWA"),which(colnames(data.birds)=="WETA"),which(colnames(data.birds)=="WIWA"))

adapter.list <- c(which(colnames(data.birds)=="AMGO"),which(colnames(data.birds)=="AMRO"),which(colnames(data.birds)=="BCCH"),which(colnames(data.birds)=="BEWR"),which(colnames(data.birds)=="BHGR"),which(colnames(data.birds)=="BRBL"),
                  which(colnames(data.birds)=="BTPI"),which(colnames(data.birds)=="BUSH"),which(colnames(data.birds)=="CAVI"),which(colnames(data.birds)=="CEDW"),which(colnames(data.birds)=="DEJU"),which(colnames(data.birds)=="EVGR"),
                  which(colnames(data.birds)=="NOFL"),which(colnames(data.birds)=="OCWA"),which(colnames(data.birds)=="OSFL"),which(colnames(data.birds)=="PISI"),which(colnames(data.birds)=="SAVS"),which(colnames(data.birds)=="SOSP"),
                  which(colnames(data.birds)=="SPTO"),which(colnames(data.birds)=="VASW"),which(colnames(data.birds)=="VGSW"),which(colnames(data.birds)=="WAVI"),which(colnames(data.birds)=="WCSP"),which(colnames(data.birds)=="WEWP"),
                  which(colnames(data.birds)=="WIFL"),which(colnames(data.birds)=="YRWA"))

exploiter.list <- c(which(colnames(data.birds)=="AMCR"),which(colnames(data.birds)=="ANHU"),which(colnames(data.birds)=="BARS"),which(colnames(data.birds)=="BHCO"),which(colnames(data.birds)=="EUST"),which(colnames(data.birds)=="HOFI"),
                    which(colnames(data.birds)=="HOSP"),which(colnames(data.birds)=="ROPI"),which(colnames(data.birds)=="RUHU"))

#check R-hat 
max(model1[,"R.hat"])
max(model2[,"R.hat"])
max(model3[,"R.hat"])
max(model4[,"R.hat"])
max(model5[,"R.hat"])
max(model6[,"R.hat"])
max(model7[,"R.hat"])
max(model8[,"R.hat"])

avoider.list <- avoider.list[which(is.element(avoider.list,data.poor)==FALSE)]
adapter.list <- adapter.list[which(is.element(adapter.list,data.poor)==FALSE)]
exploiter.list <- exploiter.list[which(is.element(exploiter.list,data.poor)==FALSE)]

#best model 
                                  
mod.selection <- cbind(model1[,"WAIC"],model2[,"WAIC"],model3[,"WAIC"],model4[,"WAIC"],model5[,"WAIC"],model6[,"WAIC"],model7[,"WAIC"],model8[,"WAIC"])

delta.mod.selection <- mod.selection 

for(i in 1:nrow(mod.selection)){
  delta.mod.selection[i,] <- mod.selection[i,] - min(mod.selection[i,])
}

avoider.selection <- delta.mod.selection[avoider.list,]
adapter.selection <- delta.mod.selection[adapter.list,]
exploiter.selection <- delta.mod.selection[exploiter.list,]

#write a function to get the inference model for each guild 
#this is the best model including site type 
inf.model.fxn <- function(x){
inf.model.out <- rep(NA,nrow(x))
for(i in 1:nrow(x)){
  ord <- order(x[i,])
  for(j in 1:length(ord)){
    if(is.element(ord[j],c(2,5,6,7,8))){
      inf.model.out[i] <- ord[j];      
      break      
    }
  } 
}
return(inf.model.out)
}
inf.model.avoider <- inf.model.fxn(avoider.selection)
inf.model.adapter <- inf.model.fxn(adapter.selection)
inf.model.exploiter <- inf.model.fxn(exploiter.selection)

#get all predictions, including mean, SD, LCI, and UCI for species and models 
preds <- array(NA, dim = c(53,96,8))
preds[,,1] <- model1[,1:96]
preds[,,2] <- model2[,1:96]
preds[,,3] <- model3[,1:96]
preds[,,4] <- model4[,1:96]
preds[,,5] <- model5[,1:96]
preds[,,6] <- model6[,1:96]
preds[,,7] <- model7[,1:96]
preds[,,8] <- model8[,1:96]

#best model - this is the model that is best for each species and contains a site effect 

#get the predictions from the best model 
preds.best.model <- function(preds,best.model,list){
  preds.best <- array(NA,dim = c(length(list),24,4))
  for(i in 1:length(list)){
    preds.best[i,c(1:24),1] <- preds[list[i],c(1:24),best.model[i]]
    preds.best[i,c(1:24),2] <- preds[list[i],c(25:48),best.model[i]]
    preds.best[i,c(1:24),3] <- preds[list[i],c(49:72),best.model[i]]
    preds.best[i,c(1:24),4] <- preds[list[i],c(73:96),best.model[i]]
  }
return(preds.best)  
}
preds.avoider <- preds.best.model(preds,inf.model.avoider,avoider.list)
preds.adapter <- preds.best.model(preds,inf.model.adapter,adapter.list)
preds.exploiter <- preds.best.model(preds,inf.model.exploiter,exploiter.list)

#params by model 
#Model1: "int.p","int.psi"
#Model2: "int.p","int.psi","beta.site.type[2]"
#Model3: "int.p","int.psi","beta.year"
#Model4: "int.p","int.psi","beta.year","beta.year2"
#Model5: "int.p","int.psi","beta.site.type[2]","beta.year"
#Model6: "int.p","int.psi","beta.site.type[2]","beta.year","beta.year2"
#Model7: "int.p","int.psi","beta.site.type[2]","beta.year","beta.site.type.year[2]"
#Model8: "int.p","int.psi","beta.site.type[2]","beta.year","beta.year2","beta.site.type.year[2]","beta.site.type.year2[2]"

coef <- array(NA, dim = c(53,28,8))
coef[,1:length(97:(ncol(model1)-2)),1] <- model1[,(97:(ncol(model1)-2))] 
coef[,1:length(97:(ncol(model2)-2)),2] <- model2[,(97:(ncol(model2)-2))] 
coef[,1:length(97:(ncol(model3)-2)),3] <- model3[,(97:(ncol(model3)-2))] 
coef[,1:length(97:(ncol(model4)-2)),4] <- model4[,(97:(ncol(model4)-2))] 
coef[,1:length(97:(ncol(model5)-2)),5] <- model5[,(97:(ncol(model5)-2))] 
coef[,1:length(97:(ncol(model6)-2)),6] <- model6[,(97:(ncol(model6)-2))] 
coef[,1:length(97:(ncol(model7)-2)),7] <- model7[,(97:(ncol(model7)-2))] 
coef[,1:length(97:(ncol(model8)-2)),8] <- model8[,(97:(ncol(model8)-2))] 

coef.best.model <- function(coef,best.model,list){
  coef.best <- array(NA,dim = c(length(list),7,4))
  for(i in 1:length(list)){
    if(best.model[i] == 2){
      coef.best[i,c(1:3),1] <- coef[list[i],c(1:3),best.model[i]]
      coef.best[i,c(1:3),2] <- coef[list[i],c(4:6),best.model[i]]
      coef.best[i,c(1:3),3] <- coef[list[i],c(7:9),best.model[i]]
      coef.best[i,c(1:3),4] <- coef[list[i],c(10:12),best.model[i]]
    }else if(best.model[i] == 5){
      coef.best[i,c(1:4),1] <- coef[list[i],c(1:4),best.model[i]]
      coef.best[i,c(1:4),2] <- coef[list[i],c(5:8),best.model[i]]
      coef.best[i,c(1:4),3] <- coef[list[i],c(9:12),best.model[i]]
      coef.best[i,c(1:4),4] <- coef[list[i],c(13:16),best.model[i]]
    }else if(best.model[i] == 6){
      coef.best[i,c(1:5),1] <- coef[list[i],c(1:5),best.model[i]]
      coef.best[i,c(1:5),2] <- coef[list[i],c(6:10),best.model[i]]
      coef.best[i,c(1:5),3] <- coef[list[i],c(11:15),best.model[i]]
      coef.best[i,c(1:5),4] <- coef[list[i],c(16:20),best.model[i]]
    }else if(best.model[i] == 7){
      coef.best[i,c(1:5),1] <- coef[list[i],c(1:5),best.model[i]]
      coef.best[i,c(1:5),2] <- coef[list[i],c(6:10),best.model[i]]
      coef.best[i,c(1:5),3] <- coef[list[i],c(11:15),best.model[i]]
      coef.best[i,c(1:5),4] <- coef[list[i],c(16:20),best.model[i]]
    }else if(best.model[i] == 8){
      coef.best[i,c(1:7),1] <- coef[list[i],c(1:7),best.model[i]]
      coef.best[i,c(1:7),2] <- coef[list[i],c(8:14),best.model[i]]
      coef.best[i,c(1:7),3] <- coef[list[i],c(15:21),best.model[i]]
      coef.best[i,c(1:7),4] <- coef[list[i],c(22:28),best.model[i]]
    }   
  }    
  return(coef.best)  
}
coef.avoider <- coef.best.model(coef,inf.model.avoider,avoider.list)
coef.adapter <- coef.best.model(coef,inf.model.adapter,adapter.list)
coef.exploiter <- coef.best.model(coef,inf.model.exploiter,exploiter.list)

coef.avoider.compact <- matrix(NA,nrow=dim(coef.avoider)[1],ncol=dim(coef.avoider)[2])
for(i in 1:dim(coef.avoider)[1]){
  for(j in 1:dim(coef.avoider)[2]){
    coef.avoider.compact[i,j] <- paste(round(coef.avoider[i,j,1],dig=2),round(coef.avoider[i,j,3],dig=2),round(coef.avoider[i,j,4],dig=2),sep=",")
  }
}