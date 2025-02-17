#read in results 

model1 <- as.matrix(read.csv("results/occ_model1_results.csv"))
model2 <- as.matrix(read.csv("results/occ_model2_results.csv"))
model3 <- as.matrix(read.csv("results/occ_model3_results.csv"))
model4 <- as.matrix(read.csv("results/occ_model4_results.csv"))
model5 <- as.matrix(read.csv("results/occ_model5_results.csv"))
model6 <- as.matrix(read.csv("results/occ_model6_results.csv"))
model7 <- as.matrix(read.csv("results/occ_model7_results.csv"))
model8 <- as.matrix(read.csv("results/occ_model8_results.csv"))

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

#best model 
                                  
mod.selection <- cbind(model1[,"WAIC"],model2[,"WAIC"],model3[,"WAIC"],model4[,"WAIC"],model5[,"WAIC"],model6[,"WAIC"],model7[,"WAIC"],model8[,"WAIC"])

delta.mod.selection <- mod.selection 

for(i in 1:nrow(mod.selection)){
  delta.mod.selection[i,] <- mod.selection[i,] - min(mod.selection[i,])
}

  
best.model <- apply(mod.selection,1,which.max)

best.model.avoider <- best.model[avoider.list]
best.model.adapter <- best.model[adapter.list]
best.model.exploiter <- best.model[exploiter.list]

preds <- array(NA, dim = c(53,127,8))
preds[,1:ncol(model1),1] <- model1 
preds[,1:ncol(model2),2] <- model2 
preds[,1:ncol(model3),3] <- model3 
preds[,1:ncol(model4),4] <- model4 
preds[,1:ncol(model5),5] <- model5 
preds[,1:ncol(model6),6] <- model6 
preds[,1:ncol(model7),7] <- model7 
preds[,1:ncol(model8),8] <- model8 

preds.best.model <- array(NA,dim=c(43,24,4))
for(i in 1:43){
  preds.best.model[i,c(1:24),1] <- preds[i,c(1:24),best.model[i]]
  preds.best.model[i,c(1:24),2] <- preds[i,c(25:48),best.model[i]]
  preds.best.model[i,c(1:24),3] <- preds[i,c(49:72),best.model[i]]
  preds.best.model[i,c(1:24),4] <- preds[i,c(73:96),best.model[i]]
}

#params by model 
#Model1: "int.p","int.psi"
#Model2: "int.p","int.psi","beta.site.type[2]"
#Model3: "int.p","int.psi","beta.year"
#Model4: "int.p","int.psi","beta.year","beta.year2"
#Model5: "int.p","int.psi","beta.site.type[2]","beta.year"
#Model6: "int.p","int.psi","beta.site.type[2]","beta.year","beta.year2"
#Model7: "int.p","int.psi","beta.site.type[2]","beta.year","beta.site.type.year[2]"
#Model8: "int.p","int.psi","beta.site.type[2]","beta.year","beta.year2","beta.site.type.year[2]","beta.site.type.year2[2]"


avoider.list <- avoider.list[which(is.element(avoider.list,data.poor)==FALSE)]
adaptor.list <- adaptor.list[which(is.element(adaptor.list,data.poor)==FALSE)]
exploiter.list <- exploiter.list[which(is.element(exploiter.list,data.poor)==FALSE)]


