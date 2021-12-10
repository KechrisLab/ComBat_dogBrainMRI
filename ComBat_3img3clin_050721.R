#ComBat data harmonization across labs
rm(list=ls())
load("dataPreprocessing_CCTSIproject_v2.RData")
library(performanceEstimation)
library(caret)

#library(devtools)
#install_github("jfortin1/CombatHarmonization/R/neuroCombat")
library(neuroCombat)
batch = c(rep(0,nrow(meninglio.CSU.image3.1)), rep(1,nrow(meninglio.Out.image3.1))) #Batch variable for the scanner id
table(batch)

#Scenario 1: 3 image vars ONLY in ComBat 
dat.1 = rbind(meninglio.CSU.image3.1[,-4], meninglio.Out.image3.1[,-4]) 
nCombat.1 <- neuroCombat(dat=t(dat.1), batch=batch)
dim(nCombat.1$dat.combat)
dat.1.post = t(nCombat.1$dat.combat)
dim(dat.1.post)

meninglio.CSU.image3.1.post = as.data.frame(dat.1.post[1:nrow(meninglio.CSU.image3.1),])
meninglio.CSU.image3.1.post$dis.lab = meninglio.CSU.image3.1$dis.lab
meninglio.Out.image3.1.post = as.data.frame(dat.1.post[-c(1:nrow(meninglio.CSU.image3.1)),])
meninglio.Out.image3.1.post$dis.lab = meninglio.Out.image3.1$dis.lab
dim(meninglio.CSU.image3.1.post) #145 x 4
dim(meninglio.Out.image3.1.post) #99 x 4

#ComBat, parametric = FALSE
nCombat.1.pf <- neuroCombat(dat=t(dat.1), batch=batch, parametric = F)
dim(nCombat.1.pf$dat.combat)
dat.1.post.pf = t(nCombat.1.pf$dat.combat)
dim(dat.1.post.pf)

#Scenario 2A-2: 3 image vars + 3 clinical covs in ComBat
dat.2 = rbind(meninglio.CSU.image3.2[,1:3], meninglio.Out.image3.2[,1:3]) 
mod.2 = rbind(meninglio.CSU.image3.2[,5:7], meninglio.Out.image3.2[,5:7])
nCombat.2 <- neuroCombat(dat=t(dat.2), batch=batch, mod = mod.2)
dim(nCombat.2$dat.combat)
dat.2.post = t(nCombat.2$dat.combat)
dim(dat.2.post)

meninglio.CSU.image3.2.post = cbind(dat.2.post[1:nrow(meninglio.CSU.image3.2),], meninglio.CSU.image3.2$dis.lab, mod.2[1:nrow(meninglio.CSU.image3.2), ])
meninglio.Out.image3.2.post = cbind(dat.2.post[-c(1:nrow(meninglio.CSU.image3.2)),], meninglio.Out.image3.2$dis.lab, mod.2[-c(1:nrow(meninglio.CSU.image3.2)), ])
colnames(meninglio.CSU.image3.2.post)[4] = colnames(meninglio.Out.image3.2.post)[4] = "dis.lab"
dim(meninglio.CSU.image3.2.post) #145 x 7
dim(meninglio.Out.image3.2.post) #99 x 7

#Scenario: 2A-2-PF
#ComBat, Parametric = FALSE
nCombat.2.pf <- neuroCombat(dat=t(dat.2), batch=batch, mod = mod.2, parametric = F)
dim(nCombat.2.pf$dat.combat)
dat.2.post.pf = t(nCombat.2.pf$dat.combat)
dim(dat.2.post.pf)

meninglio.CSU.image3.2.post.pf = cbind(dat.2.post.pf[1:nrow(meninglio.CSU.image3.2),], meninglio.CSU.image3.2$dis.lab, 
                                       mod.2[1:nrow(meninglio.CSU.image3.2), ])
meninglio.Out.image3.2.post.pf = cbind(dat.2.post.pf[-c(1:nrow(meninglio.CSU.image3.2)),], meninglio.Out.image3.2$dis.lab, 
                                       mod.2[-c(1:nrow(meninglio.CSU.image3.2)), ])
colnames(meninglio.CSU.image3.2.post.pf)[4] = colnames(meninglio.Out.image3.2.post.pf)[4] = "dis.lab"
dim(meninglio.CSU.image3.2.post.pf) #145 x 7
dim(meninglio.Out.image3.2.post.pf) #99 x 7


#Scenario 2A: 3 image vars USED in ComBat + 3 clinical vars simply appended after
meninglio.CSU.image3.22.post = cbind(dat.1.post[1:nrow(meninglio.CSU.image3.1),], meninglio.CSU.image3.2$dis.lab, 
                                     mod.2[1:nrow(meninglio.CSU.image3.2), ])
meninglio.Out.image3.22.post = cbind(dat.1.post[-c(1:nrow(meninglio.CSU.image3.2)),], meninglio.Out.image3.2$dis.lab, 
                                     mod.2[-c(1:nrow(meninglio.CSU.image3.2)),])
colnames(meninglio.CSU.image3.22.post)[4] = colnames(meninglio.Out.image3.22.post)[4] = "dis.lab"
dim(meninglio.CSU.image3.22.post) #145 x 7
dim(meninglio.Out.image3.22.post) #100 x 7

#Scenario 2A-PF
#ComBat, paramteric = FALSE
meninglio.CSU.image3.22.post.pf = cbind(dat.1.post.pf[1:nrow(meninglio.CSU.image3.1),], meninglio.CSU.image3.2$dis.lab, 
                                        mod.2[1:nrow(meninglio.CSU.image3.2), ])
meninglio.Out.image3.22.post.pf = cbind(dat.1.post.pf[-c(1:nrow(meninglio.CSU.image3.2)),], meninglio.Out.image3.2$dis.lab, 
                                        mod.2[-c(1:nrow(meninglio.CSU.image3.2)),])
colnames(meninglio.CSU.image3.22.post.pf)[4] = colnames(meninglio.Out.image3.22.post.pf)[4] = "dis.lab"
dim(meninglio.CSU.image3.22.post.pf) #145 x 7
dim(meninglio.Out.image3.22.post.pf) #99 x 7


#Scenario 2A: 3 image variables ONLY used in ComBat, classification model: 3 img + 3 clin
#Scenario 2A-PF: 3 image variables ONLY used in ComBat (parametric=FALSE), classification model: 3 img + 3 clin
#POST ComBat HARMONIZATION: LOWER BOUND
#Calculate performance accuracy over REP test sets
REP = 75

accuracy.22.lb.post = matrix(NA, REP, 4)
sensitivityGlio.22.lb.post = specificityMenin.22.lb.post = rep(NA, REP)

accuracy.22.lb.post.pf = matrix(NA, REP, 4)
sensitivityGlio.22.lb.post.pf = specificityMenin.22.lb.post.pf = rep(NA, REP)

ind.meninglio.test.CSU = vector(mode = "list", length=REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  #Test data: CSU
  menin.CSU.test = sample(sum(meninglio.CSU.image3.22.post$dis.lab == 1), 19)
  glio.CSU.test = sample((sum(meninglio.CSU.image3.22.post$dis.lab == 1)+1):nrow(meninglio.CSU.image3.22.post), 19)
  ind.meninglio.test.CSU[[r]] = c(menin.CSU.test, glio.CSU.test)
  
  #Scenario 2.2: 3 image variables USED in ComBat + 3 clinical covariates
  data.temp.22 = meninglio.Out.image3.22.post
  dim(data.temp.22) #99 x 7
  #SMOTE on training data
  data.temp.22$dis.lab = factor(ifelse(meninglio.Out.image3.22.post$dis.lab == 0,"Glio","Menin"))
  data.temp.22$sexMF = factor(ifelse(meninglio.Out.image3.22.post$sexMF == 0,"F","M"))
  data.temp.22$breedTypeBO = factor(ifelse(meninglio.Out.image3.22.post$breedTypeBO == 0,"Non-B","Brachy"))
  newData.22 <- smote(dis.lab ~ ., data = data.temp.22, perc.over = 3, perc.under = 1.45)
  table(newData.22$dis.lab)

  meninglio.Out.image3.22.train.post = rbind(data.temp.22[which(data.temp.22$dis.lab == "Menin"),],
                                             newData.22[which(newData.22$dis.lab == "Glio"),])
  #dim(meninglio.Out.image3.2.train)
  #colnames(meninglio.Out.image3.2.train)

  meninglio.CSU.image3.22.test.post = meninglio.CSU.image3.22.post[ind.meninglio.test.CSU[[r]],]
  dim(meninglio.CSU.image3.22.test.post)
  data.temp.CSU.22.test = meninglio.CSU.image3.22.test.post
  data.temp.CSU.22.test$dis.lab = factor(ifelse(meninglio.CSU.image3.22.test.post$dis.lab == 0,"Glio","Menin"))
  data.temp.CSU.22.test$sexMF = factor(ifelse(meninglio.CSU.image3.22.test.post$sexMF == 0,"F","M"))
  data.temp.CSU.22.test$breedTypeBO = factor(ifelse(meninglio.CSU.image3.22.test.post$breedTypeBO == 0,"Non-B","Brachy"))
  dim(data.temp.CSU.22.test) #38 x 7
  
  #Scenario 2.2: 3 image variables USED in ComBat + 3 clinical covariates
  #ComBat, parametric = FALSE
  data.temp.22.pf = meninglio.Out.image3.22.post.pf
  dim(data.temp.22.pf) #99 x 7
  #SMOTE on training data
  data.temp.22.pf$dis.lab = factor(ifelse(meninglio.Out.image3.22.post.pf$dis.lab == 0,"Glio","Menin"))
  data.temp.22.pf$sexMF = factor(ifelse(meninglio.Out.image3.22.post.pf$sexMF == 0,"F","M"))
  data.temp.22.pf$breedTypeBO = factor(ifelse(meninglio.Out.image3.22.post.pf$breedTypeBO == 0,"Non-B","Brachy"))
  newData.22.pf <- smote(dis.lab ~ ., data = data.temp.22.pf, perc.over = 3, perc.under = 1.45)
  table(newData.22.pf$dis.lab)
  
  meninglio.Out.image3.22.train.post.pf = rbind(data.temp.22.pf[which(data.temp.22.pf$dis.lab == "Menin"),],
                                                newData.22.pf[which(newData.22.pf$dis.lab == "Glio"),])
  #dim(meninglio.Out.image3.2.train)
  #colnames(meninglio.Out.image3.2.train)
  
  meninglio.CSU.image3.22.test.post.pf = meninglio.CSU.image3.22.post.pf[ind.meninglio.test.CSU[[r]],]
  dim(meninglio.CSU.image3.22.test.post.pf)
  data.temp.CSU.22.test.pf = meninglio.CSU.image3.22.test.post.pf
  data.temp.CSU.22.test.pf$dis.lab = factor(ifelse(meninglio.CSU.image3.22.test.post.pf$dis.lab == 0,"Glio","Menin"))
  data.temp.CSU.22.test.pf$sexMF = factor(ifelse(meninglio.CSU.image3.22.test.post.pf$sexMF == 0,"F","M"))
  data.temp.CSU.22.test.pf$breedTypeBO = factor(ifelse(meninglio.CSU.image3.22.test.post.pf$breedTypeBO == 0,"Non-B","Brachy"))
  dim(data.temp.CSU.22.test.pf) #38 x 7
  
  #Fit logistic model on the training data
  train_control = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 25,
                               classProbs = TRUE
                               #seeds = 2*r
                               #sampling = "up",
                               #search = "random"
  )
  
  
  #Scenario 2A: 3 image variables ONLY used in ComBat 
  #3 clinical covariates USED in classification model
  mtry <- 1:ncol(meninglio.Out.image3.22.train.post[,-4])
  tunegrid <- expand.grid(.mtry=mtry)

  set.seed(2*r)
  log.model.Out.train.22.post <- train(x = meninglio.Out.image3.22.train.post[,-4],
                                       y = meninglio.Out.image3.22.train.post[,4],
                                       #data = meninglio.CSU.image3.1.train,
                                       trControl = train_control,
                                       #preProcess = c("center", "scale"),
                                       method = 'rf',
                                       metric = "Accuracy",
                                       tuneGrid = tunegrid
                                       #family = binomial()
  )
  
  #Scenario 2.2.pf: 3 image variables ONLY used in ComBat (parametric=FALSE)
  #3 clinical covariates USED in classification model
  set.seed(2*r)
  log.model.Out.train.22.post.pf <- train(x = meninglio.Out.image3.22.train.post.pf[,-4],
                                          y = meninglio.Out.image3.22.train.post.pf[,4],
                                          #data = meninglio.CSU.image3.1.train,
                                          trControl = train_control,
                                          #preProcess = c("center", "scale"),
                                          method = 'rf',
                                          metric = "Accuracy",
                                          tuneGrid = tunegrid
                                          #family = binomial()
  )
  
  #Predictions
  #CSU test set
  log.predictions.CSU.test.22 <- predict(log.model.Out.train.22.post, data.temp.CSU.22.test[,-4])
  head(log.predictions.CSU.test.22)
  
  log.predictions.CSU.test.22.pf <- predict(log.model.Out.train.22.post.pf, data.temp.CSU.22.test.pf[,-4])
  head(log.predictions.CSU.test.22.pf)
  
  #Confusion Matrix
  #3 image variables
  confMat.test.CSU.22 = confusionMatrix(log.predictions.CSU.test.22, data.temp.CSU.22.test$dis.lab)
  confMat.test.CSU.22.pf = confusionMatrix(log.predictions.CSU.test.22.pf, data.temp.CSU.22.test.pf$dis.lab)

  #Store the results
  #Test set: CSU
  #Accuracy
  accuracy.22.lb.post[r,] = confMat.test.CSU.22$overall[c(1,3,4,7)]
  accuracy.22.lb.post.pf[r,] = confMat.test.CSU.22.pf$overall[c(1,3,4,7)]
  
  #Sensitivity
  sensitivityGlio.22.lb.post[r] = confMat.test.CSU.22$byClass[1]
  sensitivityGlio.22.lb.post.pf[r] = confMat.test.CSU.22.pf$byClass[1]
  
  #Specificity
  specificityMenin.22.lb.post[r] = confMat.test.CSU.22$byClass[2]
  specificityMenin.22.lb.post.pf[r] = confMat.test.CSU.22.pf$byClass[2]
 }
toc = proc.time() - tic
toc

round(median(sensitivityGlio.22.lb.post), 3)
round(mad(sensitivityGlio.22.lb.post), 3)
round(mean(sensitivityGlio.22.lb.post), 3)
round(sd(sensitivityGlio.22.lb.post), 3)

round(median(specificityMenin.22.lb.post), 3)
round(mad(specificityMenin.22.lb.post), 3)
round(mean(specificityMenin.22.lb.post), 3)
round(sd(specificityMenin.22.lb.post), 3)

round(median(accuracy.22.lb.post[,1]), 3)
round(mad(accuracy.22.lb.post[,1]), 3)
round(mean(accuracy.22.lb.post[,1]), 3)
round(sd(accuracy.22.lb.post[,1]), 3)

median(accuracy.22.lb.post[,1]) 
accuracy.22.lb.post[order(accuracy.22.lb.post[,1])[38]] 
sensitivityGlio.22.lb.post[order(accuracy.22.lb.post[,1])[38]] 
specificityMenin.22.lb.post[order(accuracy.22.lb.post[,1])[38]] 


#Scenario 2A-PF
round(median(sensitivityGlio.22.lb.post.pf), 3)
round(mad(sensitivityGlio.22.lb.post.pf), 3)
round(mean(sensitivityGlio.22.lb.post.pf), 3)
round(sd(sensitivityGlio.22.lb.post.pf), 3)

round(median(specificityMenin.22.lb.post.pf), 3)
round(mad(specificityMenin.22.lb.post.pf), 3)
round(mean(specificityMenin.22.lb.post.pf), 3)
round(sd(specificityMenin.22.lb.post.pf), 3)

round(median(accuracy.22.lb.post.pf[,1]), 3)
round(mad(accuracy.22.lb.post.pf[,1]), 3)
round(mean(accuracy.22.lb.post.pf[,1]), 3)
round(sd(accuracy.22.lb.post.pf[,1]), 3)

median(accuracy.22.lb.post.pf[,1]) 
accuracy.22.lb.post.pf[order(accuracy.22.lb.post.pf[,1])[38]] #0.684
sensitivityGlio.22.lb.post.pf[order(accuracy.22.lb.post.pf[,1])[38]] #0.579
specificityMenin.22.lb.post.pf[order(accuracy.22.lb.post.pf[,1])[38]] #0.789


#Scenario 2A-2: 3 image variables + 3 clinical covs used in ComBat, classification model: 3 img + 3 clin
#Scenario 2A-2-PF: 3 image variables + 3 clinical covs used in ComBat (parametric=FALSE), classification model: 3 img + 3 clin
#POST ComBat HARMONIZATION: LOWER BOUND
#Calculate performance accuracy over REP test sets
REP = 75

accuracy.2.lb.post = matrix(NA, REP, 4)
sensitivityGlio.2.lb.post = specificityMenin.2.lb.post = rep(NA, REP)

accuracy.2.lb.post.pf = matrix(NA, REP, 4)
sensitivityGlio.2.lb.post.pf = specificityMenin.2.lb.post.pf = rep(NA, REP)

ind.meninglio.test.CSU = vector(mode = "list", length=REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  #Test data: CSU
  menin.CSU.test = sample(sum(meninglio.CSU.image3.2.post$dis.lab == 1), 19)
  glio.CSU.test = sample((sum(meninglio.CSU.image3.2.post$dis.lab == 1)+1):nrow(meninglio.CSU.image3.2.post), 19)
  ind.meninglio.test.CSU[[r]] = c(menin.CSU.test, glio.CSU.test)
  
  #Scenario 2.1: 3 image variables + 3 clinical covariates USED in ComBat
  data.temp.2 = meninglio.Out.image3.2.post
  dim(data.temp.2) #100 x 7
  #SMOTE on training data
  data.temp.2$dis.lab = factor(ifelse(meninglio.Out.image3.2.post$dis.lab == 0,"Glio","Menin"))
  data.temp.2$sexMF = factor(ifelse(meninglio.Out.image3.2.post$sexMF == 0,"F","M"))
  data.temp.2$breedTypeBO = factor(ifelse(meninglio.Out.image3.2.post$breedTypeBO == 0,"Non-B","Brachy"))
  newData.2 <- smote(dis.lab ~ ., data = data.temp.2, perc.over = 3, perc.under = 1.45)
  table(newData.2$dis.lab)

  meninglio.Out.image3.2.train.post = rbind(data.temp.2[which(data.temp.2$dis.lab == "Menin"),],
                                       newData.2[which(newData.2$dis.lab == "Glio"),])
  #dim(meninglio.Out.image3.2.train)
  #colnames(meninglio.Out.image3.2.train)

  meninglio.CSU.image3.2.test.post = meninglio.CSU.image3.2.post[ind.meninglio.test.CSU[[r]],]
  dim(meninglio.CSU.image3.2.test.post)
  data.temp.CSU.2.test = meninglio.CSU.image3.2.test.post
  data.temp.CSU.2.test$dis.lab = factor(ifelse(meninglio.CSU.image3.2.test.post$dis.lab == 0,"Glio","Menin"))
  data.temp.CSU.2.test$sexMF = factor(ifelse(meninglio.CSU.image3.2.test.post$sexMF == 0,"F","M"))
  data.temp.CSU.2.test$breedTypeBO = factor(ifelse(meninglio.CSU.image3.2.test.post$breedTypeBO == 0,"Non-B","Brachy"))
  dim(data.temp.CSU.2.test) #38 x 7
  
  
  #Scenario 2.2: 3 image variables USED in ComBat + 3 clinical covariates
  #ComBat, parametric = FALSE
  data.temp.2.pf = meninglio.Out.image3.2.post.pf
  dim(data.temp.2.pf) #99 x 7
  #SMOTE on training data
  data.temp.2.pf$dis.lab = factor(ifelse(meninglio.Out.image3.2.post.pf$dis.lab == 0,"Glio","Menin"))
  data.temp.2.pf$sexMF = factor(ifelse(meninglio.Out.image3.2.post.pf$sexMF == 0,"F","M"))
  data.temp.2.pf$breedTypeBO = factor(ifelse(meninglio.Out.image3.2.post.pf$breedTypeBO == 0,"Non-B","Brachy"))
  newData.2.pf <- smote(dis.lab ~ ., data = data.temp.2.pf, perc.over = 3, perc.under = 1.45)
  table(newData.2.pf$dis.lab)

  meninglio.Out.image3.2.train.post.pf = rbind(data.temp.2.pf[which(data.temp.2.pf$dis.lab == "Menin"),],
                                                newData.2.pf[which(newData.2.pf$dis.lab == "Glio"),])
  #dim(meninglio.Out.image3.2.train)
  #colnames(meninglio.Out.image3.2.train)

  meninglio.CSU.image3.2.test.post.pf = meninglio.CSU.image3.2.post.pf[ind.meninglio.test.CSU[[r]],]
  dim(meninglio.CSU.image3.2.test.post.pf)
  data.temp.CSU.2.test.pf = meninglio.CSU.image3.2.test.post.pf
  data.temp.CSU.2.test.pf$dis.lab = factor(ifelse(meninglio.CSU.image3.2.test.post.pf$dis.lab == 0,"Glio","Menin"))
  data.temp.CSU.2.test.pf$sexMF = factor(ifelse(meninglio.CSU.image3.2.test.post.pf$sexMF == 0,"F","M"))
  data.temp.CSU.2.test.pf$breedTypeBO = factor(ifelse(meninglio.CSU.image3.2.test.post.pf$breedTypeBO == 0,"Non-B","Brachy"))
  dim(data.temp.CSU.2.test.pf) #38 x 7
  
  #Fit logistic model on the training data
  train_control = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 25,
                               classProbs = TRUE
                               #seeds = 2*r
                               #sampling = "up",
                               #search = "random"
  )
  
  #Scenario 2A-2: 3 image variables + 3 biological covariates USED in ComBat
  #3 clinical covariates USED in classification model
  mtry <- 1:ncol(meninglio.Out.image3.2.train.post[,-4])
  tunegrid <- expand.grid(.mtry=mtry)

  set.seed(2*r)
  log.model.Out.train.2.post <- train(x = meninglio.Out.image3.2.train.post[,-4],
                                       y = meninglio.Out.image3.2.train.post[,4],
                                       #data = meninglio.CSU.image3.1.train,
                                       trControl = train_control,
                                       #preProcess = c("center", "scale"),
                                       method = 'rf',
                                       metric = "Accuracy",
                                       tuneGrid = tunegrid
                                       #family = binomial()
  )
  
  #Scenario 2A-2-PF: 3 image variables + 3 biological covariates USED in ComBat (parametric=F)
  #3 clinical covariates USED in classification model
 
  set.seed(2*r)
  log.model.Out.train.2.post.pf <- train(x = meninglio.Out.image3.2.train.post.pf[,-4],
                                      y = meninglio.Out.image3.2.train.post.pf[,4],
                                      #data = meninglio.CSU.image3.1.train,
                                      trControl = train_control,
                                      #preProcess = c("center", "scale"),
                                      method = 'rf',
                                      metric = "Accuracy",
                                      tuneGrid = tunegrid
                                      #family = binomial()
  )
  
  #Predictions
  #CSU test set
  log.predictions.CSU.test.2 <- predict(log.model.Out.train.2.post, data.temp.CSU.2.test[,-4])
  head(log.predictions.CSU.test.2)
  log.predictions.CSU.test.2.pf <- predict(log.model.Out.train.2.post.pf, data.temp.CSU.2.test.pf[,-4])
  head(log.predictions.CSU.test.2.pf)

  
  #Confusion Matrix
  #3 image variables
  confMat.test.CSU.2 = confusionMatrix(log.predictions.CSU.test.2, data.temp.CSU.2.test$dis.lab)
  confMat.test.CSU.2.pf = confusionMatrix(log.predictions.CSU.test.2.pf, data.temp.CSU.2.test.pf$dis.lab)
  
  #Store the results
  #Test set: CSU
  #Accuracy
 
  accuracy.2.lb.post[r,] = confMat.test.CSU.2$overall[c(1,3,4,7)]
  accuracy.2.lb.post.pf[r,] = confMat.test.CSU.2.pf$overall[c(1,3,4,7)]

  
  #Sensitivity

  sensitivityGlio.2.lb.post[r] = confMat.test.CSU.2$byClass[1]
  sensitivityGlio.2.lb.post.pf[r] = confMat.test.CSU.2.pf$byClass[1]
 
  
  #Specificity
  
  specificityMenin.2.lb.post[r] = confMat.test.CSU.2$byClass[2]
  specificityMenin.2.lb.post.pf[r] = confMat.test.CSU.2.pf$byClass[2]
 
}
toc = proc.time() - tic
toc

#Scenario-2A-2
round(median(sensitivityGlio.2.lb.post), 3)
round(mad(sensitivityGlio.2.lb.post), 3)
round(mean(sensitivityGlio.2.lb.post), 3)
round(sd(sensitivityGlio.2.lb.post), 3)

round(median(specificityMenin.2.lb.post), 3)
round(mad(specificityMenin.2.lb.post), 3)
round(mean(specificityMenin.2.lb.post), 3)
round(sd(specificityMenin.2.lb.post), 3)

round(median(accuracy.2.lb.post[,1]), 3)
round(mad(accuracy.2.lb.post[,1]), 3)
round(mean(accuracy.2.lb.post[,1]), 3)
round(sd(accuracy.2.lb.post[,1]), 3)

median(accuracy.2.lb.post[,1]) 
accuracy.2.lb.post[order(accuracy.2.lb.post[,1])[38]] #0.711
sensitivityGlio.2.lb.post[order(accuracy.2.lb.post[,1])[38]] #0.579
specificityMenin.2.lb.post[order(accuracy.2.lb.post[,1])[38]] #0.842


#Scenario 2A-2-PF
round(median(sensitivityGlio.2.lb.post.pf), 3)
round(mad(sensitivityGlio.2.lb.post.pf), 3)
round(mean(sensitivityGlio.2.lb.post.pf), 3)
round(sd(sensitivityGlio.2.lb.post.pf), 3)

round(median(specificityMenin.2.lb.post.pf), 3)
round(mad(specificityMenin.2.lb.post.pf), 3)
round(mean(specificityMenin.2.lb.post.pf), 3)
round(sd(specificityMenin.2.lb.post.pf), 3)

round(median(accuracy.2.lb.post.pf[,1]), 3)
round(mad(accuracy.2.lb.post.pf[,1]), 3)
round(mean(accuracy.2.lb.post.pf[,1]), 3)
round(sd(accuracy.2.lb.post.pf[,1]), 3)

median(accuracy.2.lb.post.pf[,1]) 
accuracy.2.lb.post.pf[order(accuracy.2.lb.post.pf[,1])[38]] #0.684
sensitivityGlio.2.lb.post.pf[order(accuracy.2.lb.post.pf[,1])[38]] #0.579
specificityMenin.2.lb.post.pf[order(accuracy.2.lb.post.pf[,1])[38]] #0.789


#Scenario 2A-3: 3 img + 3 clin vars used in ComBat
#3 img vars ONLY in classification model
REP = 75

accuracy.23.lb.post = matrix(NA, REP, 4)
sensitivityGlio.23.lb.post = specificityMenin.23.lb.post = rep(NA, REP)

ind.meninglio.test.CSU = vector(mode = "list", length=REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  #Test data: CSU
  menin.CSU.test = sample(sum(meninglio.CSU.image3.2.post$dis.lab == 1), 19)
  glio.CSU.test = sample((sum(meninglio.CSU.image3.2.post$dis.lab == 1)+1):nrow(meninglio.CSU.image3.2.post), 19)
  ind.meninglio.test.CSU[[r]] = c(menin.CSU.test, glio.CSU.test)
  
  #Scenario 2.1: 3 image + 3 clinical covs USED in ComBat
  data.temp.2 = meninglio.Out.image3.2.post[, 1:4]
  dim(data.temp.2) #99 x 4
  #SMOTE on training data
  data.temp.2$dis.lab = factor(ifelse(meninglio.Out.image3.2.post$dis.lab == 0,"Glio","Menin"))
  # data.temp.2$sexMF = factor(ifelse(meninglio.Out.image3.2.post$sexMF == 0,"F","M"))
  # data.temp.2$breedTypeBO = factor(ifelse(meninglio.Out.image3.2.post$breedTypeBO == 0,"Non-B","Brachy"))
  newData.2 <- smote(dis.lab ~ ., data = data.temp.2, perc.over = 3, perc.under = 1.45)
  table(newData.2$dis.lab)
  
  meninglio.Out.image3.2.train.post = rbind(data.temp.2[which(data.temp.2$dis.lab == "Menin"),],
                                            newData.2[which(newData.2$dis.lab == "Glio"),])
  #dim(meninglio.Out.image3.2.train)
  #colnames(meninglio.Out.image3.2.train)
  
  meninglio.CSU.image3.2.test.post = meninglio.CSU.image3.2.post[ind.meninglio.test.CSU[[r]], 1:4]
  dim(meninglio.CSU.image3.2.test.post)
  data.temp.CSU.2.test = meninglio.CSU.image3.2.test.post
  data.temp.CSU.2.test$dis.lab = factor(ifelse(meninglio.CSU.image3.2.test.post$dis.lab == 0,"Glio","Menin"))
  #data.temp.CSU.2.test$sexMF = factor(ifelse(meninglio.CSU.image3.2.test.post$sexMF == 0,"F","M"))
  #data.temp.CSU.2.test$breedTypeBO = factor(ifelse(meninglio.CSU.image3.2.test.post$breedTypeBO == 0,"Non-B","Brachy"))
  dim(data.temp.CSU.2.test) #38 x 4
  
  
  #Fit logistic model on the training data
  train_control = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 25,
                               classProbs = TRUE
                               #seeds = 2*r
                               #sampling = "up",
                               #search = "random"
  )
  
  
  #Scenario 2.3: 3 image variables + 3 biological covariates USED in ComBat
  #3 Clinical covariates NOT used in classification model
  mtry <- 1:ncol(meninglio.Out.image3.2.train.post[,1:3])
  tunegrid <- expand.grid(.mtry=mtry)

  set.seed(2*r)
  log.model.Out.train.23.post <- train(x = meninglio.Out.image3.2.train.post[,1:3],
                                 y = meninglio.Out.image3.2.train.post[,4],
                                 #data = meninglio.CSU.image3.1.train,
                                 trControl = train_control,
                                 #preProcess = c("center", "scale"),
                                 method = 'rf',
                                 metric = "Accuracy",
                                 tuneGrid = tunegrid
                                 #family = binomial()
  )
  
  #Predictions
  #CSU test set
  log.predictions.CSU.test.23 <- predict(log.model.Out.train.23.post, data.temp.CSU.2.test[,1:3])
  head(log.predictions.CSU.test.23)
  
  #Confusion Matrix
  #3 image variables

  confMat.test.CSU.23 = confusionMatrix(log.predictions.CSU.test.23, data.temp.CSU.2.test$dis.lab)
 
  #Test set: CSU
  #Accuracy
  accuracy.23.lb.post[r,] = confMat.test.CSU.23$overall[c(1,3,4,7)]
  
  #Sensitivity
  sensitivityGlio.23.lb.post[r] = confMat.test.CSU.23$byClass[1]
  
  #Specificity
  specificityMenin.23.lb.post[r] = confMat.test.CSU.23$byClass[2]
}
toc = proc.time() - tic
toc

round(median(sensitivityGlio.23.lb.post), 3)
round(mad(sensitivityGlio.23.lb.post), 3)
round(mean(sensitivityGlio.23.lb.post), 3)
round(sd(sensitivityGlio.23.lb.post), 3)

round(median(specificityMenin.23.lb.post), 3)
round(mad(specificityMenin.23.lb.post), 3)
round(mean(specificityMenin.23.lb.post), 3)
round(sd(specificityMenin.23.lb.post), 3)

round(median(accuracy.23.lb.post[,1]), 3)
round(mad(accuracy.23.lb.post[,1]), 3)
round(mean(accuracy.23.lb.post[,1]), 3)
round(sd(accuracy.23.lb.post[,1]), 3)

median(accuracy.23.lb.post[,1]) 
accuracy.23.lb.post[order(accuracy.23.lb.post[,1])[38]] #0.658
sensitivityGlio.23.lb.post[order(accuracy.23.lb.post[,1])[38]] #0.579
specificityMenin.23.lb.post[order(accuracy.23.lb.post[,1])[38]] #0.789


#save.image("ComBat_3img3clin_090121.RData")













