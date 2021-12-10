#ComBat data harmonization across labs
rm(list=ls())
load("dataPreprocessing_CCTSIproject_v2.RData")
library(performanceEstimation)
library(caret)


#ComBat data harmonization across labs
library(devtools)
install_github("jfortin1/neuroCombatData")
install_github("jfortin1/neuroCombat_Rpackage")

library(neuroCombat)
batch = c(rep(0,nrow(meninglio.CSU.image3.3)), rep(1,nrow(meninglio.Out.image3.3))) #Batch variable for the scanner id
table(batch)

#Scenario 3-1 & 3-2: 3 image vars + 3 clinical covs + 1 tech cov + 6 scanner covs in ComBat
dat.3 = rbind(meninglio.CSU.image3.3[,1:3], meninglio.Out.image3.3[,1:3]) 
mod.3 = rbind(meninglio.CSU.image3.3[,5:14], meninglio.Out.image3.3[,5:14])
nCombat.3 <- neuroCombat(dat=t(dat.3), batch=batch, mod = mod.3)
dim(nCombat.3$dat.combat)
dat.3.post = t(nCombat.3$dat.combat)
dim(dat.3.post)

meninglio.CSU.image3.3.post = cbind(dat.3.post[1:nrow(meninglio.CSU.image3.3),], meninglio.CSU.image3.3$dis.lab, mod.3[1:nrow(meninglio.CSU.image3.3), 1:3])
meninglio.Out.image3.3.post = cbind(dat.3.post[-c(1:nrow(meninglio.CSU.image3.3)),], meninglio.Out.image3.3$dis.lab, mod.3[-c(1:nrow(meninglio.CSU.image3.3)), 1:3])
colnames(meninglio.CSU.image3.3.post)[4] = colnames(meninglio.Out.image3.3.post)[4] = "dis.lab"
dim(meninglio.CSU.image3.3.post) #145 x 7
dim(meninglio.Out.image3.3.post) #99 x 7


#Scenario 3.1: Use 3 image + 3 clin + 1 tech + 6 scanner covs in ComBat
#Use 3 img vars ONLY in classification model
REP = 75
accuracy.31.lb.post = matrix(NA, REP, 4)
sensitivityGlio.31.lb.post = specificityMenin.31.lb.post = rep(NA, REP)

ind.meninglio.test.CSU = vector(mode = "list", length=REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  #Test data: CSU
  menin.CSU.test = sample(sum(meninglio.CSU.image3.3.post$dis.lab == 1), 19)
  glio.CSU.test = sample((sum(meninglio.CSU.image3.3.post$dis.lab == 1)+1):nrow(meninglio.CSU.image3.3.post), 19)
  ind.meninglio.test.CSU[[r]] = c(menin.CSU.test, glio.CSU.test)
  
  #Scenario 3: 3 image variables + 3 clin + 1 tech + 6 scanner covs USED in ComBat
  data.temp.3 = meninglio.Out.image3.3.post[, 1:3]
  dim(data.temp.3) #99 x 3
  #SMOTE on training data
  data.temp.3$dis.lab = factor(ifelse(meninglio.Out.image3.3.post$dis.lab == 0,"Glio","Menin"))
  newData.3 <- smote(dis.lab ~ ., data = data.temp.3, perc.over = 3, perc.under = 1.45)
  table(newData.3$dis.lab) #G: 80, M: 87
  
  meninglio.Out.image3.3.train.post = rbind(data.temp.3[which(data.temp.3$dis.lab == "Menin"),],
                                               newData.3[which(newData.3$dis.lab == "Glio"),])
  #dim(meninglio.Out.image3.3.train)
  #colnames(meninglio.Out.image3.3.train)
  
  meninglio.CSU.image3.3.test.post = meninglio.CSU.image3.3.post[ind.meninglio.test.CSU[[r]], 1:4]
  dim(meninglio.CSU.image3.3.test.post) #38 x 4
  data.temp.CSU.3.test = meninglio.CSU.image3.3.test.post
  data.temp.CSU.3.test$dis.lab = factor(ifelse(meninglio.CSU.image3.3.test.post$dis.lab == 0,"Glio","Menin"))
 dim(data.temp.CSU.3.test) #38 x 4
  
  
  
  
  #Fit logistic model on the training data
  train_control = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 25,
                               classProbs = TRUE
                               #seeds = 2*r
                               #sampling = "up",
                               #search = "random"
  )
  
  #Scenario 3.1: 3 image variables + 3 clin + 1 tech + 6 scanner covs USED in ComBat 
  #3 img vars in classification model
  mtry <- 1:ncol(meninglio.Out.image3.3.train.post[,c(1:3)])
  tunegrid <- expand.grid(.mtry=mtry)
  
  set.seed(2*r)
  log.model.Out.train.3.post <- train(x = meninglio.Out.image3.3.train.post[,c(1:3)],
                                          y = meninglio.Out.image3.3.train.post[,4],
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
  log.predictions.CSU.test.3 <- predict(log.model.Out.train.3.post, data.temp.CSU.3.test[,c(1:3)])
  head(log.predictions.CSU.test.3)
  
  
  #Confusion Matrix
  #3 image variables
  confMat.test.CSU.3 = confusionMatrix(log.predictions.CSU.test.3, data.temp.CSU.3.test$dis.lab)

  #Store the results
  #Test set: CSU
  #Accuracy
  accuracy.31.lb.post[r,] = confMat.test.CSU.3$overall[c(1,3,4,7)]

  #Sensitivity
  sensitivityGlio.31.lb.post[r] = confMat.test.CSU.3$byClass[1]

  #Specificity
  specificityMenin.31.lb.post[r] = confMat.test.CSU.3$byClass[2]
}
toc = proc.time() - tic
toc

round(median(sensitivityGlio.31.lb.post), 3)
round(mad(sensitivityGlio.31.lb.post), 3)
round(mean(sensitivityGlio.31.lb.post), 3)
round(sd(sensitivityGlio.31.lb.post), 3)

round(median(specificityMenin.31.lb.post), 3)
round(mad(specificityMenin.31.lb.post), 3)
round(mean(specificityMenin.31.lb.post), 3)
round(sd(specificityMenin.31.lb.post), 3)

round(median(accuracy.31.lb.post[,1]), 3)
round(mad(accuracy.31.lb.post[,1]), 3)
round(mean(accuracy.31.lb.post[,1]), 3)
round(sd(accuracy.31.lb.post[,1]), 3)

median(accuracy.31.lb.post[,1]) 
accuracy.31.lb.post[order(accuracy.31.lb.post[,1])[38]] 
sensitivityGlio.31.lb.post[order(accuracy.31.lb.post[,1])[38]] 
specificityMenin.31.lb.post[order(accuracy.31.lb.post[,1])[38]]


#Scenario 3.2: Use 3 imag + 3 clin + 1 tech + 6 scanner covs in ComBat
#Use 3 img + 3 clin covs in classification model
REP = 75
accuracy.32.lb.post = matrix(NA, REP, 4)
sensitivityGlio.32.lb.post = specificityMenin.32.lb.post = rep(NA, REP)

ind.meninglio.test.CSU = vector(mode = "list", length=REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  #Test data: CSU
  menin.CSU.test = sample(sum(meninglio.CSU.image3.3.post$dis.lab == 1), 19)
  glio.CSU.test = sample((sum(meninglio.CSU.image3.3.post$dis.lab == 1)+1):nrow(meninglio.CSU.image3.3.post), 19)
  ind.meninglio.test.CSU[[r]] = c(menin.CSU.test, glio.CSU.test)
  
  #Scenario 3: 3 image variables + 3 clin + 1 tech + 6 scanner covs USED in ComBat
  data.temp.3 = meninglio.Out.image3.3.post
  dim(data.temp.3) #99 x 7
  #SMOTE on training data
  data.temp.3$dis.lab = factor(ifelse(meninglio.Out.image3.3.post$dis.lab == 0,"Glio","Menin"))
  data.temp.3$sexMF = factor(ifelse(meninglio.Out.image3.3.post$sexMF == 0,"F","M"))
  data.temp.3$breedTypeBO = factor(ifelse(meninglio.Out.image3.3.post$breedTypeBO == 0,"Non-B","Brachy"))
  newData.3 <- smote(dis.lab ~ ., data = data.temp.3, perc.over = 3, perc.under = 1.45)
  table(newData.3$dis.lab)
  
  meninglio.Out.image3.3.train.post = rbind(data.temp.3[which(data.temp.3$dis.lab == "Menin"),],
                                            newData.3[which(newData.3$dis.lab == "Glio"),])
  #dim(meninglio.Out.image3.3.train)
  #colnames(meninglio.Out.image3.3.train)
  
  meninglio.CSU.image3.3.test.post = meninglio.CSU.image3.3.post[ind.meninglio.test.CSU[[r]],]
  dim(meninglio.CSU.image3.3.test.post) #38 x 7
  data.temp.CSU.3.test = meninglio.CSU.image3.3.test.post
  data.temp.CSU.3.test$dis.lab = factor(ifelse(meninglio.CSU.image3.3.test.post$dis.lab == 0,"Glio","Menin"))
  data.temp.CSU.3.test$sexMF = factor(ifelse(meninglio.CSU.image3.3.test.post$sexMF == 0,"F","M"))
  data.temp.CSU.3.test$breedTypeBO = factor(ifelse(meninglio.CSU.image3.3.test.post$breedTypeBO == 0,"Non-B","Brachy"))
  dim(data.temp.CSU.3.test) #38 x 7
  
  
  #Fit logistic model on the training data
  train_control = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 25,
                               classProbs = TRUE
                               #seeds = 2*r
                               #sampling = "up",
                               #search = "random"
  )
  
  #Scenario 3.2: 3 image variables + 3 clin + 1 tech + 6 scanner covs USED in ComBat 
  #3 img + 3 clin covs USED in classification model
  mtry <- 1:ncol(meninglio.Out.image3.3.train.post[,c(1:3, 5:7)])
  tunegrid <- expand.grid(.mtry=mtry)
  
  set.seed(2*r)
  log.model.Out.train.3.post <- train(x = meninglio.Out.image3.3.train.post[,c(1:3, 5:7)],
                                      y = meninglio.Out.image3.3.train.post[,4],
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
  log.predictions.CSU.test.3 <- predict(log.model.Out.train.3.post, data.temp.CSU.3.test[,c(1:3, 5:7)])
  head(log.predictions.CSU.test.3)
  
  
  #Confusion Matrix
  #3 image variables
  confMat.test.CSU.3 = confusionMatrix(log.predictions.CSU.test.3, data.temp.CSU.3.test$dis.lab)
  
  #Store the results
  #Test set: CSU
  #Accuracy
  accuracy.32.lb.post[r,] = confMat.test.CSU.3$overall[c(1,3,4,7)]
  
  #Sensitivity
  sensitivityGlio.32.lb.post[r] = confMat.test.CSU.3$byClass[1]
  
  #Specificity
  specificityMenin.32.lb.post[r] = confMat.test.CSU.3$byClass[2]
}
toc = proc.time() - tic
toc

round(median(sensitivityGlio.32.lb.post), 3)
round(mad(sensitivityGlio.32.lb.post), 3)
round(mean(sensitivityGlio.32.lb.post), 3)
round(sd(sensitivityGlio.32.lb.post), 3)

round(median(specificityMenin.32.lb.post), 3)
round(mad(specificityMenin.32.lb.post), 3)
round(mean(specificityMenin.32.lb.post), 3)
round(sd(specificityMenin.32.lb.post), 3)

round(median(accuracy.32.lb.post[,1]), 3)
round(mad(accuracy.32.lb.post[,1]), 3)
round(mean(accuracy.32.lb.post[,1]), 3)
round(sd(accuracy.32.lb.post[,1]), 3)

median(accuracy.32.lb.post[,1]) 
accuracy.32.lb.post[order(accuracy.32.lb.post[,1])[38]] 
sensitivityGlio.32.lb.post[order(accuracy.32.lb.post[,1])[38]] 
specificityMenin.32.lb.post[order(accuracy.32.lb.post[,1])[38]] 


#Scenario 3.3: 3 image vars + 1 tech cov + 6 scanner covs in ComBat
library(neuroCombat)
batch = c(rep(0,nrow(meninglio.CSU.image3.3)), rep(1,nrow(meninglio.Out.image3.3))) #Batch variable for the scanner id
table(batch)

dat.3 = rbind(meninglio.CSU.image3.3[,1:3], meninglio.Out.image3.3[,1:3]) 
mod.33 = rbind(meninglio.CSU.image3.3[,8:14], meninglio.Out.image3.3[,8:14])
nCombat.33 <- neuroCombat(dat=t(dat.3), batch=batch, mod = mod.33)
dim(nCombat.33$dat.combat)
dat.33.post = t(nCombat.33$dat.combat)
dim(dat.33.post)

meninglio.CSU.image3.33.post = cbind(dat.33.post[1:nrow(meninglio.CSU.image3.3),], meninglio.CSU.image3.3$dis.lab, mod.3[1:nrow(meninglio.CSU.image3.3), 1:3])
meninglio.Out.image3.33.post = cbind(dat.33.post[-c(1:nrow(meninglio.CSU.image3.3)),], meninglio.Out.image3.3$dis.lab, mod.3[-c(1:nrow(meninglio.CSU.image3.3)), 1:3])
colnames(meninglio.CSU.image3.33.post)[4] = colnames(meninglio.Out.image3.33.post)[4] = "dis.lab"
dim(meninglio.CSU.image3.33.post) #145 x 7
dim(meninglio.Out.image3.33.post) #99 x 7


#Scenario 3.3: Use 3 image + 1 tech + 6 scanner covs in ComBat
#Use 3 img + 3 clin covs in classification model
REP = 75
accuracy.33.lb.post = matrix(NA, REP, 4)
sensitivityGlio.33.lb.post = specificityMenin.33.lb.post = rep(NA, REP)

ind.meninglio.test.CSU = vector(mode = "list", length=REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  #Test data: CSU
  menin.CSU.test = sample(sum(meninglio.CSU.image3.33.post$dis.lab == 1), 19)
  glio.CSU.test = sample((sum(meninglio.CSU.image3.33.post$dis.lab == 1)+1):nrow(meninglio.CSU.image3.33.post), 19)
  ind.meninglio.test.CSU[[r]] = c(menin.CSU.test, glio.CSU.test)
  
  #Scenario 3: 3 image variables + 1 tech + 6 scanner covs USED in ComBat
  #3 image + 3 clin vars USED in RF
  data.temp.33 = meninglio.Out.image3.33.post
  dim(data.temp.33) #99 x 7
  #SMOTE on training data
  data.temp.33$dis.lab = factor(ifelse(meninglio.Out.image3.33.post$dis.lab == 0,"Glio","Menin"))
  data.temp.33$sexMF = factor(ifelse(meninglio.Out.image3.33.post$sexMF == 0,"F","M"))
  data.temp.33$breedTypeBO = factor(ifelse(meninglio.Out.image3.33.post$breedTypeBO == 0,"Non-B","Brachy"))
  newData.33 <- smote(dis.lab ~ ., data = data.temp.33, perc.over = 3, perc.under = 1.45)
  table(newData.33$dis.lab)
  
  meninglio.Out.image3.33.train.post = rbind(data.temp.33[which(data.temp.33$dis.lab == "Menin"),],
                                            newData.33[which(newData.33$dis.lab == "Glio"),])
  dim(meninglio.Out.image3.33.train.post) #159 x 7
  colnames(meninglio.Out.image3.33.train.post)
  
  meninglio.CSU.image3.33.test.post = meninglio.CSU.image3.33.post[ind.meninglio.test.CSU[[r]], ]
  dim(meninglio.CSU.image3.33.test.post) #38 x 4
  data.temp.CSU.33.test = meninglio.CSU.image3.33.test.post
  data.temp.CSU.33.test$dis.lab = factor(ifelse(meninglio.CSU.image3.33.test.post$dis.lab == 0,"Glio","Menin"))
  data.temp.CSU.33.test$sexMF = factor(ifelse(meninglio.CSU.image3.33.test.post$sexMF == 0,"F","M"))
  data.temp.CSU.33.test$breedTypeBO = factor(ifelse(meninglio.CSU.image3.33.test.post$breedTypeBO == 0,"Non-B","Brachy"))
  
  dim(data.temp.CSU.33.test) #38 x 7
  
  #Fit logistic model on the training data
  train_control = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 25,
                               classProbs = TRUE
                               #seeds = 2*r
                               #sampling = "up",
                               #search = "random"
  )
  
  #Scenario 3.3: 3 image variables + 1 tech + 6 scanner covs USED in ComBat 
  #3 img vars + 3 clin vars USED in classification model
  mtry <- 1:ncol(meninglio.Out.image3.33.train.post[, -4])
  tunegrid <- expand.grid(.mtry=mtry)
  
  set.seed(2*r)
  log.model.Out.train.3.post <- train(x = meninglio.Out.image3.33.train.post[, -4],
                                      y = meninglio.Out.image3.33.train.post[,4],
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
  log.predictions.CSU.test.3 <- predict(log.model.Out.train.3.post, data.temp.CSU.33.test[, -4])
  head(log.predictions.CSU.test.3)
  
  #Confusion Matrix
  #3 image variables
  confMat.test.CSU.3 = confusionMatrix(log.predictions.CSU.test.3, data.temp.CSU.33.test$dis.lab)
  
  #Store the results
  #Test set: CSU
  #Accuracy
  accuracy.33.lb.post[r,] = confMat.test.CSU.3$overall[c(1,3,4,7)]
  
  #Sensitivity
  sensitivityGlio.33.lb.post[r] = confMat.test.CSU.3$byClass[1]
  
  #Specificity
  specificityMenin.33.lb.post[r] = confMat.test.CSU.3$byClass[2]
}
toc = proc.time() - tic
toc

round(median(sensitivityGlio.33.lb.post), 3)
round(mad(sensitivityGlio.33.lb.post), 3)
round(mean(sensitivityGlio.33.lb.post), 3)
round(sd(sensitivityGlio.33.lb.post), 3)

round(median(specificityMenin.33.lb.post), 3)
round(mad(specificityMenin.33.lb.post), 3)
round(mean(specificityMenin.33.lb.post), 3)
round(sd(specificityMenin.33.lb.post), 3)

round(median(accuracy.33.lb.post[,1]), 3)
round(mad(accuracy.33.lb.post[,1]), 3)
round(mean(accuracy.33.lb.post[,1]), 3)
round(sd(accuracy.33.lb.post[,1]), 3)

median(accuracy.33.lb.post[,1]) 
accuracy.33.lb.post[order(accuracy.33.lb.post[,1])[38]] 
sensitivityGlio.33.lb.post[order(accuracy.33.lb.post[,1])[38]] 
specificityMenin.33.lb.post[order(accuracy.33.lb.post[,1])[38]] 



#Scenario 3.4: Use 3 image + 1 tech + 6 scanner covs in ComBat
#Use 3 img vars in classification model
REP = 75
accuracy.34.lb.post = matrix(NA, REP, 4)
sensitivityGlio.34.lb.post = specificityMenin.34.lb.post = rep(NA, REP)

ind.meninglio.test.CSU = vector(mode = "list", length=REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  #Test data: CSU
  menin.CSU.test = sample(sum(meninglio.CSU.image3.33.post$dis.lab == 1), 19)
  glio.CSU.test = sample((sum(meninglio.CSU.image3.33.post$dis.lab == 1)+1):nrow(meninglio.CSU.image3.33.post), 19)
  ind.meninglio.test.CSU[[r]] = c(menin.CSU.test, glio.CSU.test)
  
  #Scenario 3: 3 image variables + 1 tech + 6 scanner covs USED in ComBat
  #3 image vars USED in RF
  data.temp.34 = meninglio.Out.image3.33.post[, 1:4]
  dim(data.temp.34) #99 x 4
  #SMOTE on training data
  data.temp.34$dis.lab = factor(ifelse(meninglio.Out.image3.33.post$dis.lab == 0,"Glio","Menin"))
  newData.34 <- smote(dis.lab ~ ., data = data.temp.34, perc.over = 3, perc.under = 1.45)
  table(newData.34$dis.lab)
  
  meninglio.Out.image3.34.train.post = rbind(data.temp.34[which(data.temp.34$dis.lab == "Menin"),],
                                             newData.34[which(newData.34$dis.lab == "Glio"),])
  dim(meninglio.Out.image3.34.train.post) #159 x 4
  colnames(meninglio.Out.image3.34.train.post)
  
  meninglio.CSU.image3.34.test.post = meninglio.CSU.image3.33.post[ind.meninglio.test.CSU[[r]], 1:4]
  dim(meninglio.CSU.image3.34.test.post) #38 x 4
  data.temp.CSU.34.test = meninglio.CSU.image3.34.test.post
  data.temp.CSU.34.test$dis.lab = factor(ifelse(meninglio.CSU.image3.33.test.post$dis.lab == 0,"Glio","Menin"))
  
  dim(data.temp.CSU.34.test) #38 x 4
  
  #Fit logistic model on the training data
  train_control = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 25,
                               classProbs = TRUE
                               #seeds = 2*r
                               #sampling = "up",
                               #search = "random"
  )
  
  #Scenario 3.3: 3 image variables + 1 tech + 6 scanner covs USED in ComBat 
  #3 img vars + 3 clin vars USED in classification model
  mtry <- 1:ncol(meninglio.Out.image3.34.train.post[, -4])
  tunegrid <- expand.grid(.mtry=mtry)
  
  set.seed(2*r)
  log.model.Out.train.4.post <- train(x = meninglio.Out.image3.34.train.post[, -4],
                                      y = meninglio.Out.image3.34.train.post[,4],
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
  log.predictions.CSU.test.4 <- predict(log.model.Out.train.4.post, data.temp.CSU.34.test[, -4])
  head(log.predictions.CSU.test.4)
  
  #Confusion Matrix
  #3 image variables
  confMat.test.CSU.4 = confusionMatrix(log.predictions.CSU.test.4, data.temp.CSU.34.test$dis.lab)
  
  #Store the results
  #Test set: CSU
  #Accuracy
  accuracy.34.lb.post[r,] = confMat.test.CSU.4$overall[c(1,3,4,7)]
  
  #Sensitivity
  sensitivityGlio.34.lb.post[r] = confMat.test.CSU.4$byClass[1]
  
  #Specificity
  specificityMenin.34.lb.post[r] = confMat.test.CSU.4$byClass[2]
}
toc = proc.time() - tic
toc

round(median(sensitivityGlio.34.lb.post), 3)
round(mad(sensitivityGlio.34.lb.post), 3)
round(mean(sensitivityGlio.34.lb.post), 3)
round(sd(sensitivityGlio.34.lb.post), 3)

round(median(specificityMenin.34.lb.post), 3)
round(mad(specificityMenin.34.lb.post), 3)
round(mean(specificityMenin.34.lb.post), 3)
round(sd(specificityMenin.34.lb.post), 3)

round(median(accuracy.34.lb.post[,1]), 3)
round(mad(accuracy.34.lb.post[,1]), 3)
round(mean(accuracy.34.lb.post[,1]), 3)
round(sd(accuracy.34.lb.post[,1]), 3)

median(accuracy.34.lb.post[,1]) 
accuracy.34.lb.post[order(accuracy.34.lb.post[,1])[38]] 
sensitivityGlio.34.lb.post[order(accuracy.34.lb.post[,1])[38]] 
specificityMenin.34.lb.post[order(accuracy.34.lb.post[,1])[38]] 


#save.image("ComBat_Scenario3_allcovs_051121.RData")
#save.image("ComBat_Scenario3_allcovs_090321.RData")



