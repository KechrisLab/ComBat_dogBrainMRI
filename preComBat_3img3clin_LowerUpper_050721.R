rm(list=ls())
load("dataPreprocessing_CCTSIproject_v2.RData")
library(performanceEstimation)
library(caret)

#Calculate performance accuracy over REP test sets
#Baseline lower bound
#set.seed(20210216)
REP = 75

accuracy.2.lb = matrix(NA, REP, 4)
sensitivityGlio.2.lb = specificityMenin.2.lb = rep(NA, REP)

ind.meninglio.test.CSU = vector(mode = "list", length=REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  #Test data: CSU
  menin.CSU.test = sample(nrow(menin.CSU.image3), 19)
  glio.CSU.test = sample((nrow(menin.CSU.image3)+1):nrow(meninglio.CSU.image3.1), 19)
  ind.meninglio.test.CSU[[r]] = c(menin.CSU.test, glio.CSU.test)
  
  #Scenario 2A: 3 image variables + 3 biological covariates
  data.temp.2 = meninglio.Out.image3.2
  dim(data.temp.2) #99 x 7
  #SMOTE on training data
  data.temp.2$dis.lab = factor(ifelse(meninglio.Out.image3.2$dis.lab == 0,"Glio","Menin"))
  data.temp.2$sexMF = factor(ifelse(meninglio.Out.image3.2$sexMF == 0,"F","M"))
  data.temp.2$breedTypeBO = factor(ifelse(meninglio.Out.image3.2$breedTypeBO == 0,"Non-B","Brachy"))
  newData.2 <- smote(dis.lab ~ ., data = data.temp.2, perc.over = 3, perc.under = 1.45)
  table(newData.2$dis.lab)
  
  meninglio.Out.image3.2.train = rbind(data.temp.2[which(data.temp.2$dis.lab == "Menin"),],
                                       newData.2[which(newData.2$dis.lab == "Glio"),])
  #dim(meninglio.Out.image3.2.train)
  #colnames(meninglio.Out.image3.2.train)
  
  meninglio.CSU.image3.2.test = meninglio.CSU.image3.2[ind.meninglio.test.CSU[[r]],]
  dim(meninglio.CSU.image3.2.test) #38 x 7
  data.temp.CSU.2.test = meninglio.CSU.image3.2.test
  data.temp.CSU.2.test$dis.lab = factor(ifelse(meninglio.CSU.image3.2.test$dis.lab == 0,"Glio","Menin"))
  data.temp.CSU.2.test$sexMF = factor(ifelse(meninglio.CSU.image3.2.test$sexMF == 0,"F","M"))
  data.temp.CSU.2.test$breedTypeBO = factor(ifelse(meninglio.CSU.image3.2.test$breedTypeBO == 0,"Non-B","Brachy"))
  dim(data.temp.CSU.2.test) #38 x 7
  
  #Fit logistic model on the training data
  train_control = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 25,
                               classProbs = TRUE
                               #seeds = 2*r
                               #sampling = "up",
                               #search = "random"
  )
  
  #Scenario 2: 3 image variables + 3 biological covariates
  mtry <- 1:ncol(meninglio.Out.image3.2.train[,-4])
  tunegrid <- expand.grid(.mtry=mtry)
  
  set.seed(2*r)
  log.model.Out.train.2 <- train(x = meninglio.Out.image3.2.train[,-4],
                                 y = meninglio.Out.image3.2.train[,4],
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
  log.predictions.CSU.test.2 <- predict(log.model.Out.train.2, data.temp.CSU.2.test[,-4])
  head(log.predictions.CSU.test.2)
  
  #Confusion Matrix
  #3 image variables
  confMat.test.CSU.2 = confusionMatrix(log.predictions.CSU.test.2, data.temp.CSU.2.test$dis.lab)

  #Store the results
  #Test set: CSU
  accuracy.2.lb[r,] = confMat.test.CSU.2$overall[c(1,3,4,7)]
  sensitivityGlio.2.lb[r] = confMat.test.CSU.2$byClass[1]
  specificityMenin.2.lb[r] = confMat.test.CSU.2$byClass[2]
}
toc = proc.time() - tic
toc

round(median(sensitivityGlio.2.lb), 3)
round(mad(sensitivityGlio.2.lb), 3)
round(mean(sensitivityGlio.2.lb), 3)
round(sd(sensitivityGlio.2.lb), 3)

round(median(specificityMenin.2.lb), 3)
round(mad(specificityMenin.2.lb), 3)
round(mean(specificityMenin.2.lb), 3)
round(sd(specificityMenin.2.lb), 3)

round(median(accuracy.2.lb[,1]), 3)
round(mad(accuracy.2.lb[,1]), 3)
round(mean(accuracy.2.lb[,1]), 3)
round(sd(accuracy.2.lb[,1]), 3)

median(accuracy.2.lb[,1]) 
accuracy.2.lb[order(accuracy.2.lb[,1])[38]] 
sensitivityGlio.2.lb[order(accuracy.2.lb[,1])[38]] 
specificityMenin.2.lb[order(accuracy.2.lb[,1])[38]] 


#Calculate performance accuracy over REP test sets
#Baseline UPPER BOUND
#set.seed(20210216)

REP=75
accuracy.2.ub = matrix(NA, REP, 4)
sensitivityGlio.2.ub = specificityMenin.2.ub = rep(NA, REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  #Scenario 2: 3 image variables + 3 clinical covariates
  meninglio.CSU.image3.2.train.pre = meninglio.CSU.image3.2[-ind.meninglio.test.CSU[[r]],]
  data.temp.2 = meninglio.CSU.image3.2.train.pre[c(sample(87, 79), 88:107), ]
  dim(data.temp.2) #99 x 7
  #SMOTE on training data
  data.temp.2$dis.lab = factor(ifelse(data.temp.2$dis.lab == 0,"Glio","Menin"))
  data.temp.2$sexMF = factor(ifelse(data.temp.2$sexMF == 0,"F","M"))
  data.temp.2$breedTypeBO = factor(ifelse(data.temp.2$breedTypeBO == 0,"Non-B","Brachy"))
  newData.2 <- smote(dis.lab ~ ., data = data.temp.2, perc.over = 3, perc.under = 1.45)
  table(newData.2$dis.lab)
  
  meninglio.CSU.image3.2.train = rbind(data.temp.2[which(data.temp.2$dis.lab == "Menin"),],
                                       newData.2[which(newData.2$dis.lab == "Glio"),])
  dim(meninglio.CSU.image3.2.train) #159 x 7
  
  meninglio.CSU.image3.2.test = meninglio.CSU.image3.2[ind.meninglio.test.CSU[[r]],]
  dim(meninglio.CSU.image3.2.test) #38 x 7
  data.temp.CSU.2.test = meninglio.CSU.image3.2.test
  data.temp.CSU.2.test$dis.lab = factor(ifelse(meninglio.CSU.image3.2.test$dis.lab == 0,"Glio","Menin"))
  data.temp.CSU.2.test$sexMF = factor(ifelse(meninglio.CSU.image3.2.test$sexMF == 0,"F","M"))
  data.temp.CSU.2.test$breedTypeBO = factor(ifelse(meninglio.CSU.image3.2.test$breedTypeBO == 0,"Non-B","Brachy"))
  dim(data.temp.CSU.2.test) #38 x 7
  
  #Fit logistic model on the training data
  train_control = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 25,
                               classProbs = TRUE
                               #sampling = "up",
                               #search = "random"
  )
  
  
  #Scenario 2: 3 image variables + 3 clinical covariates
  mtry <- 1:ncol(meninglio.CSU.image3.2.train[,-4])
  tunegrid <- expand.grid(.mtry=mtry)
  
  set.seed(2*r)
  log.model.CSU.train.2 <- train(x = meninglio.CSU.image3.2.train[,-4],
                                 y = meninglio.CSU.image3.2.train[,4],
                                 #data = meninglio.CSU.image3.1.train,
                                 trControl = train_control,
                                 #preProcess = c("center", "scale"),
                                 method = 'rf',
                                 metric = "Accuracy",
                                 tuneGrid = tunegrid
                                 #family = binomial()
  )
  
  
  #Predictions
  log.predictions.CSU.test.2 <- predict(log.model.CSU.train.2, data.temp.CSU.2.test[,-4])
  head(log.predictions.CSU.test.2)
  
  #Confusion Matrix
  #Test set: CSU
  confMat.test.CSU.2.ub = confusionMatrix(log.predictions.CSU.test.2, data.temp.CSU.2.test$dis.lab)

  #Store the results
  accuracy.2.ub[r,] = confMat.test.CSU.2.ub$overall[c(1,3,4,7)]
  sensitivityGlio.2.ub[r] = confMat.test.CSU.2.ub$byClass[1]
  specificityMenin.2.ub[r] = confMat.test.CSU.2.ub$byClass[2]
}
toc = proc.time() - tic
toc

round(median(sensitivityGlio.2.ub), 3)
round(mad(sensitivityGlio.2.ub), 3)
round(mean(sensitivityGlio.2.ub), 3)
round(sd(sensitivityGlio.2.ub), 3)

round(median(specificityMenin.2.ub), 3)
round(mad(specificityMenin.2.ub), 3)
round(mean(specificityMenin.2.ub), 3)
round(sd(specificityMenin.2.ub), 3)

round(median(accuracy.2.ub[,1]), 3)
round(mad(accuracy.2.ub[,1]), 3)
round(mean(accuracy.2.ub[,1]), 3)
round(sd(accuracy.2.ub[,1]), 3)

median(accuracy.2.ub[,1]) 
accuracy.2.ub[order(accuracy.2.ub[,1])[38]] 
sensitivityGlio.2.ub[order(accuracy.2.ub[,1])[38]] 
specificityMenin.2.ub[order(accuracy.2.ub[,1])[38]] 


#save.image("preComBat_3img3clin_LowerUpper_050721.RData")
#save.image("preComBat_3img3clin_LowerUpper_090121.RData")







