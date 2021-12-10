#ComBat data harmonization across labs
rm(list=ls())
load("dataPreprocessing_CCTSIproject_v2.RData")
library(performanceEstimation)
library(caret)

#Scenario 1: 3 image variables ONLY used in ComBat, classification model: 3 img variables
#Calculate performance accuracy over REP test sets
#Baseline lower bound
#set.seed(20210216)
REP = 75

accuracy.1.lb = matrix(NA, REP, 4)
sensitivityGlio.1.lb = specificityMenin.1.lb = rep(NA, REP)

ind.meninglio.test.CSU = vector(mode = "list", length=REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  #Test data: CSU
  menin.CSU.test = sample(nrow(menin.CSU.image3), 19)
  glio.CSU.test = sample((nrow(menin.CSU.image3)+1):nrow(meninglio.CSU.image3.1), 19)
  ind.meninglio.test.CSU[[r]] = c(menin.CSU.test, glio.CSU.test)
  
  #Scenario 1: 3 image variables
  #Training data: Outside; use all the outside samples for training
  data.temp.1 = meninglio.Out.image3.1
  dim(data.temp.1) #99 x 4
  #SMOTE on training data
  data.temp.1$dis.lab = factor(ifelse(meninglio.Out.image3.1$dis.lab == 0,"Glio","Menin"))
  newData.1 <- smote(dis.lab ~ ., data = data.temp.1, perc.over = 3, perc.under = 1.45)
  table(newData.1$dis.lab)

  meninglio.Out.image3.1.train = rbind(data.temp.1[which(data.temp.1$dis.lab == "Menin"),],
                                       newData.1[which(newData.1$dis.lab == "Glio"),])
  #dim(meninglio.Out.image3.1.train)
  #colnames(meninglio.Out.image3.1.train)

  #Test Data: Use 19/19 Glio/Menin from CSU
  meninglio.CSU.image3.1.test = meninglio.CSU.image3.1[ind.meninglio.test.CSU[[r]],]
  dim(meninglio.CSU.image3.1.test) #38 x 4
  data.temp.CSU.1.test = meninglio.CSU.image3.1.test
  data.temp.CSU.1.test$dis.lab = factor(ifelse(meninglio.CSU.image3.1.test$dis.lab == 0,"Glio","Menin"))
  dim(data.temp.CSU.1.test)
  
  #Fit logistic model on the training data
  train_control = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 25,
                               classProbs = TRUE
                               #seeds = 2*r
                               #sampling = "up",
                               #search = "random"
  )
  
  #Scenario 1: Only 3 image variables
  mtry <- 1:ncol(meninglio.Out.image3.1.train[,-4])
  tunegrid <- expand.grid(.mtry=mtry)

  set.seed(2*r)
  log.model.Out.train.1 <- train(x = meninglio.Out.image3.1.train[,-4],
                                 y = meninglio.Out.image3.1.train[,4],
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
  log.predictions.CSU.test.1 <- predict(log.model.Out.train.1, data.temp.CSU.1.test[,-4])
  head(log.predictions.CSU.test.1)
  
  #Confusion Matrix
  #3 image variables
   confMat.test.CSU.1 = confusionMatrix(log.predictions.CSU.test.1, data.temp.CSU.1.test$dis.lab)
  
   #Store the results
  #Test set: CSU
   accuracy.1.lb[r,] = confMat.test.CSU.1$overall[c(1,3,4,7)]
   sensitivityGlio.1.lb[r] = confMat.test.CSU.1$byClass[1]
   specificityMenin.1.lb[r] = confMat.test.CSU.1$byClass[2]
}
toc = proc.time() - tic
toc

round(median(sensitivityGlio.1.lb), 3)
round(mad(sensitivityGlio.1.lb), 3)
round(mean(sensitivityGlio.1.lb), 3)
round(sd(sensitivityGlio.1.lb), 3)

round(median(specificityMenin.1.lb), 3)
round(mad(specificityMenin.1.lb), 3)
round(mean(specificityMenin.1.lb), 3)
round(sd(specificityMenin.1.lb), 3)

round(median(accuracy.1.lb[,1]), 3)
round(mad(accuracy.1.lb[,1]), 3)
round(mean(accuracy.1.lb[,1]), 3)
round(sd(accuracy.1.lb[,1]), 3)

median(accuracy.1.lb[,1]) 
accuracy.1.lb[order(accuracy.1.lb[,1])[38]]
sensitivityGlio.1.lb[order(accuracy.1.lb[,1])[38]]
specificityMenin.1.lb[order(accuracy.1.lb[,1])[38]]

#Calculate performance accuracy over REP test sets
#Baseline UPPER BOUND
#set.seed(20210216)

REP=75
accuracy.1.ub = matrix(NA, REP, 4)
sensitivityGlio.1.ub = specificityMenin.1.ub = rep(NA, REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  #Test data: CSU
  menin.CSU.test = sample(nrow(menin.CSU.image3), 19)
  glio.CSU.test = sample((nrow(menin.CSU.image3)+1):nrow(meninglio.CSU.image3.1), 19)
  ind.meninglio.test.CSU[[r]] = c(menin.CSU.test, glio.CSU.test)
  
  #Training data: CSU; 20/79 Glio/Menin for training
  #Scenario 1: 3 image variables
  meninglio.CSU.image3.1.train.pre = meninglio.CSU.image3.1[-ind.meninglio.test.CSU[[r]],]
  data.temp.1 = meninglio.CSU.image3.1.train.pre[c(sample(87, 79), 88:107), ]
  dim(data.temp.1) #99 x 4
  #SMOTE on training data
  data.temp.1$dis.lab = factor(ifelse(data.temp.1$dis.lab == 0,"Glio","Menin"))
  newData.1 <- smote(dis.lab ~ ., data = data.temp.1, perc.over = 3, perc.under = 1.45)
  table(newData.1$dis.lab)

  meninglio.CSU.image3.1.train = rbind(data.temp.1[which(data.temp.1$dis.lab == "Menin"),],
                                       newData.1[which(newData.1$dis.lab == "Glio"),])
  dim(meninglio.CSU.image3.1.train)

  #Test data: CSU -- 19/19
  meninglio.CSU.image3.1.test = meninglio.CSU.image3.1[ind.meninglio.test.CSU[[r]],]
  dim(meninglio.CSU.image3.1.test) #38 x 4
  data.temp.CSU.1.test = meninglio.CSU.image3.1.test
  data.temp.CSU.1.test$dis.lab = factor(ifelse(meninglio.CSU.image3.1.test$dis.lab == 0,"Glio","Menin"))
  dim(data.temp.CSU.1.test) #38 x 4
  
  
  #Fit logistic model on the training data
  train_control = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 25,
                               classProbs = TRUE
                               #sampling = "up",
                               #search = "random"
  )
  #Scenario 1: Only 3 image variables
  mtry <- 1:ncol(meninglio.CSU.image3.1.train[,-4])
  tunegrid <- expand.grid(.mtry=mtry)

  set.seed(2*r)
  log.model.CSU.train.1 <- train(x = meninglio.CSU.image3.1.train[,-4],
                                 y = meninglio.CSU.image3.1.train[,4],
                                 #data = meninglio.CSU.image3.1.train,
                                 trControl = train_control,
                                 #preProcess = c("center", "scale"),
                                 method = 'rf',
                                 metric = "Accuracy",
                                 tuneGrid = tunegrid
                                 #family = binomial()
  )
  
  #Predictions
  log.predictions.CSU.test.1 <- predict(log.model.CSU.train.1, data.temp.CSU.1.test[,-4])
  head(log.predictions.CSU.test.1)
  
  #Confusion Matrix
  #Test set: CSU
   confMat.test.CSU.1.ub = confusionMatrix(log.predictions.CSU.test.1, data.temp.CSU.1.test$dis.lab)
  
   #Store the results
   accuracy.1.ub[r,] = confMat.test.CSU.1.ub$overall[c(1,3,4,7)]
   sensitivityGlio.1.ub[r] = confMat.test.CSU.1.ub$byClass[1]
   specificityMenin.1.ub[r] = confMat.test.CSU.1.ub$byClass[2]
}
toc = proc.time() - tic
toc

round(median(sensitivityGlio.1.ub), 3)
round(mad(sensitivityGlio.1.ub), 3)
round(mean(sensitivityGlio.1.ub), 3)
round(sd(sensitivityGlio.1.ub), 3)

round(median(specificityMenin.1.ub), 3)
round(mad(specificityMenin.1.ub), 3)
round(mean(specificityMenin.1.ub), 3)
round(sd(specificityMenin.1.ub), 3)

round(median(accuracy.1.ub[,1]), 3)
round(mad(accuracy.1.ub[,1]), 3)
round(mean(accuracy.1.ub[,1]), 3)
round(sd(accuracy.1.ub[,1]), 3)

median(accuracy.1.ub[,1]) 
accuracy.1.ub[order(accuracy.1.ub[,1])[38]]
sensitivityGlio.1.ub[order(accuracy.1.ub[,1])[38]]
specificityMenin.1.ub[order(accuracy.1.ub[,1])[38]]


#POST ComBat HARMONIZATION: LOWER BOUND
#library(devtools)
#install_github("jfortin1/CombatHarmonization/R/neuroCombat")
library(neuroCombat)
batch = c(rep(0,nrow(meninglio.CSU.image3.1)), rep(1,nrow(meninglio.Out.image3.1))) #Batch variable for the scanner id
table(batch)

#Scenario 1: 3 image vars ONLY in ComBat 
#No clinical covariates used in ComBat
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

meninglio.CSU.image3.1.post.pf = as.data.frame(dat.1.post.pf[1:nrow(meninglio.CSU.image3.1),])
meninglio.CSU.image3.1.post.pf$dis.lab = meninglio.CSU.image3.1$dis.lab
meninglio.Out.image3.1.post.pf = as.data.frame(dat.1.post.pf[-c(1:nrow(meninglio.CSU.image3.1)),])
meninglio.Out.image3.1.post.pf$dis.lab = meninglio.Out.image3.1$dis.lab
dim(meninglio.CSU.image3.1.post.pf) #145 x 4
dim(meninglio.Out.image3.1.post.pf) #99 x 4


#Calculate performance accuracy over REP test sets
REP = 75

accuracy.1.lb.post = matrix(NA, REP, 4)
sensitivityGlio.1.lb.post = specificityMenin.1.lb.post = rep(NA, REP)

accuracy.1.lb.post.pf = matrix(NA, REP, 4)
sensitivityGlio.1.lb.post.pf = specificityMenin.1.lb.post.pf = rep(NA, REP)

ind.meninglio.test.CSU = vector(mode = "list", length=REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  #Test data: CSU
  menin.CSU.test = sample(sum(meninglio.CSU.image3.1.post$dis.lab == 1), 19)
  glio.CSU.test = sample((sum(meninglio.CSU.image3.1.post$dis.lab == 1)+1):nrow(meninglio.CSU.image3.1.post), 19)
  ind.meninglio.test.CSU[[r]] = c(menin.CSU.test, glio.CSU.test)
  
  #Scenario 1: 3 image variables
  #Training data: Outside; use all the outside samples for training
  data.temp.1 = meninglio.Out.image3.1.post
  dim(data.temp.1) #99 x 4
  #SMOTE on training data
  data.temp.1$dis.lab = factor(ifelse(meninglio.Out.image3.1.post$dis.lab == 0,"Glio","Menin"))
  newData.1 <- smote(dis.lab ~ ., data = data.temp.1, perc.over = 3, perc.under = 1.45)
  table(newData.1$dis.lab)

  meninglio.Out.image3.1.train.post = rbind(data.temp.1[which(data.temp.1$dis.lab == "Menin"),],
                                       newData.1[which(newData.1$dis.lab == "Glio"),])
  #dim(meninglio.Out.image3.1.train)
  #colnames(meninglio.Out.image3.1.train)

  #Test Data: Use 19/19 Glio/Menin from CSU
  meninglio.CSU.image3.1.test.post = meninglio.CSU.image3.1.post[ind.meninglio.test.CSU[[r]],]
  dim(meninglio.CSU.image3.1.test.post) #38 x 4
  data.temp.CSU.1.test = meninglio.CSU.image3.1.test.post
  data.temp.CSU.1.test$dis.lab = factor(ifelse(meninglio.CSU.image3.1.test.post$dis.lab == 0,"Glio","Menin"))
  dim(data.temp.CSU.1.test) #38 x 4
  
  #Scenario 1-PF: 3 image variables in ComBat, 3 image vars in classification model
  #Training data: Outside; use all the outside samples for training
  data.temp.1.pf = meninglio.Out.image3.1.post.pf
  dim(data.temp.1.pf) #99 x 4
  #SMOTE on training data
  data.temp.1.pf$dis.lab = factor(ifelse(meninglio.Out.image3.1.post.pf$dis.lab == 0,"Glio","Menin"))
  newData.1.pf <- smote(dis.lab ~ ., data = data.temp.1.pf, perc.over = 3, perc.under = 1.45)
  table(newData.1.pf$dis.lab)
  
  meninglio.Out.image3.1.train.post.pf = rbind(data.temp.1.pf[which(data.temp.1.pf$dis.lab == "Menin"),],
                                            newData.1.pf[which(newData.1.pf$dis.lab == "Glio"),])
  #dim(meninglio.Out.image3.1.train)
  #colnames(meninglio.Out.image3.1.train)
  
  #Test Data: Use 19/19 Glio/Menin from CSU
  meninglio.CSU.image3.1.test.post.pf = meninglio.CSU.image3.1.post.pf[ind.meninglio.test.CSU[[r]],]
  dim(meninglio.CSU.image3.1.test.post.pf) #38 x 4
  data.temp.CSU.1.test.pf = meninglio.CSU.image3.1.test.post.pf
  data.temp.CSU.1.test.pf$dis.lab = factor(ifelse(meninglio.CSU.image3.1.test.post.pf$dis.lab == 0,"Glio","Menin"))
  dim(data.temp.CSU.1.test.pf) #38 x 4
  
  #Fit logistic model on the training data
  train_control = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 25,
                               classProbs = TRUE
                               #seeds = 2*r
                               #sampling = "up",
                               #search = "random"
  )
  
  #Scenario 1: Only 3 image variables in ComBat and classification model
  mtry <- 1:ncol(meninglio.Out.image3.1.train.post[,-4])
  tunegrid <- expand.grid(.mtry=mtry)

  set.seed(2*r)
  log.model.Out.train.1.post <- train(x = meninglio.Out.image3.1.train.post[,-4],
                                 y = meninglio.Out.image3.1.train.post[,4],
                                 #data = meninglio.CSU.image3.1.train,
                                 trControl = train_control,
                                 #preProcess = c("center", "scale"),
                                 method = 'rf',
                                 metric = "Accuracy",
                                 tuneGrid = tunegrid
                                 #family = binomial()
  )

  set.seed(2*r)
  log.model.Out.train.1.post.pf <- train(x = meninglio.Out.image3.1.train.post.pf[,-4],
                                      y = meninglio.Out.image3.1.train.post.pf[,4],
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
  log.predictions.CSU.test.1 <- predict(log.model.Out.train.1.post, data.temp.CSU.1.test[,-4])
  head(log.predictions.CSU.test.1)
  log.predictions.CSU.test.1.pf <- predict(log.model.Out.train.1.post.pf, data.temp.CSU.1.test.pf[,-4])
  head(log.predictions.CSU.test.1.pf)
  
  
  #Confusion Matrix
  #3 image variables
  confMat.test.CSU.1 = confusionMatrix(log.predictions.CSU.test.1, data.temp.CSU.1.test$dis.lab)
  confMat.test.CSU.1.pf = confusionMatrix(log.predictions.CSU.test.1.pf, data.temp.CSU.1.test.pf$dis.lab)
  
  #Store the results
  #Test set: CSU
  #Accuracy
  accuracy.1.lb.post[r,] = confMat.test.CSU.1$overall[c(1,3,4,7)]
  accuracy.1.lb.post.pf[r,] = confMat.test.CSU.1.pf$overall[c(1,3,4,7)]
  
  #Sensitivity
  sensitivityGlio.1.lb.post[r]  = confMat.test.CSU.1$byClass[1]
  sensitivityGlio.1.lb.post.pf[r]  = confMat.test.CSU.1.pf$byClass[1]
  
  #Specificity
  specificityMenin.1.lb.post[r] = confMat.test.CSU.1$byClass[2]
  specificityMenin.1.lb.post.pf[r] = confMat.test.CSU.1.pf$byClass[2]
}
toc = proc.time() - tic
toc

round(median(sensitivityGlio.1.lb.post), 3)
round(mad(sensitivityGlio.1.lb.post), 3)
round(mean(sensitivityGlio.1.lb.post), 3)
round(sd(sensitivityGlio.1.lb.post), 3)

round(median(specificityMenin.1.lb.post), 3)
round(mad(specificityMenin.1.lb.post), 3)
round(mean(specificityMenin.1.lb.post), 3)
round(sd(specificityMenin.1.lb.post), 3)

round(median(accuracy.1.lb.post[,1]), 3)
round(mad(accuracy.1.lb.post[,1]), 3)
round(mean(accuracy.1.lb.post[,1]), 3)
round(sd(accuracy.1.lb.post[,1]), 3)

median(accuracy.1.lb.post[,1]) 
accuracy.1.lb.post[order(accuracy.1.lb.post[,1])[38]]
sensitivityGlio.1.lb.post[order(accuracy.1.lb.post[,1])[38]]
specificityMenin.1.lb.post[order(accuracy.1.lb.post[,1])[38]]


#Scenario 1-PF
round(median(sensitivityGlio.1.lb.post.pf), 3)
round(mad(sensitivityGlio.1.lb.post.pf), 3)
round(mean(sensitivityGlio.1.lb.post.pf), 3)
round(sd(sensitivityGlio.1.lb.post.pf), 3)

round(median(specificityMenin.1.lb.post.pf), 3)
round(mad(specificityMenin.1.lb.post.pf), 3)
round(mean(specificityMenin.1.lb.post.pf), 3)
round(sd(specificityMenin.1.lb.post.pf), 3)

round(median(accuracy.1.lb.post.pf[,1]), 3)
round(mad(accuracy.1.lb.post.pf[,1]), 3)
round(mean(accuracy.1.lb.post.pf[,1]), 3)
round(sd(accuracy.1.lb.post.pf[,1]), 3)

median(accuracy.1.lb.post.pf[,1]) 
accuracy.1.lb.post.pf[order(accuracy.1.lb.post.pf[,1])[38]] #0.553
sensitivityGlio.1.lb.post.pf[order(accuracy.1.lb.post.pf[,1])[38]] #0.368
specificityMenin.1.lb.post.pf[order(accuracy.1.lb.post.pf[,1])[38]] #0.737


#save.image("3imgVars_prepostComBat_051221.RData")
#save.image("3imgVars_prepostComBat_090221.RData")

library(ggplot2)
#Figure 1A
#Scenario 1-a: 3 image vars
png(filename = "Fig-1A_all-1a_prepost_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 6, width = 10)
dat.all.1.prepost = stack(data.frame(
                                     #sens.lb.0 = sensitivityGlio.0.lb,
                                     #sens.ub.0 = sensitivityGlio.0.ub,
                                     Sens.lb = sensitivityGlio.1.lb,
                                     Sens.lb.CB = sensitivityGlio.1.lb.post,
                                     Sens.ub = sensitivityGlio.1.ub,
                                     #spec.lb.0 = specificityMenin.0.lb,
                                     #spec.ub.0 = specificityMenin.0.ub,
                                     Spec.lb = specificityMenin.1.lb,
                                     Spec.lb.CB = specificityMenin.1.lb.post,
                                     Spec.ub = specificityMenin.1.ub,
                                     #accu.lb.0 = accuracy.0.lb[,1],
                                     #accu.ub.0 = accuracy.0.ub[,1],
                                     TotAcc.lb = accuracy.1.lb[,1],
                                     TotAcc.lb.CB = accuracy.1.lb.post[,1],
                                     TotAcc.ub = accuracy.1.ub[,1]
))
bp.dat.all.1.prepost = ggplot(dat.all.1.prepost, aes(x = ind, y = values)) + 
  #geom_violin(width=1.4) + 
  geom_boxplot(color = c(rep(c(1,2,3),3)), alpha=0.5, outlier.color = 2, notch = F)  
#geom_abline(intercept = 26/45) 
bp.dat.all.1.prepost = bp.dat.all.1.prepost + labs(y="RF classification performance", x = "Classification metrics") +
  scale_y_continuous(breaks = seq(0,1, by = 0.05)) +
  labs(title = "Boxplots of Spec/Sens/Tot-Acc metrics over 75 runs\nRF classification: Glioma/Meningioma\n[Scenario 1-a: 3 Image variables in ComBat and RF model]")
bp.dat.all.1.prepost +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), panel.grid = element_blank())
dev.off()


#Wilcoxon's rank-sum test
#LowerB: Pre and post ComBat
wilcox.test(sensitivityGlio.1.lb, sensitivityGlio.1.lb.post, 
            alternative = "less",
            paired = T) #p-val=4.329E-8
wilcox.test(specificityMenin.1.lb, specificityMenin.1.lb.post, 
            alternative = "less",
            paired = T) #p-val=0.001157
wilcox.test(accuracy.1.lb[,1], accuracy.1.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val = 0.6704

#LowerB-Post ComBat and UpperB
wilcox.test(sensitivityGlio.1.lb.post, sensitivityGlio.1.ub, 
            alternative = "less",
            paired = T) #p-val=0.9996
wilcox.test(specificityMenin.1.lb.post, specificityMenin.1.ub, 
            alternative = "less",
            paired = T) #p-val=0.0.004986
wilcox.test(accuracy.1.lb.post[,1], accuracy.1.ub[,1], 
            alternative = "less",
            paired = T) #p-val = 0.8611

#Case 0 vs Case 1
load("3clinLowerUpper_RF_SMOTE_75iters_051221.RData")

#Sensitivity
median(sensitivityGlio.0.lb) #0.4210526
median(sensitivityGlio.1.lb) #0.4736842
wilcox.test(sensitivityGlio.0.lb, sensitivityGlio.1.lb, 
            alternative = "less",
            paired = T) #p-val=0.0009685

median(sensitivityGlio.0.ub) #0.4736842
median(sensitivityGlio.1.ub) #0.5263158
wilcox.test(sensitivityGlio.0.ub, sensitivityGlio.1.ub, 
            alternative = "less",
            paired = T) #p-val=0.0006576

#Pre- & post-ComBat: Case 1a
median(sensitivityGlio.1.lb) #0.4736842
median(sensitivityGlio.1.lb.post) #0.5789474
wilcox.test(sensitivityGlio.1.lb, sensitivityGlio.1.lb.post, 
            alternative = "less",
            paired = T) #p-val=4.329e-08

#Post-ComBat (Case 1a) vs Case 0
median(sensitivityGlio.0.lb) #0.4210526
median(sensitivityGlio.1.lb.post) #0.5789474
wilcox.test(sensitivityGlio.0.lb, sensitivityGlio.1.lb.post, 
            alternative = "less",
            paired = T) #p-val=7.883e-11

#Specificity
median(specificityMenin.0.lb) #0.7368421
median(specificityMenin.1.lb) #0.6842105
wilcox.test(specificityMenin.1.lb, specificityMenin.0.lb, 
            alternative = "less",
            paired = T) #p-val=0.00331

median(specificityMenin.0.ub) #0.7368421
median(specificityMenin.1.ub) #0.7894737
wilcox.test(specificityMenin.0.ub, specificityMenin.1.ub, 
            alternative = "less",
            paired = T) #5.668e-05

#Pre- & post-ComBat: Case 1a
median(specificityMenin.1.lb) #0.6842105
median(specificityMenin.1.lb.post) #0.7368421
wilcox.test(specificityMenin.1.lb, specificityMenin.1.lb.post, 
            alternative = "less",
            paired = T) #p-val=0.001157

#Post-ComBat (Case 1a) vs Case 0
median(specificityMenin.0.lb) #0.7368421
median(specificityMenin.1.lb.post) #0.7368421
wilcox.test(specificityMenin.0.lb, specificityMenin.1.lb.post, 
            alternative = "less",
            paired = T) #p-val=0.3467


#Total accuracy
median(accuracy.0.lb[,1]) #0.5789474
median(accuracy.1.lb[,1]) #0.6052632
wilcox.test(accuracy.0.lb[,1], accuracy.1.lb[,1], 
            alternative = "less",
            paired = T) #p-val = 0.2702

median(accuracy.0.ub[,1]) #0.5789474
median(accuracy.1.ub[,1]) #0.6578947
wilcox.test(accuracy.0.ub[,1], accuracy.1.ub[,1], 
            alternative = "less",
            paired = T) #p-val = 4.065e-07

#Pre- and post-ComBat: Case 1a
median(accuracy.1.lb[,1]) #0.6052632
median(accuracy.1.lb.post[,1]) #0.6578947
wilcox.test(accuracy.1.lb[,1], accuracy.1.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val = 2.636e-08

#Post-ComBat (Case 1a) vs Case 0
median(accuracy.0.lb[,1]) #0.5789474
median(accuracy.1.lb.post[,1]) #0.6578947
wilcox.test(accuracy.0.lb[,1], accuracy.1.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val = 4.98e-08
