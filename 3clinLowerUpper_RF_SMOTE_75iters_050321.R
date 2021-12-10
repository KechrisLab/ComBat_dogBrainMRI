rm(list=ls())
load("dataPreprocessing_CCTSIproject_v2.RData")
library(performanceEstimation)
library(caret)
#Calculate performance accuracy over REP test sets (nGlio=19, nMenin=19)
#Baseline lower bound
#set.seed(20210216)
REP = 75

accuracy.0.lb = matrix(NA, REP, 4)
sensitivityGlio.0.lb = specificityMenin.0.lb = rep(NA, REP)

ind.meninglio.test.CSU = vector(mode = "list", length=REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  #Test data: CSU
  menin.CSU.test = sample(nrow(menin.CSU.image3), 19)
  glio.CSU.test = sample((nrow(menin.CSU.image3)+1):nrow(meninglio.CSU.image3.1), 19)
  ind.meninglio.test.CSU[[r]] = c(menin.CSU.test, glio.CSU.test)
  
  #Scenario 2: 3 image variables + 3 biological covariates
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
  
  meninglio.CSU.image3.2.test = meninglio.CSU.image3.2[ind.meninglio.test.CSU[[r]], ]
  dim(meninglio.CSU.image3.2.test) #38 x 7
  data.temp.CSU.2.test = meninglio.CSU.image3.2.test
  data.temp.CSU.2.test$dis.lab = factor(ifelse(meninglio.CSU.image3.2.test$dis.lab == 0,"Glio","Menin"))
  data.temp.CSU.2.test$breedTypeBO = factor(ifelse(meninglio.CSU.image3.2.test$breedTypeBO == 0,"Non-B","Brachy"))
  data.temp.CSU.2.test$sexMF = factor(ifelse(meninglio.CSU.image3.2.test$sexMF == 0,"F","M"))
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
  
  
  
  #Scenario 0: 3 clinical covariates only
  mtry <- 1:ncol(meninglio.Out.image3.2.train[,5:7])
  tunegrid <- expand.grid(.mtry=mtry)
  
  set.seed(2*r)
  log.model.Out.train.0 <- train(x = meninglio.Out.image3.2.train[,5:7],
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
  log.predictions.CSU.test.0 <- predict(log.model.Out.train.0, data.temp.CSU.2.test[,5:7])
  head(log.predictions.CSU.test.0)
  
  #Confusion Matrix
  #3 image variables
  confMat.test.CSU.0 = confusionMatrix(log.predictions.CSU.test.0, data.temp.CSU.2.test$dis.lab)

  #Store the results
  #Test set: CSU
  accuracy.0.lb[r,] = confMat.test.CSU.0$overall[c(1,3,4,7)]
  sensitivityGlio.0.lb[r] = confMat.test.CSU.0$byClass[1]
  specificityMenin.0.lb[r] = confMat.test.CSU.0$byClass[2]
}
toc = proc.time() - tic
toc

round(median(sensitivityGlio.0.lb), 3)
round(mad(sensitivityGlio.0.lb), 3)
round(mean(sensitivityGlio.0.lb), 3)
round(sd(sensitivityGlio.0.lb), 3)

round(median(specificityMenin.0.lb), 3)
round(mad(specificityMenin.0.lb), 3)
round(mean(specificityMenin.0.lb), 3)
round(sd(specificityMenin.0.lb), 3)

round(median(accuracy.0.lb[,1]), 3)
round(mad(accuracy.0.lb[,1]), 3)
round(mean(accuracy.0.lb[,1]), 3)
round(sd(accuracy.0.lb[,1]), 3)

median(accuracy.0.lb[,1]) 
accuracy.0.lb[order(accuracy.0.lb[,1])[38]]
sensitivityGlio.0.lb[order(accuracy.0.lb[,1])[38]]
specificityMenin.0.lb[order(accuracy.0.lb[,1])[38]]

#Calculate performance accuracy over REP test sets (nGlio=19, nMenin=19)
#Baseline UPPER BOUND
#set.seed(20210216)

REP = 75
accuracy.0.ub = matrix(NA, REP, 4)
sensitivityGlio.0.ub = specificityMenin.0.ub = rep(NA, REP)

tic = proc.time()
for(r in 1:REP){
  print(r)
  set.seed(2*r)
  
  
  #Training data: CSU; 20/79 Glio/Menin for training
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
  
  #Scenario 0: 3 clinical covariates
  mtry <- 1:ncol(meninglio.CSU.image3.2.train[,5:7])
  tunegrid <- expand.grid(.mtry=mtry)
  
  set.seed(2*r)
  log.model.CSU.train.0 <- train(x = meninglio.CSU.image3.2.train[,5:7],
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
  log.predictions.CSU.test.0 <- predict(log.model.CSU.train.0, data.temp.CSU.2.test[,5:7])
  head(log.predictions.CSU.test.0)

  #Confusion Matrix
  #Test set: CSU
  confMat.test.CSU.0.ub = confusionMatrix(log.predictions.CSU.test.0, data.temp.CSU.2.test$dis.lab)

  accuracy.0.ub[r,] = confMat.test.CSU.0.ub$overall[c(1,3,4,7)]
  sensitivityGlio.0.ub[r] = confMat.test.CSU.0.ub$byClass[1]
  specificityMenin.0.ub[r] = confMat.test.CSU.0.ub$byClass[2]
}
toc = proc.time() - tic
toc

round(median(sensitivityGlio.0.ub), 3)
round(mad(sensitivityGlio.0.ub), 3)
round(mean(sensitivityGlio.0.ub), 3)
round(sd(sensitivityGlio.0.ub), 3)

round(median(specificityMenin.0.ub), 3)
round(mad(specificityMenin.0.ub), 3)
round(mean(specificityMenin.0.ub), 3)
round(sd(specificityMenin.0.ub), 3)

round(median(accuracy.0.ub[,1]), 3)
round(mad(accuracy.0.ub[,1]), 3)
round(mean(accuracy.0.ub[,1]), 3)
round(sd(accuracy.0.ub[,1]), 3)

median(accuracy.0.ub[,1]) 
accuracy.0.ub[order(accuracy.0.ub[,1])[38]]
sensitivityGlio.0.ub[order(accuracy.0.ub[,1])[38]]
specificityMenin.0.ub[order(accuracy.0.ub[,1])[38]]

#save.image("3clinLowerUpper_RF_SMOTE_75iters_051221.RData")
load("3clinLowerUpper_RF_SMOTE_75iters_051221.RData")

#Scenario 0: 3 clinical variables 
png(filename = "all.0.LowerUpper_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 8, width = 12)
dat.all.0.pre = stack(data.frame(    sens.LB = sensitivityGlio.0.lb,
                                     sens.UB = sensitivityGlio.0.ub,
  
                                     spec.LB = specificityMenin.0.lb,
                                     spec.UB = specificityMenin.0.ub,
                                     
                                     tacc.LB = accuracy.0.lb[,1],
                                     tacc.UB = accuracy.0.ub[,1]
))

library(ggplot2)
bp.dat.all.0.pre = ggplot(dat.all.0.pre, aes(x = ind, y = values)) + 
  #geom_violin(width=1.4) + 
  geom_boxplot(color = c(2,3, 2,3, 2,3), alpha=0.5, outlier.color = 2, notch = F)  
#geom_abline(intercept = 26/45) 
bp.dat.all.0.pre = bp.dat.all.0.pre + labs(y="Values", x = "Classification performance metrics (lower/upper bounds)") +
  scale_y_continuous(breaks = seq(0,1, by = 0.05)) +
  labs(title = "Boxplots of Spec/Sens/Tot-Acc across 75 repetitions\nRF classification: Glioma/Meningioma\n(only clinical covariates in RF model)")
bp.dat.all.0.pre +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25), panel.grid = element_blank()) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 25), panel.grid = element_blank()) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 25), panel.grid = element_blank()) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 20), panel.grid = element_blank()) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 20), panel.grid = element_blank())
dev.off()




#Wilcoxon's rank-sum paired tests
boxplot(sensitivityGlio.0.lb, sensitivityGlio.0.ub)
median(sensitivityGlio.0.lb) #0.4210526
median(sensitivityGlio.0.ub) #0.4736842
wilcox.test(sensitivityGlio.0.lb, sensitivityGlio.0.ub, 
            alternative = "less",
            paired = T) #p-val=0.1326

boxplot(specificityMenin.0.lb, specificityMenin.0.ub)
median(specificityMenin.0.lb) #0.7368421
median(specificityMenin.0.ub) #0.7368421
wilcox.test(specificityMenin.0.lb, specificityMenin.0.ub, 
            alternative = "less",
            paired = T) #p-val=0.8836

boxplot(accuracy.0.lb[,1], accuracy.0.ub[,1])
median(accuracy.0.lb[,1]) #0.5789474
median(accuracy.0.ub[,1]) #0.5789474
wilcox.test(accuracy.0.lb[,1], accuracy.0.ub[,1], 
            alternative = "less",
            paired = T) #p-val = 0.3319



#Agemos across CSU and Outside
boxplot(meninglio.CSU.image3.2$agemos[(meninglio.CSU.image3.2$dis.lab == 0)], 
        meninglio.CSU.image3.2$agemos[(meninglio.CSU.image3.2$dis.lab == 1)])
wilcox.test(meninglio.CSU.image3.2$agemos[(meninglio.CSU.image3.2$dis.lab == 0)], 
            meninglio.CSU.image3.2$agemos[(meninglio.CSU.image3.2$dis.lab == 1)],
            alternative = "less") #p-value = 0.03426

boxplot(meninglio.Out.image3.2$agemos[(meninglio.Out.image3.2$dis.lab == 0)], 
        meninglio.Out.image3.2$agemos[(meninglio.Out.image3.2$dis.lab == 1)])
wilcox.test(meninglio.Out.image3.2$agemos[(meninglio.Out.image3.2$dis.lab == 0)], 
            meninglio.Out.image3.2$agemos[(meninglio.Out.image3.2$dis.lab == 1)],
            alternative = "less") #p-value = 0.009632

#Sex across CSU and Outside
sexTab.CSUOut = matrix(NA, 2, 2)
rownames(sexTab.CSUOut) = c("CSU", "Out")
colnames(sexTab.CSUOut) = c("F", "M")
sexTab.CSUOut[1, ] = table(meninglio.CSU.image3.2$sexMF)
sexTab.CSUOut[2, ] = table(meninglio.Out.image3.2$sexMF)
sexTab.CSUOut = as.table(sexTab.CSUOut)
chisq.test(sexTab.CSUOut,correct=T) #0.7619

#Breedtype across CSU and Outside
breed.CSUOut = matrix(NA, 2, 2)
rownames(breed.CSUOut) = c("CSU", "Out")
colnames(breed.CSUOut) = c("NB", "B")
breed.CSUOut[1, ] = table(meninglio.CSU.image3.2$breedTypeBO)
breed.CSUOut[2, ] = table(meninglio.Out.image3.2$breedTypeBO)
breed.CSUOut = as.table(breed.CSUOut)
chisq.test(breed.CSUOut,correct=T) #0.6038

