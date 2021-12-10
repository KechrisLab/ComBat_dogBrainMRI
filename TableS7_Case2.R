#Check values for Table S7
load("ComBat_Scenario3_allcovs_090321.RData")
load("3imgVars_prepostComBat_090221.RData")
load("preComBat_3img3clin_LowerUpper_090121.RData")

#RF: 3 img vars
#LB
median(sensitivityGlio.1.lb)
mad(sensitivityGlio.1.lb)
median(specificityMenin.1.lb)
mad(specificityMenin.1.lb)
median(accuracy.1.lb[,1])
mad(accuracy.1.lb[,1])

#COMBAT.LB: 2a
median(sensitivityGlio.34.lb.post)
mad(sensitivityGlio.34.lb.post)
median(specificityMenin.34.lb.post)
mad(specificityMenin.34.lb.post)
median(accuracy.34.lb.post[,1])
mad(accuracy.34.lb.post[,1])

#quickTest: Case 1a vs. 2a
boxplot(sensitivityGlio.1.lb.post, sensitivityGlio.34.lb.post)
boxplot(specificityMenin.1.lb.post, specificityMenin.34.lb.post)
wilcox.test(specificityMenin.1.lb.post, specificityMenin.34.lb.post, 
            alternative = "less",
            paired = T) #p-val=0.01111
boxplot(accuracy.1.lb.post[,1], accuracy.34.lb.post[,1])

#COMBAT.LB: 2b
median(sensitivityGlio.31.lb.post)
mad(sensitivityGlio.31.lb.post)
median(specificityMenin.31.lb.post)
mad(specificityMenin.31.lb.post)
median(accuracy.31.lb.post[,1])
mad(accuracy.31.lb.post[,1])

#quickTest: Case 1b vs. Case 2b
boxplot(sensitivityGlio.23.lb.post, sensitivityGlio.31.lb.post)
boxplot(specificityMenin.23.lb.post, specificityMenin.31.lb.post)
wilcox.test(specificityMenin.23.lb.post, specificityMenin.31.lb.post, 
            alternative = "less",
            paired = T) #p-val=0.0332
boxplot(accuracy.23.lb.post[,1], accuracy.31.lb.post[,1])

#UB
median(sensitivityGlio.1.ub)
mad(sensitivityGlio.1.ub)
median(specificityMenin.1.ub)
mad(specificityMenin.1.ub)
median(accuracy.1.ub[,1])
mad(accuracy.1.ub[,1])

#RF: 3 img vars + 3 clin covs
#LB
median(sensitivityGlio.2.lb)
mad(sensitivityGlio.2.lb)
median(specificityMenin.2.lb)
mad(specificityMenin.2.lb)
median(accuracy.2.lb[,1])
mad(accuracy.2.lb[,1])

#COMBAT.LB: 2c
median(sensitivityGlio.32.lb.post)
mad(sensitivityGlio.32.lb.post)
median(specificityMenin.32.lb.post)
mad(specificityMenin.32.lb.post)
median(accuracy.32.lb.post[,1])
mad(accuracy.32.lb.post[,1])

range(accuracy.32.lb.post[,1])
      
#quickTest: Case 1c vs. 2c
boxplot(sensitivityGlio.2.lb.post, sensitivityGlio.32.lb.post)
boxplot(specificityMenin.2.lb.post, specificityMenin.32.lb.post)
wilcox.test(specificityMenin.2.lb.post, specificityMenin.32.lb.post, 
            alternative = "less",
            paired = T) #p-val=0.4016
boxplot(accuracy.2.lb.post[,1], accuracy.32.lb.post[,1])

#COMBAT.LB: 2d
median(sensitivityGlio.33.lb.post)
mad(sensitivityGlio.33.lb.post)
median(specificityMenin.33.lb.post)
mad(specificityMenin.33.lb.post)
median(accuracy.33.lb.post[,1])
mad(accuracy.33.lb.post[,1])

#quickTest: Case 1d vs. 2d
boxplot(sensitivityGlio.22.lb.post, sensitivityGlio.33.lb.post)
boxplot(specificityMenin.22.lb.post, specificityMenin.33.lb.post)
wilcox.test(specificityMenin.22.lb.post, specificityMenin.33.lb.post, 
            alternative = "less",
            paired = T) #p-val=0.003235
boxplot(accuracy.22.lb.post[,1], accuracy.33.lb.post[,1])

#UB
median(sensitivityGlio.2.ub)
mad(sensitivityGlio.2.ub)
median(specificityMenin.2.ub)
mad(specificityMenin.2.ub)
median(accuracy.2.ub[,1])
mad(accuracy.2.ub[,1])

#Check "bold" and "underline" in cells for Table S7
#Lower Bound pre- vs. post-ComBat (bold)
#Lower-Bound post-ComBat vs. Upper Bound (underline)

#2a
#sensitivity
median(sensitivityGlio.1.lb) 
median(sensitivityGlio.34.lb.post) 
wilcox.test(sensitivityGlio.1.lb, sensitivityGlio.34.lb.post, 
            alternative = "less",
            paired = T) #p-val = 0.1631

median(sensitivityGlio.34.lb.post) 
median(sensitivityGlio.1.ub)
wilcox.test(sensitivityGlio.34.lb.post, sensitivityGlio.1.ub, 
            alternative = "less",
            paired = T) #p-val = 0.08892

#specificity
median(specificityMenin.1.lb) 
median(specificityMenin.34.lb.post) 
wilcox.test(specificityMenin.1.lb, specificityMenin.34.lb.post, 
            alternative = "less",
            paired = T) #p-val = 5.683e-06

median(specificityMenin.34.lb.post) 
median(specificityMenin.1.ub)
wilcox.test(specificityMenin.34.lb.post, specificityMenin.1.ub, 
            alternative = "less",
            paired = T) #p-val = 0.2626

#total accuracy
median(accuracy.1.lb[,1]) 
median(accuracy.34.lb.post[,1]) 
wilcox.test(accuracy.1.lb[,1], accuracy.34.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val = 6.078e-05

median(accuracy.34.lb.post[,1]) 
median(accuracy.1.ub[,1])
wilcox.test(accuracy.34.lb.post[,1], accuracy.1.ub[,1], 
            alternative = "less",
            paired = T) #p-val = 0.1488

#2b
#sensitivity
median(sensitivityGlio.1.lb) 
median(sensitivityGlio.31.lb.post) 
wilcox.test(sensitivityGlio.1.lb, sensitivityGlio.31.lb.post, 
            alternative = "less",
            paired = T) #p-val = 0.5939

median(sensitivityGlio.31.lb.post) 
median(sensitivityGlio.1.ub)
wilcox.test(sensitivityGlio.31.lb.post, sensitivityGlio.1.ub, 
            alternative = "less",
            paired = T) #p-val = 0.008529

#specificity
median(specificityMenin.1.lb) 
median(specificityMenin.31.lb.post) 
wilcox.test(specificityMenin.1.lb, specificityMenin.31.lb.post, 
            alternative = "less",
            paired = T) #p-val = 5.574e-06

median(specificityMenin.31.lb.post) 
median(specificityMenin.1.ub)
wilcox.test(specificityMenin.31.lb.post, specificityMenin.1.ub, 
            alternative = "less",
            paired = T) #p-val = 0.08796

#total accuracy
median(accuracy.1.lb[,1]) 
median(accuracy.31.lb.post[,1]) 
wilcox.test(accuracy.1.lb[,1], accuracy.31.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val = 0.0001819

median(accuracy.31.lb.post[,1]) 
median(accuracy.1.ub[,1])
wilcox.test(accuracy.31.lb.post[,1], accuracy.1.ub[,1], 
            alternative = "less",
            paired = T) #p-val = 0.01359

#2c
#sensitivity
median(sensitivityGlio.2.lb) 
median(sensitivityGlio.32.lb.post) 
wilcox.test(sensitivityGlio.2.lb, sensitivityGlio.32.lb.post, 
            alternative = "less",
            paired = T) #p-val = 1

median(sensitivityGlio.32.lb.post) 
median(sensitivityGlio.2.ub)
wilcox.test(sensitivityGlio.32.lb.post, sensitivityGlio.2.ub, 
            alternative = "less",
            paired = T) #p-val = 3.113e-08

#specificity
median(specificityMenin.2.lb) 
median(specificityMenin.32.lb.post) 
wilcox.test(specificityMenin.2.lb, specificityMenin.32.lb.post, 
            alternative = "less",
            paired = T) #p-val = 1.172e-08

median(specificityMenin.32.lb.post) 
median(specificityMenin.2.ub)
wilcox.test(specificityMenin.32.lb.post, specificityMenin.2.ub, 
            alternative = "less",
            paired = T) #p-val = 1

#total accuracy
median(accuracy.2.lb[,1]) 
median(accuracy.32.lb.post[,1]) 
wilcox.test(accuracy.2.lb[,1], accuracy.32.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val = 0.4139

median(accuracy.32.lb.post[,1]) 
median(accuracy.2.ub[,1])
wilcox.test(accuracy.32.lb.post[,1], accuracy.2.ub[,1], 
            alternative = "less",
            paired = T) #p-val = 0.004882

#2d
#sensitivity
median(sensitivityGlio.2.lb) 
median(sensitivityGlio.33.lb.post) 
wilcox.test(sensitivityGlio.2.lb, sensitivityGlio.33.lb.post, 
            alternative = "less",
            paired = T) #p-val = 1

median(sensitivityGlio.33.lb.post) 
median(sensitivityGlio.2.ub)
wilcox.test(sensitivityGlio.33.lb.post, sensitivityGlio.2.ub, 
            alternative = "less",
            paired = T) #p-val = 3.901e-08

#specificity
median(specificityMenin.2.lb) 
median(specificityMenin.33.lb.post) 
wilcox.test(specificityMenin.2.lb, specificityMenin.33.lb.post, 
            alternative = "less",
            paired = T) #p-val = 3.035e-09

median(specificityMenin.33.lb.post) 
median(specificityMenin.2.ub)
wilcox.test(specificityMenin.33.lb.post, specificityMenin.2.ub, 
            alternative = "less",
            paired = T) #p-val = 1

#total accuracy
median(accuracy.2.lb[,1]) 
median(accuracy.33.lb.post[,1]) 
wilcox.test(accuracy.2.lb[,1], accuracy.33.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val = 0.2628

median(accuracy.33.lb.post[,1]) 
median(accuracy.2.ub[,1])
wilcox.test(accuracy.33.lb.post[,1], accuracy.2.ub[,1], 
            alternative = "less",
            paired = T) #p-val = 0.005974











