#Results: Case 1
#Checks values and comparisons in Table S6
load("3clinLowerUpper_RF_SMOTE_75iters_051221.RData")
load("3imgVars_prepostComBat_090221.RData")
load("preComBat_3img3clin_LowerUpper_090121.RData")
load("ComBat_3img3clin_090121.RData")

#RF: 3 img vars
#LB
median(sensitivityGlio.1.lb)
mad(sensitivityGlio.1.lb)
median(specificityMenin.1.lb)
mad(specificityMenin.1.lb)
median(accuracy.1.lb[,1])
mad(accuracy.1.lb[,1])

#COMBAT.LB: 1a
median(sensitivityGlio.1.lb.post)
mad(sensitivityGlio.1.lb.post)
median(specificityMenin.1.lb.post)
mad(specificityMenin.1.lb.post)
median(accuracy.1.lb.post[,1])
mad(accuracy.1.lb.post[,1])

#COMBAT.LB: 1b
median(sensitivityGlio.23.lb.post)
mad(sensitivityGlio.23.lb.post)
median(specificityMenin.23.lb.post)
mad(specificityMenin.23.lb.post)
median(accuracy.23.lb.post[,1])
mad(accuracy.23.lb.post[,1])

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

#COMBAT.LB: 1c
median(sensitivityGlio.2.lb.post)
mad(sensitivityGlio.2.lb.post)
median(specificityMenin.2.lb.post)
mad(specificityMenin.2.lb.post)
median(accuracy.2.lb.post[,1])
mad(accuracy.2.lb.post[,1])

#COMBAT.LB: 1d
median(sensitivityGlio.22.lb.post)
mad(sensitivityGlio.22.lb.post)
median(specificityMenin.22.lb.post)
mad(specificityMenin.22.lb.post)
median(accuracy.22.lb.post[,1])
mad(accuracy.22.lb.post[,1])

#UB
median(sensitivityGlio.2.ub)
mad(sensitivityGlio.2.ub)
median(specificityMenin.2.ub)
mad(specificityMenin.2.ub)
median(accuracy.2.ub[,1])
mad(accuracy.2.ub[,1])

#Wilcoxon's signed-rank paired one-sided tests
#Case 0 vs Scenario 1-a
load("3imgVars_prepostComBat_090221.RData")
load("3clinLowerUpper_RF_SMOTE_75iters_051221.RData")
#Lower Bound
median(accuracy.0.lb[,1]) #0.5789
median(accuracy.1.lb[,1]) #0.6053

wilcox.test(accuracy.0.lb[,1], accuracy.1.lb[,1], 
            alternative = "less",
            paired = T) #p-val=0.2702

#Upper Bound
median(accuracy.0.ub[,1]) #0.5789
median(accuracy.1.ub[,1]) #0.6579

wilcox.test(accuracy.0.ub[,1], accuracy.1.ub[,1], 
            alternative = "less",
            paired = T) #p-val=4.065e-07

#Lower Bound post-ComBat
median(accuracy.0.lb[,1]) #0.5789474
median(accuracy.1.lb.post[,1]) #0.6578947

wilcox.test(accuracy.0.lb[,1], accuracy.1.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val=4.98e-08


#Case 1, scenarios a,b vs c,d
#Total accuracy
median(accuracy.1.lb[,1]) #0.6052632
median(accuracy.2.lb[,1]) #0.6842105

wilcox.test(accuracy.1.lb[,1], accuracy.2.lb[,1], 
            alternative = "less",
            paired = T) #p-val=7.476e-09

median(accuracy.1.ub[,1]) #0.6578947
median(accuracy.2.ub[,1]) #0.7105263

wilcox.test(accuracy.1.ub[,1], accuracy.2.ub[,1], 
            alternative = "less",
            paired = T) #p-val=3.638e-07

#Sensitivity
median(sensitivityGlio.1.lb) #0.4736842
median(sensitivityGlio.2.lb) #0.5263158
wilcox.test(sensitivityGlio.1.lb, sensitivityGlio.2.lb, 
            alternative = "less",
            paired = T) #p-val=0.0008771

median(sensitivityGlio.1.ub) #0.5263158
median(sensitivityGlio.2.ub) #0.6315789
wilcox.test(sensitivityGlio.1.ub, sensitivityGlio.2.ub, 
            alternative = "less",
            paired = T) #p-val=1.761e-06

#Specificity
median(specificityMenin.1.lb) #0.6842105
median(specificityMenin.2.lb) #0.7894737
wilcox.test(specificityMenin.1.lb, specificityMenin.2.lb, 
            alternative = "less",
            paired = T) #p-val=2.329e-10

median(specificityMenin.1.ub) #0.7894737
median(specificityMenin.2.ub) #0.8421053
wilcox.test(specificityMenin.1.ub, specificityMenin.2.ub, 
            alternative = "less",
            paired = T) #p-val=0.002905


#Scenario 1-a
#Lower Bound: W/O and W/ ComBat
median(sensitivityGlio.1.lb) #0.4736842
median(sensitivityGlio.1.lb.post) #0.5789474
wilcox.test(sensitivityGlio.1.lb, sensitivityGlio.1.lb.post, 
            alternative = "less",
            paired = T) #p-val = 4.329e-08

median(specificityMenin.1.lb) # 0.6842105
median(specificityMenin.1.lb.post) #0.7368421
wilcox.test(specificityMenin.1.lb, specificityMenin.1.lb.post, 
            alternative = "less",
            paired = T) #p-val=0.001157

median(accuracy.1.lb[,1]) #0.6052632
median(accuracy.1.lb.post[,1]) #0.6578947
wilcox.test(accuracy.1.lb[,1], accuracy.1.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val=2.636e-08

#Check "bold" and "underline" in cells for Table S6
#Lower Bound pre- vs. post-ComBat (bold)
#Lower-Bound post-ComBat vs. Upper Bound (underline)

#1a
#sensitivity
median(sensitivityGlio.1.lb) 
median(sensitivityGlio.1.lb.post) 
wilcox.test(sensitivityGlio.1.lb, sensitivityGlio.1.lb.post, 
            alternative = "less",
            paired = T) #p-val = 4.329e-08

median(sensitivityGlio.1.lb.post) 
median(sensitivityGlio.1.ub)
wilcox.test(sensitivityGlio.1.lb.post, sensitivityGlio.1.ub, 
            alternative = "less",
            paired = T) #p-val = 0.9996

#specificity
median(specificityMenin.1.lb) 
median(specificityMenin.1.lb.post) 
wilcox.test(specificityMenin.1.lb, specificityMenin.1.lb.post, 
            alternative = "less",
            paired = T) #p-val = 0.001157

median(specificityMenin.1.lb.post) 
median(specificityMenin.1.ub)
wilcox.test(specificityMenin.1.lb.post, specificityMenin.1.ub, 
            alternative = "less",
            paired = T) #p-val = 0.004986

#total accuracy
median(accuracy.1.lb[,1]) 
median(accuracy.1.lb.post[,1]) 
wilcox.test(accuracy.1.lb[,1], accuracy.1.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val = 0.001157

median(accuracy.1.lb.post[,1]) 
median(accuracy.1.ub[,1])
wilcox.test(accuracy.1.lb.post[,1], accuracy.1.ub[,1], 
            alternative = "less",
            paired = T) #p-val = 0.8611

#1b
#sensitivity
median(sensitivityGlio.1.lb) 
median(sensitivityGlio.23.lb.post) 
wilcox.test(sensitivityGlio.1.lb, sensitivityGlio.23.lb.post, 
            alternative = "less",
            paired = T) #p-val = 4.329e-08

median(sensitivityGlio.23.lb.post) 
median(sensitivityGlio.1.ub)
wilcox.test(sensitivityGlio.23.lb.post, sensitivityGlio.1.ub, 
            alternative = "less",
            paired = T) #p-val = 1

#specificity
median(specificityMenin.1.lb) 
median(specificityMenin.23.lb.post) 
wilcox.test(specificityMenin.1.lb, specificityMenin.23.lb.post, 
            alternative = "less",
            paired = T) #p-val = 0.001539

median(specificityMenin.23.lb.post) 
median(specificityMenin.1.ub)
wilcox.test(specificityMenin.23.lb.post, specificityMenin.1.ub, 
            alternative = "less",
            paired = T) #p-val = 0.003265

#total accuracy
median(accuracy.1.lb[,1]) 
median(accuracy.23.lb.post[,1]) 
wilcox.test(accuracy.1.lb[,1], accuracy.23.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val = 8.283e-10

median(accuracy.23.lb.post[,1]) 
median(accuracy.1.ub[,1])
wilcox.test(accuracy.23.lb.post[,1], accuracy.1.ub[,1], 
            alternative = "less",
            paired = T) #p-val = 0.9822

#1c
#sensitivity
median(sensitivityGlio.2.lb) 
median(sensitivityGlio.2.lb.post) 
wilcox.test(sensitivityGlio.2.lb, sensitivityGlio.2.lb.post, 
            alternative = "less",
            paired = T) #p-val = 0.9526

median(sensitivityGlio.2.lb.post) 
median(sensitivityGlio.2.ub)
wilcox.test(sensitivityGlio.2.lb.post, sensitivityGlio.2.ub, 
            alternative = "less",
            paired = T) #p-val = 1.208e-05

#specificity
median(specificityMenin.2.lb) 
median(specificityMenin.2.lb.post) 
wilcox.test(specificityMenin.2.lb, specificityMenin.2.lb.post, 
            alternative = "less",
            paired = T) #p-val = 9.435e-10

median(specificityMenin.2.lb.post) 
median(specificityMenin.2.ub)
wilcox.test(specificityMenin.2.lb.post, specificityMenin.2.ub, 
            alternative = "less",
            paired = T) #p-val = 1

#total accuracy
median(accuracy.2.lb[,1]) 
median(accuracy.2.lb.post[,1]) 
wilcox.test(accuracy.2.lb[,1], accuracy.2.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val = 0.0008803

median(accuracy.2.lb.post[,1]) 
median(accuracy.2.ub[,1])
wilcox.test(accuracy.2.lb.post[,1], accuracy.2.ub[,1], 
            alternative = "less",
            paired = T) #p-val = 0.1191

#1d
#sensitivity
median(sensitivityGlio.2.lb) 
median(sensitivityGlio.22.lb.post) 
wilcox.test(sensitivityGlio.2.lb, sensitivityGlio.22.lb.post, 
            alternative = "less",
            paired = T) #p-val = 0.9997

median(sensitivityGlio.22.lb.post) 
median(sensitivityGlio.2.ub)
wilcox.test(sensitivityGlio.22.lb.post, sensitivityGlio.2.ub, 
            alternative = "less",
            paired = T) #p-val = 6.744e-06

#specificity
median(specificityMenin.2.lb) 
median(specificityMenin.22.lb.post) 
wilcox.test(specificityMenin.2.lb, specificityMenin.22.lb.post, 
            alternative = "less",
            paired = T) #p-val = 2.222e-07

median(specificityMenin.22.lb.post) 
median(specificityMenin.2.ub)
wilcox.test(specificityMenin.22.lb.post, specificityMenin.2.ub, 
            alternative = "less",
            paired = T) #p-val = 1

#total accuracy
median(accuracy.2.lb[,1]) 
median(accuracy.22.lb.post[,1]) 
wilcox.test(accuracy.2.lb[,1], accuracy.22.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val = 0.03071

median(accuracy.22.lb.post[,1]) 
median(accuracy.2.ub[,1])
wilcox.test(accuracy.22.lb.post[,1], accuracy.2.ub[,1], 
            alternative = "less",
            paired = T) #p-val = 0.0199


#Scenario 1a vs 1b 
#total accuracy
median(accuracy.1.lb.post[,1]) #0.6578947
median(accuracy.23.lb.post[,1]) #0.6578947
wilcox.test(accuracy.1.lb.post[,1], accuracy.23.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val=0.05067

#Scenario (1c, 1d) vs. LB | 1c vs 1d | 1b vs 1c 
#sensitivity 1c
median(sensitivityGlio.2.lb) #0.5263158
median(sensitivityGlio.2.lb.post) #0.5263158
wilcox.test(sensitivityGlio.2.lb, sensitivityGlio.2.lb.post, 
            alternative = "less",
            paired = T) #p-val=0.9526
#sensitivity 1d
median(sensitivityGlio.2.lb) #0.5263158
median(sensitivityGlio.22.lb.post) #0.5263158
wilcox.test(sensitivityGlio.2.lb, sensitivityGlio.22.lb.post, 
            alternative = "less",
            paired = T) #p-val=0.9997

#sensitivity: 1c vs 1d
median(sensitivityGlio.2.lb.post) #0.5263158
median(sensitivityGlio.22.lb.post)  #0.5263158
wilcox.test(sensitivityGlio.22.lb.post, sensitivityGlio.2.lb.post, 
            alternative = "less",
            #exact = F,
            paired = T) #p-val = 0.2114 (paired FALSE, ties for TRUE)

#sensitivity: 1b vs 1c
median(sensitivityGlio.23.lb.post) #0.6315789
median(sensitivityGlio.2.lb.post)  #0.5263158
wilcox.test(sensitivityGlio.2.lb.post, sensitivityGlio.23.lb.post, 
            alternative = "less",
            paired = T) #p-val = 2.07e-05

#specificity 1c
median(specificityMenin.2.lb) #0.7894737
median(specificityMenin.2.lb.post)  #0.8421053
wilcox.test(specificityMenin.2.lb, specificityMenin.2.lb.post, 
            alternative = "less",
            paired = T) #p-val=9.435 E-10

#specificity 1d
median(specificityMenin.2.lb) #0.7894737
median(specificityMenin.22.lb.post)  #0.8421053
wilcox.test(specificityMenin.2.lb, specificityMenin.22.lb.post, 
            alternative = "less",
            paired = T) #p-val=2.222e-07

#specificity: 1c vs 1d
median(specificityMenin.22.lb.post) #0.8421053
median(specificityMenin.2.lb.post) #0.8421053
wilcox.test(specificityMenin.22.lb.post, specificityMenin.2.lb.post, 
            alternative = "less",
            paired = T) #p-val=0.00269

#specificity: 1b vs 1c
median(specificityMenin.23.lb.post) #0.7368421
median(specificityMenin.2.lb.post) #0.8421053
wilcox.test(specificityMenin.23.lb.post, specificityMenin.2.lb.post, 
            alternative = "less",
            paired = T) #p-val=3.049E-12

#total accuracy 1c
median(accuracy.2.lb[,1]) #0.6842105
median(accuracy.2.lb.post[,1]) #0.7105263
wilcox.test(accuracy.2.lb[,1], accuracy.2.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val=0.0008803

#total accuracy 1d
median(accuracy.2.lb[,1]) #0.6842105
median(accuracy.22.lb.post[,1]) #0.6842105
wilcox.test(accuracy.2.lb[,1], accuracy.22.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val=0.03071

#total accuracy: 1c vs 1d
median(accuracy.22.lb.post[,1]) #0.6842105
median(accuracy.2.lb.post[,1]) #0.7105263
wilcox.test(accuracy.22.lb.post[,1], accuracy.2.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val=0.006971

#total accuracy: 1b vs 1c
median(accuracy.23.lb.post[,1]) #0.6578947
median(accuracy.2.lb.post[,1]) #0.7105263
wilcox.test(accuracy.23.lb.post[,1], accuracy.2.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val=0.0001838


#LowerB-Post ComBat and UpperB
wilcox.test(sensitivityGlio.2.lb.post, sensitivityGlio.2.ub, 
            alternative = "less",
            paired = T) #p-val=1.208 E-05

median(specificityMenin.2.ub) #0.8421053
median(specificityMenin.2.lb.post) #0.8421053
wilcox.test(specificityMenin.2.lb.post, specificityMenin.2.ub, 
            alternative = "less",
            paired = T) #p-val=1

median(accuracy.2.lb.post[,1]) #0.7105263
median(accuracy.2.ub[,1]) #0.7105263
wilcox.test(accuracy.2.lb.post[,1], accuracy.2.ub[,1], 
            alternative = "less",
            paired = T) #p-val = 0.1191



#Scenario 1-d
#Lower Bound: W/O and W/ ComBat
wilcox.test(sensitivityGlio.2.lb, sensitivityGlio.22.lb.post, 
            alternative = "less",
            paired = T) #p-val=0.9997
wilcox.test(specificityMenin.2.lb, specificityMenin.22.lb.post, 
            alternative = "less",
            paired = T) #p-val=
wilcox.test(accuracy.2.lb[,1], accuracy.22.lb.post[,1], 
            alternative = "less",
            paired = T) #p-val=

#LowerB: W/ ComBat and UpperB
wilcox.test(sensitivityGlio.22.lb.post, sensitivityGlio.2.ub, 
            alternative = "less",
            paired = T) #p-val=6.744E-06
wilcox.test(specificityMenin.22.lb.post, specificityMenin.2.ub, 
            alternative = "less",
            paired = T) #p-val=1
wilcox.test(accuracy.22.lb.post[,1], accuracy.2.ub[,1], 
            alternative = "less",
            paired = T) #p-val = 0.0199


median(accuracy.1.lb[,1]) #0.6052632
median(accuracy.2.lb.post[,1]) #0.7105263
wilcox.test(accuracy.1.lb[,1], accuracy.2.lb.post[,1], 
            alternative = "less", 
            paired = T)

#Check lowest median classification metrics across all scenarios
median(accuracy.0.lb[,1]) #0.5789474
median(accuracy.0.ub[,1]) #0.5789474

median(sensitivityGlio.0.lb) #0.4210526
median(sensitivityGlio.0.ub) #0.4736842

median(specificityMenin.0.lb) #0.7368421
median(specificityMenin.0.ub) #0.7368421


#Check Case 0 vs 1a specificity
median(specificityMenin.0.lb) #0.7368421
median(specificityMenin.1.lb.post) #0.7368421
wilcox.test(specificityMenin.0.lb, specificityMenin.1.lb.post, 
            alternative = "less",
            #exact = F,
            paired = T) #p-val = 0.3467 
sum(specificityMenin.0.lb == specificityMenin.1.lb.post)
summary(specificityMenin.0.lb)
summary(specificityMenin.1.lb.post)

#Check Case 0 vs 1b specificity
median(specificityMenin.0.lb) #0.7368421
median(specificityMenin.23.lb.post) #0.7368421
wilcox.test(specificityMenin.0.lb, specificityMenin.23.lb.post, 
            alternative = "less",
            #exact = F,
            paired = T) #p-val = 0.3587 
sum(specificityMenin.0.lb == specificityMenin.23.lb.post)
summary(specificityMenin.0.lb)
summary(specificityMenin.23.lb.post)
