#Codes to generate the supplementary figures
rm(list=ls())
load("dataPreprocessing_CCTSIproject_v2.RData")
load("preComBat_3img3clin_LowerUpper_090121.RData")
load("ComBat_3img3clin_090121.RData")
load("3imgVars_prepostComBat_090221.RData")
load("3clinLowerUpper_RF_SMOTE_75iters_051221.RData")

#################       Figure 1, Main manuscript  ###################
#Case 1, scenarios a-b (Figure 1A)
library(ggplot2)
png(filename = "New-Fig-1A_totacc_1ab_prepost_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 8, width = 10)
dat.tot.1ab.prepost = stack(data.frame(LB = accuracy.1.lb[,1],
                                       LB.ComBat.1a = accuracy.1.lb.post[,1],
                                       LB.ComBat.1b = accuracy.23.lb.post[,1],
                                       UB = accuracy.1.ub[,1]
))
bp.dat.tot.1ab.prepost = ggplot(dat.tot.1ab.prepost, aes(x = ind, y = values)) + 
  #geom_violin(width=1.4) + 
  geom_boxplot(color = 1:4, alpha=0.5, outlier.color = 2, notch = T)  
#geom_abline(intercept = 26/45) 
bp.dat.tot.1ab.prepost = bp.dat.tot.1ab.prepost + labs(y="Total accuracy", x = "Scenarios") +
  scale_y_continuous(breaks = seq(0,1, by = 0.05)) +
  labs(title = "Boxplots of total accuracies across 75 repetitions\nRF classification: Glioma/Meningioma\n(only image variables in RF model)")
bp.dat.tot.1ab.prepost +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25)) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 25)) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 25)) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 20))
dev.off()

#Case 1, scenarios c-d (Figure 1B)
png(filename = "New-Fig-1B_totacc_1cd_prepost_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 8, width = 10)
dat.tot.1cd.prepost = stack(data.frame(LB = accuracy.2.lb[,1],
                                       LB.ComBat.1c = accuracy.2.lb.post[,1],
                                       LB.ComBat.1d = accuracy.22.lb.post[,1],
                                       UB = accuracy.2.ub[,1]
))
bp.dat.tot.1cd.prepost = ggplot(dat.tot.1cd.prepost, aes(x = ind, y = values)) + 
  #geom_violin(width=1.4) + 
  geom_boxplot(color = 1:4, alpha=0.5, outlier.color = 2, notch = T)  
#geom_abline(intercept = 26/45) 
bp.dat.tot.1cd.prepost = bp.dat.tot.1cd.prepost + labs(y="Total accuracy", x = "Scenarios") +
  scale_y_continuous(breaks = seq(0,1, by = 0.05)) +
  labs(title = "Boxplots of total accuracies across 75 repetitions\nRF classification: Glioma/Meningioma\n(image variables + clinical covariates in RF model)")
bp.dat.tot.1cd.prepost +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25)) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 25)) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 25)) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 20))
dev.off()

##########################################
# data not shown
#agemos CSU
hist(meninglio.CSU.image3.3$agemos)
shapiro.test(meninglio.CSU.image3.3$agemos)

#agemos Out
hist(meninglio.Out.image3.3$agemos)
shapiro.test(meninglio.Out.image3.3$agemos)

#adj img vars CSU
hist(meninglio.CSU.image3.1$adjmeanSI)
shapiro.test(meninglio.CSU.image3.1$adjmeanSI)
hist(meninglio.CSU.image3.1$adjsdSI)
hist(meninglio.CSU.image3.1$adjcenterSI)
shapiro.test(meninglio.CSU.image3.1$adjcenterSI)

#adj img vars Out
hist(meninglio.Out.image3.1$adjmeanSI)
shapiro.test(meninglio.Out.image3.1$adjmeanSI)
hist(meninglio.Out.image3.1$adjsdSI)
hist(meninglio.Out.image3.1$adjcenterSI, breaks=10)
shapiro.test(meninglio.Out.image3.1$adjcenterSI)

#TR, TE, NEX CSU
hist(meninglio.CSU.image3.3$TR)
shapiro.test(meninglio.CSU.image3.3$TR)
hist(meninglio.CSU.image3.3$TE)
hist(meninglio.CSU.image3.3$NEX)

#TR, TE, NEX Out
hist(meninglio.Out.image3.3$TR)
hist(meninglio.Out.image3.3$TE)
shapiro.test(meninglio.Out.image3.3$TE)
hist(meninglio.Out.image3.3$NEX)


##################      Supplement          ########################
#Figure S2
#Kernel (Epanechnikov) density estimations
#Glioma-CSU
den.mu.dnRatio.meanSI.GC = density(mean.dnRatio.meanSI.glioCSU, kernel = "epa")
den.mu.dnRatio.sdSI.GC = density(mean.dnRatio.sdSI.glioCSU, kernel = "epa")
den.mu.dnRatio.cvSI.GC = density(mean.dnRatio.cvSI.glioCSU, kernel = "epa")
den.mu.dnRatio.centerSI.GC = density(mean.dnRatio.centerSI.glioCSU, kernel = "epa")

#Glioma-Outside
den.mu.dnRatio.meanSI.GO = density(mean.dnRatio.meanSI.glioOut, kernel = "epa")
den.mu.dnRatio.sdSI.GO = density(mean.dnRatio.sdSI.glioOut, kernel = "epa")
den.mu.dnRatio.cvSI.GO = density(mean.dnRatio.cvSI.glioOut, kernel = "epa")
den.mu.dnRatio.centerSI.GO = density(mean.dnRatio.centerSI.glioOut, kernel = "epa")

#Meningioma-CSU
den.mu.dnRatio.meanSI.MC = density(mean.dnRatio.meanSI.meninCSU, kernel = "epa")
den.mu.dnRatio.sdSI.MC = density(mean.dnRatio.sdSI.meninCSU, kernel = "epa")
den.mu.dnRatio.cvSI.MC = density(mean.dnRatio.cvSI.meninCSU, kernel = "epa")
den.mu.dnRatio.centerSI.MC = density(mean.dnRatio.centerSI.meninCSU, kernel = "epa")

#Meningioma-Outside
den.mu.dnRatio.meanSI.MO = density(mean.dnRatio.meanSI.meninOut, kernel = "epa")
den.mu.dnRatio.sdSI.MO = density(mean.dnRatio.sdSI.meninOut, kernel = "epa")
den.mu.dnRatio.cvSI.MO = density(mean.dnRatio.cvSI.meninOut, kernel = "epa")
den.mu.dnRatio.centerSI.MO = density(mean.dnRatio.centerSI.meninOut, kernel = "epa")

#Boxplot: mean-adj-meanSI
minAll = min(mean.dnRatio.meanSI.glioCSU, mean.dnRatio.meanSI.meninCSU,
             mean.dnRatio.meanSI.glioOut, mean.dnRatio.meanSI.meninOut)
maxAll = max(mean.dnRatio.meanSI.glioCSU, mean.dnRatio.meanSI.meninCSU,
             mean.dnRatio.meanSI.glioOut, mean.dnRatio.meanSI.meninOut)
nSize = c(
          rep("Glio-CSU",  length(mean.dnRatio.meanSI.glioCSU)), 
          rep("Menin-CSU", length(mean.dnRatio.meanSI.meninCSU)), 
          rep("Glio-Out",  length(mean.dnRatio.meanSI.glioOut)), 
          rep("Menin-Out", length(mean.dnRatio.meanSI.meninOut))
      )
value = c(mean.dnRatio.meanSI.glioCSU, mean.dnRatio.meanSI.meninCSU,
          mean.dnRatio.meanSI.glioOut, mean.dnRatio.meanSI.meninOut)

data = data.frame(nSize, value)
data$nSize <- factor(data$nSize, levels=unique(nSize))

my_xlab <- paste(levels(data$nSize),"\n(n=", table(data$nSize),")",sep="")

library(ggplot2)
png("New_CSUOut-MeninGlio_boxplot_final_meanSI.png",
    width = 12, height = 8, units = "in", res = 600)
ggplot(data, aes(x=nSize, y=value, fill=nSize)) +
  geom_boxplot(varwidth = T, alpha=0.2, notch = T) +
  ggtitle("") +
  ylab("Means of normalized mean(SI)") + 
  xlab("Subpopulations") +
  #ylim(minAll, maxAll) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25)) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 15)) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 15)) +
  theme(legend.position = "None") +
  scale_x_discrete(labels=my_xlab) + 
  scale_y_continuous(limits = c(minAll, maxAll), n.breaks = 15)
dev.off()


#Boxplot: mean-adj-sdSI
minAll = min(mean.dnRatio.sdSI.glioCSU, mean.dnRatio.sdSI.meninCSU,
             mean.dnRatio.sdSI.glioOut, mean.dnRatio.sdSI.meninOut)
maxAll = max(mean.dnRatio.sdSI.glioCSU, mean.dnRatio.sdSI.meninCSU,
             mean.dnRatio.sdSI.glioOut, mean.dnRatio.sdSI.meninOut)
nSize = c(
  rep("Glio-CSU",  length(mean.dnRatio.sdSI.glioCSU)), 
  rep("Menin-CSU", length(mean.dnRatio.sdSI.meninCSU)), 
  rep("Glio-Out",  length(mean.dnRatio.sdSI.glioOut)), 
  rep("Menin-Out", length(mean.dnRatio.sdSI.meninOut))
)
value = c(mean.dnRatio.sdSI.glioCSU, mean.dnRatio.sdSI.meninCSU,
          mean.dnRatio.sdSI.glioOut, mean.dnRatio.sdSI.meninOut)

data = data.frame(nSize, value)
data$nSize <- factor(data$nSize, levels=unique(nSize))

my_xlab <- paste(levels(data$nSize),"\n(n=", table(data$nSize),")",sep="")

library(ggplot2)
png("New_CSUOut-MeninGlio_boxplot_final_sdSI.png",
    width = 12, height = 8, units = "in", res = 600)
ggplot(data, aes(x=nSize, y=value, fill=nSize)) +
  geom_boxplot(varwidth = T, alpha=0.2, notch = T) +
  ggtitle("") +
  ylab("Means of normalized SD(SI)") + 
  xlab("Subpopulations") +
  #ylim(minAll, maxAll) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25)) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 15)) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 15)) +
  theme(legend.position = "None") +
  scale_x_discrete(labels=my_xlab) + 
  scale_y_continuous(limits = c(minAll, maxAll), n.breaks = 15)
dev.off()


#Boxplot: mean-adj-centSI
minAll = min(mean.dnRatio.centSI.glioCSU, mean.dnRatio.centSI.meninCSU,
             mean.dnRatio.centSI.glioOut, mean.dnRatio.centSI.meninOut)
minAll = max(mean.dnRatio.centSI.glioCSU, mean.dnRatio.centSI.meninCSU,
             mean.dnRatio.centSI.glioOut, mean.dnRatio.centSI.meninOut)
nSize = c(
  rep("Glio-CSU",  length(mean.dnRatio.centSI.glioCSU)), 
  rep("Menin-CSU", length(mean.dnRatio.centSI.meninCSU)), 
  rep("Glio-Out",  length(mean.dnRatio.centSI.glioOut)), 
  rep("Menin-Out", length(mean.dnRatio.centSI.meninOut))
)
value = c(mean.dnRatio.centSI.glioCSU, mean.dnRatio.centSI.meninCSU,
          mean.dnRatio.centSI.glioOut, mean.dnRatio.centSI.meninOut)

data = data.frame(nSize, value)
data$nSize <- factor(data$nSize, levels=unique(nSize))

my_xlab <- paste(levels(data$nSize),"\n(n=", table(data$nSize),")",sep="")

library(ggplot2)
png("New_CSUOut-MeninGlio_boxplot_final_centSI.png",
    width = 12, height = 8, units = "in", res = 600)
ggplot(data, aes(x=nSize, y=value, fill=nSize)) +
  geom_boxplot(varwidth = T, alpha=0.2, notch = T) +
  ggtitle("") +
  ylab("Means of normalized cent(SI)") + 
  xlab("Subpopulations") +
  #ylim(minAll, maxAll) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25)) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 15)) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 15)) +
  theme(legend.position = "None") +
  scale_x_discrete(labels=my_xlab) + 
  scale_y_continuous(limits = c(minAll, maxAll), n.breaks = 15)
dev.off()


#Figure S3
#Correlation tests for predictor variables
#install.packages("corrplot")
#install.packages("RColorBrewer")
library(corrplot)
library(RColorBrewer)

#meninglio-CSU
png(filename = "corrplot_image3_meninglioCSU.png", units = "in", res = 600, 
    height = 8, width = 10)
cor.CSU = round(cor(meninglio.CSU.image3.1),2)
corrplot(cor.CSU, type="upper", order="hclust", 
         method = "circle",
         diag = F,
         col=brewer.pal(n=8, name="RdBu"),
         tl.col="black", tl.srt=45)
dev.off()

#meninglio-Out
png(filename = "corrplot_image3_meninglioOut.png", units = "in", res = 600, 
    height = 8, width = 10)
cor.Out = round(cor(meninglio.Out.image3.1),2)
corrplot(cor.Out, type="upper", order="hclust", 
         method = "circle",
         diag = F,
         col=brewer.pal(n=8, name="RdBu"),
         tl.col="black", tl.srt=45)
dev.off()

#Figure S4
#Boxplots across processors -- XY and DN

#Boxplots -- adjusted means
#Glioma -- CSU
mean.dnRatio.meanSI.glioCSU.XY = mean.dnRatio.meanSI.glioCSU[1:19]
mean.dnRatio.meanSI.glioCSU.DN = mean.dnRatio.meanSI.glioCSU[20:39]
#Glioma -- Outside
mean.dnRatio.meanSI.glioOut.XY = mean.dnRatio.meanSI.glioOut[1:11]
mean.dnRatio.meanSI.glioOut.DN = mean.dnRatio.meanSI.glioOut[12:20]

#Meningioma -- CSU
mean.dnRatio.meanSI.meninCSU.XY = mean.dnRatio.meanSI.meninCSU[1:52]
mean.dnRatio.meanSI.meninCSU.DN = mean.dnRatio.meanSI.meninCSU[53:106]
#Meningioma -- Outside
mean.dnRatio.meanSI.meninOut.XY = mean.dnRatio.meanSI.meninOut[1:40]
mean.dnRatio.meanSI.meninOut.DN = mean.dnRatio.meanSI.meninOut[41:79]

#Boxplot: mean-adj-centSI
minAll = min(mean.dnRatio.meanSI.glioCSU.XY, mean.dnRatio.meanSI.glioCSU.DN,
             mean.dnRatio.meanSI.meninCSU.XY, mean.dnRatio.meanSI.meninCSU.DN,
             mean.dnRatio.meanSI.glioOut.XY,mean.dnRatio.meanSI.glioOut.DN,
             mean.dnRatio.meanSI.meninOut.XY,mean.dnRatio.meanSI.meninOut.DN)

maxAll = max(mean.dnRatio.meanSI.glioCSU.XY, mean.dnRatio.meanSI.glioCSU.DN,
             mean.dnRatio.meanSI.meninCSU.XY, mean.dnRatio.meanSI.meninCSU.DN,
             mean.dnRatio.meanSI.glioOut.XY,mean.dnRatio.meanSI.glioOut.DN,
             mean.dnRatio.meanSI.meninOut.XY,mean.dnRatio.meanSI.meninOut.DN)

nSize = c(
  rep("GC-XY",  length(mean.dnRatio.meanSI.glioCSU.XY)), 
  rep("GC-DN",  length(mean.dnRatio.meanSI.glioCSU.DN)), 
  rep("MC-XY",  length(mean.dnRatio.meanSI.meninCSU.XY)),
  rep("MC-DN",  length(mean.dnRatio.meanSI.meninCSU.DN)),
  rep("GO-XY",  length(mean.dnRatio.meanSI.glioOut.XY)),
  rep("GO-DN",  length(mean.dnRatio.meanSI.glioOut.DN)),
  rep("MO-XY",  length(mean.dnRatio.meanSI.meninOut.XY)),
  rep("MO-DN",  length(mean.dnRatio.meanSI.meninOut.DN))
)
value = c(mean.dnRatio.meanSI.glioCSU.XY, mean.dnRatio.meanSI.glioCSU.DN,
          mean.dnRatio.meanSI.meninCSU.XY, mean.dnRatio.meanSI.meninCSU.DN,
          mean.dnRatio.meanSI.glioOut.XY, mean.dnRatio.meanSI.glioOut.DN,
          mean.dnRatio.meanSI.meninOut.XY, mean.dnRatio.meanSI.meninOut.DN
)

data = data.frame(nSize, value)
data$nSize <- factor(data$nSize, levels=unique(nSize))

my_xlab <- paste(levels(data$nSize),"\n(n=", table(data$nSize),")",sep="")

library(ggplot2)
png("Processors_allGroups_boxplot_final_meanSI.png",
    width = 12, height = 8, units = "in", res = 600)
ggplot(data, aes(x=nSize, y=value, fill=nSize)) +
  geom_boxplot(varwidth = T, alpha=0.2, notch = T, col = c(2,2,3,3,4,4,5,5)) +
  ggtitle("") +
  ylab("Means of normalized mean(SI)") + 
  xlab("Subpopulations") +
  #ylim(minAll, maxAll) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25)) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 15)) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 15)) +
  theme(legend.position = "None") +
  scale_x_discrete(labels=my_xlab) + 
  scale_y_continuous(limits = c(minAll, maxAll), n.breaks = 15)
dev.off()

#Boxplot: mean-adj-sdSI
minAll = min(mean.dnRatio.sdSI.glioCSU.XY, mean.dnRatio.sdSI.glioCSU.DN,
             mean.dnRatio.sdSI.meninCSU.XY, mean.dnRatio.sdSI.meninCSU.DN,
             mean.dnRatio.sdSI.glioOut.XY,mean.dnRatio.sdSI.glioOut.DN,
             mean.dnRatio.sdSI.meninOut.XY,mean.dnRatio.sdSI.meninOut.DN)

maxAll = max(mean.dnRatio.sdSI.glioCSU.XY, mean.dnRatio.sdSI.glioCSU.DN,
             mean.dnRatio.sdSI.meninCSU.XY, mean.dnRatio.sdSI.meninCSU.DN,
             mean.dnRatio.sdSI.glioOut.XY,mean.dnRatio.sdSI.glioOut.DN,
             mean.dnRatio.sdSI.meninOut.XY,mean.dnRatio.sdSI.meninOut.DN)

nSize = c(
  rep("GC-XY",  length(mean.dnRatio.sdSI.glioCSU.XY)), 
  rep("GC-DN",  length(mean.dnRatio.sdSI.glioCSU.DN)), 
  rep("MC-XY",  length(mean.dnRatio.sdSI.meninCSU.XY)),
  rep("MC-DN",  length(mean.dnRatio.sdSI.meninCSU.DN)),
  rep("GO-XY",  length(mean.dnRatio.sdSI.glioOut.XY)),
  rep("GO-DN",  length(mean.dnRatio.sdSI.glioOut.DN)),
  rep("MO-XY",  length(mean.dnRatio.sdSI.meninOut.XY)),
  rep("MO-DN",  length(mean.dnRatio.sdSI.meninOut.DN))
)
value = c(mean.dnRatio.sdSI.glioCSU.XY, mean.dnRatio.sdSI.glioCSU.DN,
          mean.dnRatio.sdSI.meninCSU.XY, mean.dnRatio.sdSI.meninCSU.DN,
          mean.dnRatio.sdSI.glioOut.XY, mean.dnRatio.sdSI.glioOut.DN,
          mean.dnRatio.sdSI.meninOut.XY, mean.dnRatio.sdSI.meninOut.DN
)

data = data.frame(nSize, value)
data$nSize <- factor(data$nSize, levels=unique(nSize))

my_xlab <- paste(levels(data$nSize),"\n(n=", table(data$nSize),")",sep="")

library(ggplot2)
png("Processors_allGroups_boxplot_final_sdSI.png",
    width = 12, height = 8, units = "in", res = 600)
ggplot(data, aes(x=nSize, y=value, fill=nSize)) +
  geom_boxplot(varwidth = T, alpha=0.2, notch = T, col = c(2,2,3,3,4,4,5,5)) +
  ggtitle("") +
  ylab("Means of normalized SD(SI)") + 
  xlab("Subpopulations") +
  #ylim(minAll, maxAll) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25)) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 15)) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 15)) +
  theme(legend.position = "None") +
  scale_x_discrete(labels=my_xlab) + 
  scale_y_continuous(limits = c(minAll, maxAll), n.breaks = 15)
dev.off()

#Boxplot: mean-adj-centSI
minAll = min(mean.dnRatio.centSI.glioCSU.XY, mean.dnRatio.centSI.glioCSU.DN,
             mean.dnRatio.centSI.meninCSU.XY, mean.dnRatio.centSI.meninCSU.DN,
             mean.dnRatio.centSI.glioOut.XY,mean.dnRatio.centSI.glioOut.DN,
             mean.dnRatio.centSI.meninOut.XY,mean.dnRatio.centSI.meninOut.DN)

maxAll = max(mean.dnRatio.centSI.glioCSU.XY, mean.dnRatio.centSI.glioCSU.DN,
             mean.dnRatio.centSI.meninCSU.XY, mean.dnRatio.centSI.meninCSU.DN,
             mean.dnRatio.centSI.glioOut.XY,mean.dnRatio.centSI.glioOut.DN,
             mean.dnRatio.centSI.meninOut.XY,mean.dnRatio.centSI.meninOut.DN)

nSize = c(
  rep("GC-XY",  length(mean.dnRatio.centSI.glioCSU.XY)), 
  rep("GC-DN",  length(mean.dnRatio.centSI.glioCSU.DN)), 
  rep("MC-XY",  length(mean.dnRatio.centSI.meninCSU.XY)),
  rep("MC-DN",  length(mean.dnRatio.centSI.meninCSU.DN)),
  rep("GO-XY",  length(mean.dnRatio.centSI.glioOut.XY)),
  rep("GO-DN",  length(mean.dnRatio.centSI.glioOut.DN)),
  rep("MO-XY",  length(mean.dnRatio.centSI.meninOut.XY)),
  rep("MO-DN",  length(mean.dnRatio.centSI.meninOut.DN))
)
value = c(mean.dnRatio.centSI.glioCSU.XY, mean.dnRatio.centSI.glioCSU.DN,
          mean.dnRatio.centSI.meninCSU.XY, mean.dnRatio.centSI.meninCSU.DN,
          mean.dnRatio.centSI.glioOut.XY, mean.dnRatio.centSI.glioOut.DN,
          mean.dnRatio.centSI.meninOut.XY, mean.dnRatio.centSI.meninOut.DN
)

data = data.frame(nSize, value)
data$nSize <- factor(data$nSize, levels=unique(nSize))

my_xlab <- paste(levels(data$nSize),"\n(n=", table(data$nSize),")",sep="")

library(ggplot2)
png("Processors_allGroups_boxplot_final_centSI.png",
    width = 12, height = 8, units = "in", res = 600)
ggplot(data, aes(x=nSize, y=value, fill=nSize)) +
  geom_boxplot(varwidth = T, alpha=0.2, notch = T, col = c(2,2,3,3,4,4,5,5)) +
  ggtitle("") +
  ylab("Means of normalized cent(SI)") + 
  xlab("Subpopulations") +
  #ylim(minAll, maxAll) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25)) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 20)) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 15)) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 15)) +
  theme(legend.position = "None") +
  scale_x_discrete(labels=my_xlab) + 
  scale_y_continuous(limits = c(minAll, maxAll), n.breaks = 15)
dev.off()


################         Figure S5            ############################
#Case 0: total acc, sens, spec

library(ggplot2)
png(filename = "Case0_all_Lb_UB_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 8, width = 10)
case0.all.LB.UB = stack(data.frame(
                                       tota.L = accuracy.0.lb[,1],
                                       tota.U = accuracy.0.lb[,1],
                                       sens.L = sensitivityGlio.0.lb,
                                       sens.U = sensitivityGlio.0.ub,
                                       spec.L = specificityMenin.0.lb,
                                       spec.U = specificityMenin.0.ub
))
bp.case0.all.LB.UB = ggplot(case0.all.LB.UB, aes(x = ind, y = values)) + 
  geom_boxplot(color = rep(c(1,4),3), alpha=0.5, outlier.color = 2, notch = T)  
#geom_abline(intercept = 26/45) 
bp.case0.all.LB.UB = bp.case0.all.LB.UB + labs(y="Values", x = "Classification metrics/Bounds") +
  scale_y_continuous(breaks = seq(0,1, by = 0.1)) +
  labs(title = "Boxplots across 75 repetitions\nRF classification: Glioma/Meningioma\n(Case 0: Only clinical covariates in RF model)")
bp.case0.all.LB.UB +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        #panel.grid = element_blank()
        ) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 25), 
       # panel.grid = element_blank()
        ) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 25), 
        #panel.grid = element_blank()
        ) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
        ) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
        )
dev.off()



################         Figure S6     ############################
#Case 0 & Case 1: total acc, sens, spec
#Figure S6-A: total accuracy
library(ggplot2)
png(filename = "Case0-1ab_tota_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 8, width = 10)
case01ab.tota = stack(data.frame(
  L.c0 = accuracy.0.lb[,1],
  L = accuracy.1.lb[,1],
  L.CB.1a = accuracy.1.lb.post[,1],
  L.CB.1b = accuracy.23.lb.post[,1],
  U = accuracy.1.ub[,1],
  U.c0 = accuracy.0.ub[,1]
))
bp.case01ab.tota = ggplot(case01ab.tota, aes(x = ind, y = values)) + 
  geom_boxplot(color = 1:6, alpha=0.5, outlier.color = 2, notch = T)  
#geom_abline(intercept = 26/45) 
bp.case01ab.tota = bp.case01ab.tota + labs(y="Total accuracy", x = "Cases/Bounds") +
  scale_y_continuous(breaks = seq(0,1, by = 0.1)) +
  labs(title = "Boxplots across 75 repetitions\nRF classification: Glioma/Meningioma\n(Case 0 & Case 1, Scenarios a,b)")
bp.case01ab.tota +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        #panel.grid = element_blank()
  ) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 25), 
        # panel.grid = element_blank()
  ) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 25), 
        #panel.grid = element_blank()
  ) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
  ) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
  )
dev.off()

#Figure S6-B: sensitivity
library(ggplot2)
png(filename = "Case0-1ab_sens_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 8, width = 10)
case01ab.sens = stack(data.frame(
  L.c0 = sensitivityGlio.0.lb,
  L = sensitivityGlio.1.lb,
  L.CB.1a = sensitivityGlio.1.lb.post,
  L.CB.1b = sensitivityGlio.23.lb.post,
  U = sensitivityGlio.1.ub,
  U.c0 = sensitivityGlio.0.ub
))
bp.case01ab.sens = ggplot(case01ab.sens, aes(x = ind, y = values)) + 
  geom_boxplot(color = 1:6, alpha=0.5, outlier.color = 2, notch = T)  
#geom_abline(intercept = 26/45) 
bp.case01ab.sens = bp.case01ab.sens + labs(y="Sensitivity", x = "Cases/Bounds") +
  scale_y_continuous(breaks = seq(0,1, by = 0.1)) +
  labs(title = "Boxplots across 75 repetitions\nRF classification: Glioma/Meningioma\n(Case 0 & Case 1, Scenarios a,b)")
bp.case01ab.sens +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        #panel.grid = element_blank()
  ) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 25), 
        # panel.grid = element_blank()
  ) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 25), 
        #panel.grid = element_blank()
  ) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
  ) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
  )
dev.off()

#Figure S6-C: specificity
library(ggplot2)
png(filename = "Case0-1ab_spec_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 8, width = 10)
case01ab.spec = stack(data.frame(
  L.c0 = specificityMenin.0.lb,
  L = specificityMenin.1.lb,
  L.CB.1a = specificityMenin.1.lb.post,
  L.CB.1b = specificityMenin.23.lb.post,
  U = specificityMenin.1.ub,
  U.c0 = specificityMenin.0.ub
))
bp.case01ab.spec = ggplot(case01ab.spec, aes(x = ind, y = values)) + 
  geom_boxplot(color = 1:6, alpha=0.5, outlier.color = 2, notch = T)  
#geom_abline(intercept = 26/45) 
bp.case01ab.spec = bp.case01ab.spec + labs(y="Specificity", x = "Cases/Bounds") +
  scale_y_continuous(breaks = seq(0,1, by = 0.1)) +
  labs(title = "Boxplots across 75 repetitions\nRF classification: Glioma/Meningioma\n(Case 0 & Case 1, Scenarios a,b)")
bp.case01ab.spec +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        #panel.grid = element_blank()
  ) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 25), 
        # panel.grid = element_blank()
  ) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 25), 
        #panel.grid = element_blank()
  ) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
  ) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
  )
dev.off()

################         Figure S7     ############################
#RF with 3 img, and (3 img + 3 clin)
#Case 1 scenarios c and d
#Figure S7-A: total accuracy
library(ggplot2)
png(filename = "Case1cd_tota_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 8, width = 10)
case1cd.tota = stack(data.frame(
  L3 = accuracy.1.lb[,1],
  L6 = accuracy.2.lb[,1],
  L6.CB.1c = accuracy.2.lb.post[,1],
  L6.CB.1d = accuracy.22.lb.post[,1],
  U6 = accuracy.2.ub[,1],
  U3 = accuracy.1.ub[,1]
))
bp.case1cd.tota = ggplot(case1cd.tota, aes(x = ind, y = values)) + 
  geom_boxplot(color = 1:6, alpha=0.5, outlier.color = 2, notch = T)  
#geom_abline(intercept = 26/45) 
bp.case1cd.tota = bp.case1cd.tota + labs(y="Total accuracy", x = "Cases/Bounds") +
  scale_y_continuous(breaks = seq(0,1, by = 0.1)) +
  labs(title = "Boxplots across 75 repetitions\nRF classification: Glioma/Meningioma\n(Case 1, Scenarios c,d)")
bp.case1cd.tota +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        #panel.grid = element_blank()
  ) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 25), 
        # panel.grid = element_blank()
  ) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 25), 
        #panel.grid = element_blank()
  ) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
  ) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
  )
dev.off()

#Figure S7-B: sensitivity
library(ggplot2)
png(filename = "Case1cd_sens_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 8, width = 10)
case1cd.sens = stack(data.frame(
  L3 = sensitivityGlio.1.lb,
  L6 = sensitivityGlio.2.lb,
  L6.CB.1c = sensitivityGlio.2.lb.post,
  L6.CB.1d = sensitivityGlio.22.lb.post,
  U6 = sensitivityGlio.2.ub,
  U3 = sensitivityGlio.1.ub
))
bp.case1cd.sens = ggplot(case1cd.sens, aes(x = ind, y = values)) + 
  geom_boxplot(color = 1:6, alpha=0.5, outlier.color = 2, notch = T)  
#geom_abline(intercept = 26/45) 
bp.case1cd.sens = bp.case1cd.sens + labs(y="Sensitivity", x = "Cases/Bounds") +
  scale_y_continuous(breaks = seq(0,1, by = 0.1)) +
  labs(title = "Boxplots across 75 repetitions\nRF classification: Glioma/Meningioma\n(Case 1, Scenarios c,d)")
bp.case1cd.sens +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        #panel.grid = element_blank()
  ) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 25), 
        # panel.grid = element_blank()
  ) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 25), 
        #panel.grid = element_blank()
  ) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
  ) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
  )
dev.off()

#Figure S7-C: specificity
library(ggplot2)
png(filename = "Case1cd_spec_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 8, width = 10)
case1cd.spec = stack(data.frame(
  L3 = specificityMenin.1.lb,
  L6 = specificityMenin.2.lb,
  L6.CB.1c = specificityMenin.2.lb.post,
  L6.CB.1d = specificityMenin.22.lb.post,
  U6 = specificityMenin.2.ub,
  U3 = specificityMenin.1.ub
))
bp.case1cd.spec = ggplot(case1cd.spec, aes(x = ind, y = values)) + 
  geom_boxplot(color = 1:6, alpha=0.5, outlier.color = 2, notch = T)  
#geom_abline(intercept = 26/45) 
bp.case1cd.spec = bp.case1cd.spec + labs(y="Specificity", x = "Cases/Bounds") +
  scale_y_continuous(breaks = seq(0,1, by = 0.1)) +
  labs(title = "Boxplots across 75 repetitions\nRF classification: Glioma/Meningioma\n(Case 1, Scenarios c,d)")
bp.case1cd.spec +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        #panel.grid = element_blank()
  ) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold", size = 25), 
        # panel.grid = element_blank()
  ) +
  theme(axis.title.y = element_text(hjust=0.5, face = "bold", size = 25), 
        #panel.grid = element_blank()
  ) +
  theme(axis.text.x = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
  ) +
  theme(axis.text.y = element_text(hjust=0.5, face = "bold", size = 20),
        #panel.grid = element_blank()
  )
dev.off()



#Supplementary Figure S3
#Scenario: Cases 1 a-d
rm(list=ls())
load("3imgVars_prepostComBat_090221.RData")
load("3clinLowerUpper_RF_SMOTE_75iters_051221.RData")

library(ggplot2)
png(filename = "Fig-S2_Case0-1a-totacc_prepost_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 6, width = 10)
dat.totacc.01a.prepost = stack(data.frame(TotAcc.lb.0 = accuracy.0.lb[,1],
                                          TotAcc.lb.1a = accuracy.1.lb[,1],
                                          TotAcc.lb.1a.CB = accuracy.1.lb.post[,1],
                                          TotAcc.ub.1a = accuracy.1.ub[,1],
                                          TotAcc.ub.0 = accuracy.0.ub[,1]
))
bp.dat.totacc.01a.prepost = ggplot(dat.totacc.01a.prepost, aes(x = ind, y = values)) + 
  #geom_violin(width=1.4) + 
  geom_boxplot(color = c(4,1:3,5), alpha=0.5, outlier.color = 2, notch = F)  
#geom_abline(intercept = 26/45) 
bp.dat.totacc.01a.prepost = bp.dat.totacc.01a.prepost + labs(y="Total accuracy", x = "Scenarios") +
  scale_y_continuous(breaks = seq(0,1, by = 0.05)) +
  labs(title = "Boxplots of total accuracies over 75 runs\nRF classification: Glioma/Meningioma [Case 0 vs. Case 1, Scenario a]")
bp.dat.totacc.01a.prepost +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), panel.grid = element_blank())
dev.off()





#Supplementary Figure S4
#Scenario: Cases 1, Scenario b vs. Case 1, Scenario c 
rm(list=c())
load("preComBat_3img3clin_LowerUpper_090121.RData")
load("ComBat_3img3clin_090121.RData")
load("3imgVars_prepostComBat_090221.RData")
png(filename = "Fig-S4_all-1bc_postComBat_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 6, width = 10)
dat.all.1bc.post = stack(data.frame(Sens.lb.1b = sensitivityGlio.23.lb.post,
                                    Sens.lb.1c = sensitivityGlio.2.lb.post,
                                    Spec.lb.1b = specificityMenin.23.lb.post,
                                    Spec.lb.1c = specificityMenin.2.lb.post,
                                    TotAcc.lb.1b = accuracy.23.lb.post[,1],
                                    TotAcc.lb.1c = accuracy.2.lb.post[,1]
))
bp.dat.all.1bc.post = ggplot(dat.all.1bc.post, aes(x = ind, y = values)) + 
  #geom_violin(width=1.4) + 
  geom_boxplot(color = rep(6:7,3), alpha=0.5, outlier.color = 2, notch = F)  
#geom_abline(intercept = 26/45) 
bp.dat.all.1bc.post = bp.dat.all.1bc.post + labs(y="RF classification performance", x = "Classification metrics/Scenarios") +
  scale_y_continuous(breaks = seq(0,1, by = 0.05)) +
  labs(title = "Boxplots of Sens/Spec/Tot-Acc metrics over 75 runs\nRF classification: Glioma/Meningioma [Scenarios 1b vs. 1c]")
bp.dat.all.1bc.post +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), panel.grid = element_blank())
dev.off()



#Scenario 2.0
png(filename = "all.20.prepost_RF-SMOTE_75iters.png", units = "in", res = 600, 
    height = 6, width = 10)
dat.all.1.prepost = stack(data.frame(spec.lb.0 = specificityMenin.0.lb,
                                     spec.ub.0 = specificityMenin.0.ub,
                                     spec.lb.2 = specificityMenin.2.lb,
                                     spec.ub.2 = specificityMenin.2.ub,
                                     spec.lb.20.post = specificityMenin.20.lb.post,
                                     sens.lb.0 = sensitivityGlio.0.lb,
                                     sens.ub.0 = sensitivityGlio.0.ub,
                                     sens.lb.2 = sensitivityGlio.2.lb,
                                     sens.ub.2 = sensitivityGlio.2.ub,
                                     sens.lb.20.post = sensitivityGlio.20.lb.post,
                                     accu.lb.0 = accuracy.0.lb[,1],
                                     accu.ub.0 = accuracy.0.ub[,1],
                                     accu.lb.2 = accuracy.2.lb[,1],
                                     accu.ub.2 = accuracy.2.ub[,1],
                                     accu.lb.20.post = accuracy.20.lb.post[,1]
))
bp.dat.all.1.prepost = ggplot(dat.all.1.prepost, aes(x = ind, y = values)) + 
  #geom_violin(width=1.4) + 
  geom_boxplot(color = c(1:2,1:3,1:2,1:3,5:9), alpha=0.5, outlier.color = 2, notch = F)  
#geom_abline(intercept = 26/45) 
bp.dat.all.1.prepost = bp.dat.all.1.prepost + labs(y="Spec/Sens", x = "Pre/Post Harmonization") +
  scale_y_continuous(breaks = seq(0,1, by = 0.05)) +
  labs(title = "Boxplots of Spec/Sens/Tot-Acc [PRE/POST HARMON]\nGlioma vs Meningioma [75 reps, RF-SMOTE, 3 Image + 3 Clin]")
bp.dat.all.1.prepost +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), panel.grid = element_blank())
dev.off()





