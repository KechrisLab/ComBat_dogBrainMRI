#Data preprocessing and creating the predictor matrix

rm(list=ls())

#Install required packages
install.packages("tidyverse")
library(tidyverse)
install.packages("readxl")
library(readxl)

#install.packages("ggplot2")
library(ggplot2)

#Glioma -- CSU
glioma.CSU = read_excel("/Users/dnandy/OneDrive - The University of Colorado Denver/CCTSI_TM-Pilot_2020-21/DataFiles/Final_DataFiles/Glioma_CSU.xlsx",
                        sheet = 1)
glioma.CSU = as.data.frame(glioma.CSU)
dim(glioma.CSU) #234 x 25
colnames(glioma.CSU)

#Find samples by processors -- XY and DN
which.glioma.CSU.XY = which(glioma.CSU$Processor == "XY")
length(unique(glioma.CSU$PatientID[which.glioma.CSU.XY])) #19: Samples 1-19

which.glioma.CSU.DN = which(glioma.CSU$Processor == "DN")
length(unique(glioma.CSU$PatientID[which.glioma.CSU.DN])) #20: Samples 20-39



#Find subjects with less than 3 slices
which.nas.glioCSU = which(glioma.CSU$`# of slices` < 3)
pIDs.noSlices.lessthan3.GC = unique(glioma.CSU$PatientID[which.nas.glioCSU])
pIDs.noSlices.lessthan3.GC #4 subjects -- 329102 (2) 203038 (1) 302105 (1) 280649 (1)
glioma.CSU$`# of slices`[which.nas.glioCSU]

#Find subjects with NOSE muscle as normal tissue
which.nose.glioCSU = which(glioma.CSU$Normal_Nose == 1)
pIDs.nose.GC = unique(glioma.CSU$PatientID[which.nose.glioCSU])
pIDs.nose.GC #NULL
glioma.CSU$Normal_Nose[which.nose.glioCSU] #NULL

#Find which variables have NA's
nas.GlioCSU = apply(glioma.CSU, 2, function(x) sum(is.na(x)))
colnames(glioma.CSU)[which(nas.GlioCSU > 0)]
#"Series#"       "Img#"          "DiseaseStatus" "CenterX"       "CenterY"      
#"Center_SI"     "Area_cm2"      "Mean_SI"       "SD_SI"

#Find variability within the scanner covariates
colnames(glioma.CSU)[17:23]
for(i in 17:23){
  print(colnames(glioma.CSU)[i])
  print(length(unique(glioma.CSU[, i])))
}

#Glioma -- Outside
glioma.Outside = read_excel("/Users/dnandy/OneDrive - The University of Colorado Denver/CCTSI_TM-Pilot_2020-21/DataFiles/Final_DataFiles/Glioma_Outside.xlsx",
                            sheet = 1)
glioma.Outside = as.data.frame(glioma.Outside)
dim(glioma.Outside) #120 x 26
colnames(glioma.Outside)

#Find subjects with less than 3 slices
which.nas.glioOut = which(glioma.Outside$`# slices` < 3)
pIDs.noSlices.lessthan3.GO = unique(glioma.Outside$PatientID[which.nas.glioOut])
pIDs.noSlices.lessthan3.GO #3 subjects: 341925(2) 335115(1) 331176(2)
glioma.Outside$`# slices`[which.nas.glioOut] 

#Find subjects with NOSE muscle as normal tissue
which.nose.glioOut = which(glioma.Outside$Normal_Nose == 1)
pIDs.nose.GO = unique(glioma.Outside$PatientID[which.nose.glioOut])
pIDs.nose.GO #NULL
glioma.Outside$Normal_Nose[which.nose.glioOut] #NULL

#Find samples by processors -- XY and DN
which.glioma.Out.XY = which(glioma.Outside$Processor == "XY")
length(unique(glioma.Outside$PatientID[which.glioma.Out.XY])) #11: Samples 1-11

which.glioma.Out.DN = which(glioma.Outside$Processor == "DN")
length(unique(glioma.Outside$PatientID[which.glioma.Out.DN])) #9: Samples 12-20

#Find which variables have NA's
nas.GlioOut = apply(glioma.Outside, 2, function(x) sum(is.na(x)))
colnames(glioma.Outside)[which(nas.GlioOut > 0)]
#"Series#"       "Image#"        "DiseaseStatus" "CenterX"       "CenterY"      
#"Center_SI"     "Area_cm2"      "Mean_SI"       "SD_SI"         "FOVRecon"

na.go = Reduce(intersect, list(which(!is.na(glioma.Outside$`Series#`)), which(is.na(glioma.Outside$`FOVRecon`))))
length(na.go) #46

##Write the sample info on dogs with NA's within "FOVRecon"
# write.table(unique(glioma.Outside[na.go, c(2:5, 24)]), "gliomaOutside_FOVReconNAs.csv",
#             sep = ",",
#             row.names = F,
#             col.names = T)

#Find variability within the scanner covariates
colnames(glioma.Outside)[18:24]
for(i in 18:24){
  print(colnames(glioma.Outside)[i])
  print(length(unique(glioma.Outside[, i])))
}


#Meningioma -- CSU
meningioma.CSU = read_excel("/Users/dnandy/OneDrive - The University of Colorado Denver/CCTSI_TM-Pilot_2020-21/DataFiles/Final_DataFiles/Meningioma_CSU.xlsx",
                            sheet = 1)
meningioma.CSU = as.data.frame(meningioma.CSU)
dim(meningioma.CSU) #636 x 25
colnames(meningioma.CSU)

#Find subjects with less than 3 slices
which.nas.meninCSU = which(meningioma.CSU$`# of slices` < 3)
pIDs.noSlices.lessthan3.MC = unique(meningioma.CSU$PatientID[which.nas.meninCSU])
pIDs.noSlices.lessthan3.MC #4 subjects: 226383 (1) 294901 (1) 278154 (1) 245619 (2)
meningioma.CSU$`# of slices`[which.nas.meninCSU] 

#Find subjects with NOSE muscle as normal tissue
which.nose.meninCSU = which(meningioma.CSU$Normal_Nose == 1)
pIDs.nose.MC = unique(meningioma.CSU$PatientID[which.nose.meninCSU])
pIDs.nose.MC #3 subjects: 325202 226383 242510
meningioma.CSU$Normal_Nose[which.nose.meninCSU]

#Find samples by processors -- XY and DN
which.menin.CSU.XY = which(meningioma.CSU$Processor == "XY")
length(unique(meningioma.CSU$PatientID[which.menin.CSU.XY])) #52: Samples 1-52

which.menin.CSU.DN = which(meningioma.CSU$Processor == "DN")
length(unique(meningioma.CSU$PatientID[which.menin.CSU.DN])) #54: Samples 53-106

#Find which variables have NA's
nas.meninCSU = apply(meningioma.CSU, 2, function(x) sum(is.na(x)))
colnames(meningioma.CSU)[which(nas.meninCSU > 0)]
#[1] "Series#"       "Image#"        "DiseaseStatus" "CenterX"       "CenterY"      
#[6] "Center_SI"     "Area_cm2"      "Mean_SI"       "SD_SI"


#Find variability within the scanner covariates
colnames(meningioma.CSU)[17:23]
for(i in 17:23){
  print(colnames(meningioma.CSU)[i])
  print(length(unique(meningioma.CSU[, i])))
}

#Meningioma -- Outside
meningioma.Outside_1 = read_excel("/Users/dnandy/OneDrive - The University of Colorado Denver/CCTSI_TM-Pilot_2020-21/DataFiles/Final_DataFiles/Meningioma_Outside.xlsx",
                                sheet = 1)
meningioma.Outside_1 = as.data.frame(meningioma.Outside_1)
dim(meningioma.Outside_1) #480 x 26
colnames(meningioma.Outside_1)

#Find subjects with less than 3 slices
which.nas.meninOut = which(meningioma.Outside_1$`# of slices` < 3)
pIDs.noSlices.lessthan3.MO = unique(meningioma.Outside_1$PatientID[which.nas.meninOut])
pIDs.noSlices.lessthan3.MO #2 subjects: 321575 (2) 290955 (2)
meningioma.Outside_1$`# of slices`[which.nas.meninOut] 

#Find subjects with NOSE muscle as normal tissue
which.nose.meninOut = which(meningioma.Outside_1$Normal_Nose == 1)
length(which.nose.meninOut) #24
pIDs.nose.MO = unique(meningioma.Outside_1$PatientID[which.nose.meninOut])
pIDs.nose.MO #340085 317809 304815 297800
meningioma.Outside_1$Normal_Nose[which.nose.meninOut] 

#Find samples by processors -- XY and DN
which.menin.Out.XY = which(meningioma.Outside_1$Processor == "XY")
length(unique(meningioma.Outside_1$PatientID[which.menin.Out.XY])) #40: Samples 1-40

which.menin.Out.DN = which(meningioma.Outside_1$Processor == "DN")
length(unique(meningioma.Outside_1$PatientID[which.menin.Out.DN])) #40: Samples 41-80

#Find which variables have NA's
nas.meninOut = apply(meningioma.Outside_1, 2, function(x) sum(is.na(x)))
colnames(meningioma.Outside_1)[which(nas.meninOut > 0)]
#[1] "Series#"       "Image#"        "DiseaseStatus" "CenterX"       "CenterY"      
#[6] "Center_SI"     "Area_cm2"      "Mean_SI"       "SD_SI"         "FreqPhase1"   
#[11] "FreqPhase2"    "FOVRecon"

na.mo = Reduce(intersect, list(which(!is.na(meningioma.Outside_1$`Series#`)), which(is.na(meningioma.Outside_1$`FOVRecon`))))
na.mo.1 = Reduce(union, list(na.mo, which(is.na(meningioma.Outside_1$FreqPhase1))))
length(na.mo.1) #220

# #Write the sample info on dogs with NA's within "FOVRecon"
# write.table(unique(meningioma.Outside_1[na.mo.1, c(2:5, 22:24)]), "meningiomaOutside_FOVReconFreqPhaseNAs.csv",
#             sep = ",",
#             row.names = F,
#             col.names = T)

sum(is.na(meningioma.Outside_1$FreqPhase1)) #6 
sum(is.na(meningioma.Outside_1$FreqPhase2)) #6
sum(is.na(meningioma.Outside_1$FOVRecon)) #222 

#Find variability within the scanner covariates
colnames(meningioma.Outside_1)[18:24]
for(i in 18:24){
  print(colnames(meningioma.Outside_1)[i])
  print(length(unique(meningioma.Outside_1[, i])))
}

#Find out which sample has FreqPhase1-FreqPhase2 data missing
omit1 = which(is.na(meningioma.Outside_1$FreqPhase1))
omit2 = which(is.na(meningioma.Outside_1$FreqPhase2))
sum(omit1-omit2)

length(unique(meningioma.Outside_1$PatientID[omit1])) #1


meningioma.Outside = meningioma.Outside_1[-omit1,]
dim(meningioma.Outside) #474 x 26
sum(is.na(meningioma.Outside$FreqPhase1)) 
# install.packages("xlsx")
# library(xlsx)

#write.xlsx(meningioma.Outside, "/Users/dnandy/OneDrive - The University of Colorado Denver/CCTSI_TM-Pilot_2020-21/DataFiles/Final_DataFiles/Meningioma_Outside.xlsx",
#            row.names = F, sheetName = "Final_v2", append = T)

#Find subjects with less than 3 slices
which.nas.meninOut = which(meningioma.Outside$`# of slices` < 3)
pIDs.noSlices.lessthan3.MO = unique(meningioma.Outside$PatientID[which.nas.meninOut])
pIDs.noSlices.lessthan3.MO #2 subjects: 321575 (2) 290955 (2)
meningioma.Outside$`# of slices`[which.nas.meninOut] 

#Find subjects with NOSE muscle as normal tissue
which.nose.meninOut = which(meningioma.Outside$Normal_Nose == 1)
length(which.nose.meninOut) #24
pIDs.nose.MO = unique(meningioma.Outside$PatientID[which.nose.meninOut])
pIDs.nose.MO #340085 317809 304815 297800
meningioma.Outside$Normal_Nose[which.nose.meninOut]

#Collect all the outside sites
outlabs = Reduce(union, list(glioma.Outside$OutsideSite, meningioma.Outside$OutsideSite))
write.csv2(sort(outlabs), file = "alloutlabs.csv")
sort(outlabs)

#Check outside labs
#WestVet vsCada West vet Specialists
unique(glioma.Outside[(glioma.Outside$OutsideSite == "Cada West Vet Specialists"), 2:5])
#331176
unique(meningioma.Outside[(meningioma.Outside$OutsideSite == "WestVet"), 2:5])
#302402 296546 320409

#Compute the ratio of mean_SI(D)/mean_SI(N)
#Glioma_CSU
#glioma.CSU = read_excel("/Users/Nandyd/OneDrive - The University of Colorado Denver/CCTSI_TM-Pilot_2020-21/DataFiles/Final_DataFiles/Glioma_CSU.xlsx",
#                        sheet = 1)

glioma.CSU = as.data.frame(glioma.CSU)
colnames(glioma.CSU)
colnames(glioma.CSU)[c(1,14,15,16)] #[1] "Processor"  "Sex"  "Breed" "BreedType"    
for(l in setdiff(1:ncol(glioma.CSU), c(1,14,15,16))){
  #print(class(glioma.CSU[,l]))
  glioma.CSU[,l] = as.numeric(glioma.CSU[,l])
}

#Test if disease/normal are consecutively stored
test1 = rep(NA, nrow(glioma.CSU)/2)
for(i in 1:(nrow(glioma.CSU)/2)){
  test1[i] = glioma.CSU$DiseaseStatus[(i-1)*2+1]*glioma.CSU$DiseaseStatus[2*i]
}
sum(test1, na.rm = T)

n_glioCSU = nrow(glioma.CSU)
dnRatio.meanSI.glioCSU = rep(0, n_glioCSU/2)
dnRatio.sdSI.glioCSU = rep(0, n_glioCSU/2)
dnRatio.cvSI.glioCSU = rep(0, n_glioCSU/2)
dnRatio.centerSI.glioCSU = rep(0, n_glioCSU/2)

for(i in 1:(n_glioCSU/2)){
  if(is.na(glioma.CSU$Mean_SI[(i-1)*2 + 1])){
    dnRatio.meanSI.glioCSU[i] = NA
    dnRatio.sdSI.glioCSU[i] = NA
    dnRatio.cvSI.glioCSU[i] = NA
    dnRatio.centerSI.glioCSU[i] = NA
    
  } else {
    dnRatio.meanSI.glioCSU[i] =  glioma.CSU$Mean_SI[(i-1)*2 + 1]/glioma.CSU$Mean_SI[2*i]
    dnRatio.sdSI.glioCSU[i] =  glioma.CSU$SD_SI[(i-1)*2 + 1]/glioma.CSU$SD_SI[2*i]
    cv_d = glioma.CSU$SD_SI[(i-1)*2 + 1]/glioma.CSU$Mean_SI[(i-1)*2 + 1]
    cv_n = glioma.CSU$SD_SI[2*i]/glioma.CSU$Mean_SI[2*i]
    dnRatio.cvSI.glioCSU[i] =  cv_d/cv_n
    dnRatio.centerSI.glioCSU[i] = glioma.CSU$Center_SI[(i-1)*2 + 1]/glioma.CSU$Center_SI[2*i]
  }
}



#Glioma_Outside
#glioma.Outside = read_excel("/Users/dnandy/OneDrive - The University of Colorado Denver/CCTSI_TM-Pilot_2020-21/DataFiles/Final_DataFiles/Glioma_Outside.xlsx",
#                            sheet = 1)
glioma.Outside = as.data.frame(glioma.Outside)
colnames(glioma.Outside)
colnames(glioma.Outside)[c(1,4,15,16,17)] #[1] "Processor" "OutsideSite" "Sex"  "Breed" "BreedType"  
for(l in setdiff(1:ncol(glioma.Outside), c(1,4,15,16,17))){
  glioma.Outside[,l] = as.numeric(glioma.Outside[,l])
}

#Test if disease/normal are consecutively stored
test2 = rep(NA, nrow(glioma.Outside)/2)
for(i in 1:(nrow(glioma.Outside)/2)){
  test2[i] = glioma.Outside$DiseaseStatus[(i-1)*2+1]*glioma.Outside$DiseaseStatus[2*i]
}
sum(test2, na.rm = T)

n_glioOut = nrow(glioma.Outside) 
dnRatio.meanSI.glioOut = rep(0, n_glioOut/2)
dnRatio.sdSI.glioOut = rep(0, n_glioOut/2)
dnRatio.cvSI.glioOut = rep(0, n_glioOut/2)
dnRatio.centerSI.glioOut = rep(0, n_glioOut/2)

for(i in 1:(n_glioOut/2)){
  if(is.na(glioma.Outside$Mean_SI[(i-1)*2 + 1])){
    dnRatio.meanSI.glioOut[i] = NA
    dnRatio.sdSI.glioOut[i] = NA
    dnRatio.cvSI.glioOut[i] = NA
    dnRatio.centerSI.glioOut[i] = NA
  } else {
    dnRatio.meanSI.glioOut[i] =  glioma.Outside$Mean_SI[(i-1)*2 + 1]/glioma.Outside$Mean_SI[2*i]
    dnRatio.sdSI.glioOut[i] =  glioma.Outside$SD_SI[(i-1)*2 + 1]/glioma.Outside$SD_SI[2*i]
    cv_d = glioma.Outside$SD_SI[(i-1)*2 + 1]/glioma.Outside$Mean_SI[(i-1)*2 + 1]
    cv_n = glioma.Outside$SD_SI[2*i]/glioma.Outside$Mean_SI[2*i]
    dnRatio.cvSI.glioOut[i] =  cv_d/cv_n
    dnRatio.centerSI.glioOut[i] =  glioma.Outside$Center_SI[(i-1)*2 + 1]/glioma.Outside$Center_SI[2*i]
  }
}


#Meningioma_CSU
#meningioma.CSU = read_excel("/Users/dnandy/OneDrive - The University of Colorado Denver/CCTSI_TM-Pilot_2020-21/DataFiles/Final_DataFiles/Meningioma_CSU.xlsx",
#                            sheet = 1)
meningioma.CSU = as.data.frame(meningioma.CSU)
colnames(meningioma.CSU)
colnames(meningioma.CSU)[c(1,14,15,16)] #[1] "Processor" "Sex" "Breed" "BreedType"    
for(l in setdiff(1:ncol(meningioma.CSU), c(1,14,15,16))){
  #print(class(glioma.CSU[,l]))
  meningioma.CSU[,l] = as.numeric(meningioma.CSU[,l])
}

#Test if disease/normal are consecutively stored
test3 = rep(NA, nrow(meningioma.CSU)/2)
for(i in 1:(nrow(meningioma.CSU)/2)){
  test3[i] = meningioma.CSU$DiseaseStatus[(i-1)*2+1]*meningioma.CSU$DiseaseStatus[2*i]
}
sum(test3, na.rm = T)

n_meninCSU = nrow(meningioma.CSU)
dnRatio.meanSI.meninCSU = rep(0, n_meninCSU/2)
dnRatio.sdSI.meninCSU = rep(0, n_meninCSU/2)
dnRatio.cvSI.meninCSU = rep(0, n_meninCSU/2)
dnRatio.centerSI.meninCSU = rep(0, n_meninCSU/2)

for(i in 1:(n_meninCSU/2)){
  if(is.na(meningioma.CSU$Mean_SI[(i-1)*2 + 1])){
    dnRatio.meanSI.meninCSU[i] = NA
    dnRatio.sdSI.meninCSU[i] = NA
    dnRatio.cvSI.meninCSU[i] = NA
    dnRatio.centerSI.meninCSU[i] = NA
  } else {
    dnRatio.meanSI.meninCSU[i] =  meningioma.CSU$Mean_SI[(i-1)*2 + 1]/meningioma.CSU$Mean_SI[2*i] 
    dnRatio.sdSI.meninCSU[i] =  meningioma.CSU$SD_SI[(i-1)*2 + 1]/meningioma.CSU$SD_SI[2*i]
    cv_d = meningioma.CSU$SD_SI[(i-1)*2 + 1]/meningioma.CSU$Mean_SI[(i-1)*2 + 1]
    cv_n = meningioma.CSU$SD_SI[2*i]/meningioma.CSU$Mean_SI[2*i]
    dnRatio.cvSI.meninCSU[i] =  cv_d/cv_n
    dnRatio.centerSI.meninCSU[i] =  meningioma.CSU$Center_SI[(i-1)*2 + 1]/meningioma.CSU$Center_SI[2*i]
  }
}

#Meningioma_Outside
#meningioma.Outside = read_excel("/Users/dnandy/OneDrive - The University of Colorado Denver/CCTSI_TM-Pilot_2020-21/DataFiles/Final_DataFiles/Meningioma_Outside.xlsx",
#                                sheet = 1)
meningioma.Outside = as.data.frame(meningioma.Outside)
colnames(meningioma.Outside)
colnames(meningioma.Outside)[c(1,4,15,16,17)] #[1] "Processor" "OutsideSite" "Sex" "Breed" "BreedType"    
for(l in setdiff(1:ncol(meningioma.Outside), c(1,4,15,16,17))){
  #print(class(glioma.CSU[,l]))
  meningioma.Outside[,l] = as.numeric(meningioma.Outside[,l])
}

n_meninOut = nrow(meningioma.Outside) 
dnRatio.meanSI.meninOut = rep(0, n_meninOut/2)
dnRatio.sdSI.meninOut = rep(0, n_meninOut/2)
dnRatio.cvSI.meninOut = rep(0, n_meninOut/2)
dnRatio.centerSI.meninOut = rep(0, n_meninOut/2)

for(i in 1:(n_meninOut/2)){
  if(is.na(meningioma.Outside$Mean_SI[(i-1)*2 + 1])){
    dnRatio.meanSI.meninOut[i] = NA 
    dnRatio.sdSI.meninOut[i] = NA
    dnRatio.cvSI.meninOut[i] = NA
    dnRatio.centerSI.meninOut[i] = NA
  } else {
    dnRatio.meanSI.meninOut[i] =  meningioma.Outside$Mean_SI[(i-1)*2 + 1]/meningioma.Outside$Mean_SI[2*i]    
    dnRatio.sdSI.meninOut[i] =  meningioma.Outside$SD_SI[(i-1)*2 + 1]/meningioma.Outside$SD_SI[2*i]
    cv_d = meningioma.Outside$SD_SI[(i-1)*2 + 1]/meningioma.Outside$Mean_SI[(i-1)*2 + 1]
    cv_n = meningioma.Outside$SD_SI[2*i]/meningioma.Outside$Mean_SI[2*i]
    dnRatio.cvSI.meninOut[i] =  cv_d/cv_n
    dnRatio.centerSI.meninOut[i] =  meningioma.Outside$Center_SI[(i-1)*2 + 1]/meningioma.Outside$Center_SI[2*i]
  }
}


#Take average of (i) adjusted means, (ii) adjusted sds, (iii)adjusted CVs, and (iv) adjusted center-SIs
#across the slices (1/2/3) for each subject WITH weights 1/3, 2/3, and 1 respectively 
#WITHOUT weighting w.r.t. ROI areas

#Glioma CSU
l1 = length(dnRatio.meanSI.glioCSU) #117
mean.dnRatio.meanSI.glioCSU = mean.dnRatio.sdSI.glioCSU = 
  mean.dnRatio.cvSI.glioCSU = mean.dnRatio.centerSI.glioCSU = rep(0, l1/3)
wtmean.dnRatio.meanSI.glioCSU = wtmean.dnRatio.sdSI.glioCSU = 
  wtmean.dnRatio.cvSI.glioCSU = wtmean.dnRatio.centerSI.glioCSU = rep(0, l1/3)

for(i in 1:(l1/3)){
  nas = sum(is.na(dnRatio.meanSI.glioCSU[((i-1)*3 + 1):(3*i)]))
  wt = 1-(nas/3)
  #weighted (by # of slices) means
  wtmean.dnRatio.meanSI.glioCSU[i] = mean(dnRatio.meanSI.glioCSU[((i-1)*3 + 1):(3*i)], na.rm = T) * wt 
  wtmean.dnRatio.sdSI.glioCSU[i] = mean(dnRatio.sdSI.glioCSU[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  wtmean.dnRatio.cvSI.glioCSU[i] = mean(dnRatio.cvSI.glioCSU[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  wtmean.dnRatio.centerSI.glioCSU[i] = mean(dnRatio.centerSI.glioCSU[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  
  mean.dnRatio.meanSI.glioCSU[i] = mean(dnRatio.meanSI.glioCSU[((i-1)*3 + 1):(3*i)], na.rm = T)  
  mean.dnRatio.sdSI.glioCSU[i] = mean(dnRatio.sdSI.glioCSU[((i-1)*3 + 1):(3*i)], na.rm = T) 
  mean.dnRatio.cvSI.glioCSU[i] = mean(dnRatio.cvSI.glioCSU[((i-1)*3 + 1):(3*i)], na.rm = T) 
  mean.dnRatio.centerSI.glioCSU[i] = mean(dnRatio.centerSI.glioCSU[((i-1)*3 + 1):(3*i)], na.rm = T) 
}

#Glioma Outside
l2 = length(dnRatio.meanSI.glioOut) #60
mean.dnRatio.meanSI.glioOut = mean.dnRatio.sdSI.glioOut = 
  mean.dnRatio.cvSI.glioOut = mean.dnRatio.centerSI.glioOut = rep(0, l2/3)
wtmean.dnRatio.meanSI.glioOut = wtmean.dnRatio.sdSI.glioOut = 
  wtmean.dnRatio.cvSI.glioOut = wtmean.dnRatio.centerSI.glioOut = rep(0, l2/3)
for(i in 1:(l2/3)){
  nas = sum(is.na(dnRatio.meanSI.glioOut[((i-1)*3 + 1):(3*i)]))
  wt = 1-(nas/3)
  #weighted (by # of slices) means
  wtmean.dnRatio.meanSI.glioOut[i] = mean(dnRatio.meanSI.glioOut[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  wtmean.dnRatio.sdSI.glioOut[i] = mean(dnRatio.sdSI.glioOut[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  wtmean.dnRatio.cvSI.glioOut[i] = mean(dnRatio.cvSI.glioOut[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  wtmean.dnRatio.centerSI.glioOut[i] = mean(dnRatio.centerSI.glioOut[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  
  mean.dnRatio.meanSI.glioOut[i] = mean(dnRatio.meanSI.glioOut[((i-1)*3 + 1):(3*i)], na.rm = T) 
  mean.dnRatio.sdSI.glioOut[i] = mean(dnRatio.sdSI.glioOut[((i-1)*3 + 1):(3*i)], na.rm = T)
  mean.dnRatio.cvSI.glioOut[i] = mean(dnRatio.cvSI.glioOut[((i-1)*3 + 1):(3*i)], na.rm = T)
  mean.dnRatio.centerSI.glioOut[i] = mean(dnRatio.centerSI.glioOut[((i-1)*3 + 1):(3*i)], na.rm = T)
}

#Meningioma CSU
l3 = length(dnRatio.meanSI.meninCSU) #318
mean.dnRatio.meanSI.meninCSU = mean.dnRatio.sdSI.meninCSU = 
  mean.dnRatio.cvSI.meninCSU = mean.dnRatio.centerSI.meninCSU = rep(0, l3/3)
wtmean.dnRatio.meanSI.meninCSU = wtmean.dnRatio.sdSI.meninCSU = 
  wtmean.dnRatio.cvSI.meninCSU = wtmean.dnRatio.centerSI.meninCSU = rep(0, l3/3)
for(i in 1:(l3/3)){
  nas = sum(is.na(dnRatio.meanSI.meninCSU[((i-1)*3 + 1):(3*i)]))
  wt = 1-(nas/3)
  #weighted (by # of slices) means
  wtmean.dnRatio.meanSI.meninCSU[i] = mean(dnRatio.meanSI.meninCSU[((i-1)*3 + 1):(3*i)], na.rm = T) * wt 
  wtmean.dnRatio.sdSI.meninCSU[i] = mean(dnRatio.sdSI.meninCSU[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  wtmean.dnRatio.cvSI.meninCSU[i] = mean(dnRatio.cvSI.meninCSU[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  wtmean.dnRatio.centerSI.meninCSU[i] = mean(dnRatio.centerSI.meninCSU[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  
  mean.dnRatio.meanSI.meninCSU[i] = mean(dnRatio.meanSI.meninCSU[((i-1)*3 + 1):(3*i)], na.rm = T) 
  mean.dnRatio.sdSI.meninCSU[i] = mean(dnRatio.sdSI.meninCSU[((i-1)*3 + 1):(3*i)], na.rm = T)
  mean.dnRatio.cvSI.meninCSU[i] = mean(dnRatio.cvSI.meninCSU[((i-1)*3 + 1):(3*i)], na.rm = T)
  mean.dnRatio.centerSI.meninCSU[i] = mean(dnRatio.centerSI.meninCSU[((i-1)*3 + 1):(3*i)], na.rm = T)
}

#Meningioma Outside
l4 = length(dnRatio.meanSI.meninOut) #237
mean.dnRatio.meanSI.meninOut = mean.dnRatio.sdSI.meninOut = 
  mean.dnRatio.cvSI.meninOut = mean.dnRatio.centerSI.meninOut = rep(0, l4/3)
wtmean.dnRatio.meanSI.meninOut = wtmean.dnRatio.sdSI.meninOut = 
  wtmean.dnRatio.cvSI.meninOut = wtmean.dnRatio.centerSI.meninOut = rep(0, l4/3)

for(i in 1:(l4/3)){
  nas = sum(is.na(dnRatio.meanSI.meninOut[((i-1)*3 + 1):(3*i)]))
  wt = 1-(nas/3)
  #weighted (by # of slices) means
  wtmean.dnRatio.meanSI.meninOut[i] = mean(dnRatio.meanSI.meninOut[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  wtmean.dnRatio.sdSI.meninOut[i] = mean(dnRatio.sdSI.meninOut[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  wtmean.dnRatio.cvSI.meninOut[i] = mean(dnRatio.cvSI.meninOut[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  wtmean.dnRatio.centerSI.meninOut[i] = mean(dnRatio.centerSI.meninOut[((i-1)*3 + 1):(3*i)], na.rm = T) * wt
  
  mean.dnRatio.meanSI.meninOut[i] = mean(dnRatio.meanSI.meninOut[((i-1)*3 + 1):(3*i)], na.rm = T) 
  mean.dnRatio.sdSI.meninOut[i] = mean(dnRatio.sdSI.meninOut[((i-1)*3 + 1):(3*i)], na.rm = T)
  mean.dnRatio.cvSI.meninOut[i] = mean(dnRatio.cvSI.meninOut[((i-1)*3 + 1):(3*i)], na.rm = T)
  mean.dnRatio.centerSI.meninOut[i] = mean(dnRatio.centerSI.meninOut[((i-1)*3 + 1):(3*i)], na.rm = T)
}



# den.mean.dnRatio.meanSI.glioCSU.XY = density(mean.dnRatio.meanSI.glioCSU.XY, kernel = "epa")
# den.mean.dnRatio.meanSI.glioCSU.DN = density(mean.dnRatio.meanSI.glioCSU.DN, kernel = "epa")
# den.mean.dnRatio.meanSI.glioOut.XY = density(mean.dnRatio.meanSI.glioOut.XY, kernel = "epa")
# den.mean.dnRatio.meanSI.glioOut.DN = density(mean.dnRatio.meanSI.glioOut.DN, kernel = "epa")
# den.mean.dnRatio.meanSI.meninCSU.XY = density(mean.dnRatio.meanSI.meninCSU.XY, kernel = "epa")
# den.mean.dnRatio.meanSI.meninCSU.DN = density(mean.dnRatio.meanSI.meninCSU.DN, kernel = "epa")
# den.mean.dnRatio.meanSI.meninOut.XY = density(mean.dnRatio.meanSI.meninOut.XY, kernel = "epa")
# den.mean.dnRatio.meanSI.meninOut.DN = density(mean.dnRatio.meanSI.meninOut.DN, kernel = "epa")
# 
# min.den.labs.mean.DX = min(den.mean.dnRatio.meanSI.glioCSU.XY$y, den.mean.dnRatio.meanSI.glioCSU.DN$y,
#                            den.mean.dnRatio.meanSI.glioOut.XY$y, den.mean.dnRatio.meanSI.glioOut.DN$y,
#                            den.mean.dnRatio.meanSI.meninCSU.XY$y, den.mean.dnRatio.meanSI.meninCSU.DN$y,
#                            den.mean.dnRatio.meanSI.meninOut.XY$y, den.mean.dnRatio.meanSI.meninOut.DN$y)
# max.den.labs.mean.DX = max(den.mean.dnRatio.meanSI.glioCSU.XY$y, den.mean.dnRatio.meanSI.glioCSU.DN$y,
#                            den.mean.dnRatio.meanSI.glioOut.XY$y, den.mean.dnRatio.meanSI.glioOut.DN$y,
#                            den.mean.dnRatio.meanSI.meninCSU.XY$y, den.mean.dnRatio.meanSI.meninCSU.DN$y,
#                            den.mean.dnRatio.meanSI.meninOut.XY$y, den.mean.dnRatio.meanSI.meninOut.DN$y)
# 
# min.den.labs.mean.DX.x = min(den.mean.dnRatio.meanSI.glioCSU.XY$x, den.mean.dnRatio.meanSI.glioCSU.DN$x,
#                              den.mean.dnRatio.meanSI.glioOut.XY$x, den.mean.dnRatio.meanSI.glioOut.DN$x,
#                              den.mean.dnRatio.meanSI.meninCSU.XY$x, den.mean.dnRatio.meanSI.meninCSU.DN$x,
#                              den.mean.dnRatio.meanSI.meninOut.XY$x, den.mean.dnRatio.meanSI.meninOut.DN$x)
# max.den.labs.mean.DX.x = max(den.mean.dnRatio.meanSI.glioCSU.XY$x, den.mean.dnRatio.meanSI.glioCSU.DN$x,
#                              den.mean.dnRatio.meanSI.glioOut.XY$x, den.mean.dnRatio.meanSI.glioOut.DN$x,
#                              den.mean.dnRatio.meanSI.meninCSU.XY$x, den.mean.dnRatio.meanSI.meninCSU.DN$x,
#                              den.mean.dnRatio.meanSI.meninOut.XY$x, den.mean.dnRatio.meanSI.meninOut.DN$x)
# 
# plot(den.mean.dnRatio.meanSI.glioCSU.XY, 
#      main = "Density Plot of mean adjusted mean(SI)\nXY vs. DN",
#      xlab = "Mean Adjusted Mean(SI)",
#      col=2,
#      ylim = c(min.den.labs.mean.DX, max.den.labs.mean.DX),
#      xlim = c(min.den.labs.mean.DX.x,max.den.labs.mean.DX.x)
# )
# 
# lines(den.mean.dnRatio.meanSI.glioCSU.DN, col = 2, lwd = 4)
# lines(den.mean.dnRatio.meanSI.glioOut.XY, col = 2, lty = 2)
# lines(den.mean.dnRatio.meanSI.glioOut.DN, col = 2, lty = 2, lwd = 4)
# lines(den.mean.dnRatio.meanSI.meninCSU.XY, col = 1)
# lines(den.mean.dnRatio.meanSI.meninCSU.DN, col = 1, lwd = 4)
# lines(den.mean.dnRatio.meanSI.meninOut.XY, col = 1, lty=2)
# lines(den.mean.dnRatio.meanSI.meninOut.DN, col = 1, lty=2, lwd = 4)
# 
# 
# legend('topright', col = c(rep(1,3),2),
#        lty = c(1,1,2,1), lwd = c(1,4,1,1), c("CSU-Menin-XY", "CSU-Menin-DN",
#                                              "Out-Menin-XY", "CSU-Glio-XY"))
# dev.off()


#Boxplots -- adjusted SDs
#Glioma -- CSU
mean.dnRatio.sdSI.glioCSU.XY = mean.dnRatio.sdSI.glioCSU[1:19]
mean.dnRatio.sdSI.glioCSU.DN = mean.dnRatio.sdSI.glioCSU[20:39]
#Glioma -- Outside
mean.dnRatio.sdSI.glioOut.XY = mean.dnRatio.sdSI.glioOut[1:11]
mean.dnRatio.sdSI.glioOut.DN = mean.dnRatio.sdSI.glioOut[12:20]

#Meningioma -- CSU
mean.dnRatio.sdSI.meninCSU.XY = mean.dnRatio.sdSI.meninCSU[1:52]
mean.dnRatio.sdSI.meninCSU.DN = mean.dnRatio.sdSI.meninCSU[53:106]
#Meningioma -- Outside
mean.dnRatio.sdSI.meninOut.XY = mean.dnRatio.sdSI.meninOut[1:40]
mean.dnRatio.sdSI.meninOut.DN = mean.dnRatio.sdSI.meninOut[41:79]

png("DNXY_boxplot_final_sdSI.png",
    width = 12, height = 8, units = "in", res = 300)
boxplot(mean.dnRatio.sdSI.glioCSU.XY,
        mean.dnRatio.sdSI.glioCSU.DN,
        mean.dnRatio.sdSI.glioOut.XY,
        mean.dnRatio.sdSI.glioOut.DN,
        mean.dnRatio.sdSI.meninCSU.XY,
        mean.dnRatio.sdSI.meninCSU.DN,
        mean.dnRatio.sdSI.meninOut.XY,
        mean.dnRatio.sdSI.meninOut.DN, 
        col=c(2,2,3,3,4,4,5,5),
        #notch = T,
        names = c("GC-XY","GC-DN","GO-XY","GO-DN",
                  "MC-XY","MC-DN","MO-XY","MO-DN"),
        ylab = "μ(adj-SD(SI))",
        xlab="Sub-populations",
        main = "Processors (DN & XY): μ(adj-SD(SI))\n(across four sub-populations)"
)
dev.off()


#Density plots -- adjusted cent(SI)s
#Glioma -- CSU
mean.dnRatio.centSI.glioCSU.XY = mean.dnRatio.centerSI.glioCSU[1:19]
mean.dnRatio.centSI.glioCSU.DN = mean.dnRatio.centerSI.glioCSU[20:39]
#Glioma -- Outside
mean.dnRatio.centSI.glioOut.XY = mean.dnRatio.centerSI.glioOut[1:11]
mean.dnRatio.centSI.glioOut.DN = mean.dnRatio.centerSI.glioOut[12:20]

#Meningioma -- CSU
mean.dnRatio.centSI.meninCSU.XY = mean.dnRatio.centerSI.meninCSU[1:52]
mean.dnRatio.centSI.meninCSU.DN = mean.dnRatio.centerSI.meninCSU[53:106]
#Meningioma -- Outside
mean.dnRatio.centSI.meninOut.XY = mean.dnRatio.centerSI.meninOut[1:40]
mean.dnRatio.centSI.meninOut.DN = mean.dnRatio.centerSI.meninOut[41:79]

png("DNXY_boxplot_final_centSI.png",
    width = 12, height = 8, units = "in", res = 300)
boxplot(mean.dnRatio.centSI.glioCSU.XY,
        mean.dnRatio.centSI.glioCSU.DN,
        mean.dnRatio.centSI.glioOut.XY,
        mean.dnRatio.centSI.glioOut.DN,
        mean.dnRatio.centSI.meninCSU.XY,
        mean.dnRatio.centSI.meninCSU.DN,
        mean.dnRatio.centSI.meninOut.XY,
        mean.dnRatio.centSI.meninOut.DN, 
        col=c(2,2,3,3,4,4,5,5),
        #notch = T,
        names = c("GC-XY","GC-DN","GO-XY","GO-DN",
                  "MC-XY","MC-DN","MO-XY","MO-DN"),
        ylab = "μ(adj-cent(SI))",
        xlab="Sub-populations",
        main = "Processors (DN & XY): μ(adj-cent(SI))\n(across four sub-populations)"
)
dev.off()

#Results: Baseline classification of diseases
#Apply logistic regression

#Meningioma + Glioma: all CSU cases
menin.CSU.image3 = data.frame(adjmeanSI = mean.dnRatio.meanSI.meninCSU, 
                              adjsdSI = mean.dnRatio.sdSI.meninCSU, 
                              adjcenterSI = mean.dnRatio.centerSI.meninCSU)
dim(menin.CSU.image3) #106 x 3

glio.CSU.image3 = data.frame(adjmeanSI = mean.dnRatio.meanSI.glioCSU, 
                             adjsdSI = mean.dnRatio.sdSI.glioCSU, 
                             adjcenterSI = mean.dnRatio.centerSI.glioCSU)
dim(glio.CSU.image3) #39 x 3

meninglio.CSU.image3 = rbind(menin.CSU.image3, glio.CSU.image3)
#Meningioma = 1, Glioma = 0
meninglio.CSU.image3$dis.lab = c(rep(1, nrow(menin.CSU.image3)), rep(0, nrow(glio.CSU.image3)))

#Meningioma + Glioma: all Outside cases
menin.Out.image3 = data.frame(adjmeanSI = mean.dnRatio.meanSI.meninOut, 
                              adjsdSI = mean.dnRatio.sdSI.meninOut, 
                              adjcenterSI = mean.dnRatio.centerSI.meninOut)
dim(menin.Out.image3) #79 x 3

glio.Out.image3 = data.frame(adjmeanSI = mean.dnRatio.meanSI.glioOut, 
                             adjsdSI = mean.dnRatio.sdSI.glioOut, 
                             adjcenterSI = mean.dnRatio.centerSI.glioOut)
dim(glio.Out.image3) #20 x 3

meninglio.Out.image3 = rbind(menin.Out.image3, glio.Out.image3)
#Meningioma = 1, Glioma = 0
meninglio.Out.image3$dis.lab = c(rep(1, nrow(menin.Out.image3)), rep(0, nrow(glio.Out.image3)))

library(knitr)
library(tidyverse)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(mice)
library(lattice)
library(reshape2)
library(DataExplorer)
library(caret)
library(corrplot)
library(RColorBrewer)

#CSU
head(meninglio.CSU.image3)
str(meninglio.CSU.image3)
summary(meninglio.CSU.image3)
introduce(meninglio.CSU.image3)
table(meninglio.CSU.image3$dis.lab)

png("meninglioCSU_corr_final_3img_dislab.png",
    width = 12, height = 8, units = "in", res = 300)
cor.meninglioCSU.3imgDis = cor(meninglio.CSU.image3)
corrplot.mixed(cor.meninglioCSU.3imgDis, 
              order="AOE")
#plot_correlation(meninglio.CSU.image3,
#                 title = "Inter-correlations among 3 image variables and tumor-types\n Site: CSU | 1 = Meningioma, 0 = Glioma",
#                 #xlab = "Variables",
#                 ggtheme = theme(plot.title = element_text(hjust=0.5, face = "bold")))
dev.off()

# png("meninglioCSU_hist_final_3img.png",
#     width = 12, height = 6, units = "in", res = 300)
# plot_histogram(meninglio.CSU.image3,
#                  title = "Histograms of the 3 MRI Variables [CSU]",
#                  ggtheme = theme(plot.title = element_text(hjust=0.5, face = "bold")))
# dev.off()

#Outside sites
head(meninglio.Out.image3)
str(meninglio.Out.image3)
summary(meninglio.Out.image3)
introduce(meninglio.Out.image3)
table(meninglio.Out.image3$dis.lab)

png("meninglioOut_corr_final_3img_dislab.png",
    width = 12, height = 8, units = "in", res = 300)
cor.meninglioOut.3imgDis = cor(meninglio.Out.image3)
corrplot.mixed(cor.meninglioOut.3imgDis, 
               order="AOE")
# plot_correlation(meninglio.Out.image3,
#                  title = "Correlations among the 3 MRI Variables & Disease Labels [Outside]\n[1 = Meningioma, 0 = Glioma]",
#                  ggtheme = theme(plot.title = element_text(hjust=0.5, face = "bold")))
dev.off()

# png("Out_hist_3meanadjMRIvars.png",
#     width = 12, height = 8, units = "in", res = 300)
# plot_histogram(meninglio.Out.image3,
#                title = "Histograms of the 3 MRI Variables [Outside]",
#                ggtheme = theme(plot.title = element_text(hjust=0.5, face = "bold")))
# dev.off()

#install.packages("AID")
library(AID)
#Box-Cox transform the adjsdSI variable
#CSU

meninglio.CSU.image3.1 = meninglio.CSU.image3
colnames(meninglio.CSU.image3.1)
# meninglio.CSU.image3.1[, 2] = log(meninglio.CSU.image3$adjsdSI)
# colnames(meninglio.CSU.image3.1)[2] = "log.adjsdSI"
# plot_correlation(meninglio.CSU.image3.1)
# plot_histogram(meninglio.CSU.image3.1)

# shapiro.test(meninglio.CSU.image3.1$log.adjsdSI) #pval = 0.6469
# shapiro.test(meninglio.CSU.image3.1$adjmeanSI) #pval = 0.1651
# shapiro.test(meninglio.CSU.image3.1$adjcenterSI) #pval = 0.2808

#Outside sites

#tran.adjsdSI.Out = boxcoxnc(meninglio.Out.image3$adjsdSI, method = "sw")

meninglio.Out.image3.1 = meninglio.Out.image3
colnames(meninglio.Out.image3.1)
# meninglio.Out.image3.1[, 2] = tran.adjsdSI.Out$tf.data
# colnames(meninglio.Out.image3.1)[2] = "boxcox.adjsdSI"
# plot_correlation(meninglio.Out.image3.1)
# plot_histogram(meninglio.Out.image3.1)

# shapiro.test(meninglio.Out.image3.1$boxcox.adjsdSI) #pval = 0.9962
# shapiro.test(meninglio.Out.image3.1$adjmeanSI) #pval = 0.907
# shapiro.test(meninglio.Out.image3.1$adjcenterSI) #pval = 0.1203

#Add biological covariates: Sex, Age, BreedType
#Add technical covariate: Processor 
#Add scanner covariates: TR, TE, NEX, Thick, FreqPhase1, FreqPhase2

#########################  CSU ##############################
# CSU -- Meningioma
#############################################################
l.meninCSU = nrow(menin.CSU.image3)
agemos.meninCSU = sex.meninCSU = breedType.meninCSU = 
  processor.meninCSU = 
  TR.meninCSU = TE.meninCSU = NEX.meninCSU = thick.meninCSU = 
  freq1.meninCSU = freq2.meninCSU = rep(NA, l.meninCSU)

for(i in 1:l.meninCSU){
  agemos.meninCSU[i] = meningioma.CSU$Age_month[(i-1)*6+1]
  sex.meninCSU[i] = meningioma.CSU$Sex[(i-1)*6+1]
  breedType.meninCSU[i] = meningioma.CSU$BreedType[(i-1)*6+1]
  processor.meninCSU[i] = meningioma.CSU$Processor[(i-1)*6+1]
  TR.meninCSU[i] = meningioma.CSU$TR[(i-1)*6+1]
  TE.meninCSU[i] = meningioma.CSU$TE[(i-1)*6+1]
  NEX.meninCSU[i] = meningioma.CSU$NEX[(i-1)*6+1]
  thick.meninCSU[i] = meningioma.CSU$Thick[(i-1)*6+1]
  freq1.meninCSU[i] = meningioma.CSU$FreqPhase1[(i-1)*6+1]
  freq2.meninCSU[i] = meningioma.CSU$FreqPhase2[(i-1)*6+1]
}

#Age
round(summary(agemos.meninCSU), 4)
round(c(median(agemos.meninCSU), mad(agemos.meninCSU)), 4)

#Sex
table(sex.meninCSU) #Just F: 3, Just M: 3, F SP: 51; M CAS: 49
f.sex.meninCSU = which(sex.meninCSU == "F SP" | sex.meninCSU == "FEMALE SP" | sex.meninCSU == "FEMALE")
sex.meninCSU[f.sex.meninCSU] = "F"
m.sex.meninCSU = which(sex.meninCSU == "M CAS" | sex.meninCSU == "MALE CAS" | sex.meninCSU == "MALE")
sex.meninCSU[m.sex.meninCSU] = "M"
table(sex.meninCSU) #F: 54, M: 52


#Breedtype
table(breedType.meninCSU) #Brachy: 15; Non-brachy: 91

#Processor
table(processor.meninCSU) #DN: 54, XY: 52

#TR
round(summary(TR.meninCSU), 4)
round(c(median(TR.meninCSU), mad(TR.meninCSU)), 4)

#TE
round(summary(TE.meninCSU), 4)
round(c(median(TE.meninCSU), mad(TE.meninCSU)), 4)

#NEX
round(summary(NEX.meninCSU), 4)
round(c(median(NEX.meninCSU), mad(NEX.meninCSU)), 4)

#Thick
round(summary(thick.meninCSU), 4)
round(c(median(thick.meninCSU), mad(thick.meninCSU)), 4)

#Frequency Phases 1/2
plot(freq1.meninCSU, freq2.meninCSU)
round(summary(freq1.meninCSU), 4)
round(c(median(freq1.meninCSU), mad(freq1.meninCSU)), 4)
length(unique(freq1.meninCSU)) #4

round(summary(freq2.meninCSU), 4)
round(c(median(freq2.meninCSU), mad(freq2.meninCSU)), 4)
length(unique(freq2.meninCSU)) #2

#MR Image Variables
summary(menin.CSU.image3)
round(apply(menin.CSU.image3, 2, median), 4)
round(apply(menin.CSU.image3, 2, mad), 4)

#########################  CSU ##############################
# CSU -- Glioma
#############################################################
l.glioCSU = nrow(glio.CSU.image3)
agemos.glioCSU = sex.glioCSU = breedType.glioCSU = 
  processor.glioCSU = 
  TR.glioCSU = TE.glioCSU = NEX.glioCSU = thick.glioCSU = 
  freq1.glioCSU = freq2.glioCSU = rep(NA, l.glioCSU)

for(i in 1:l.glioCSU){
  agemos.glioCSU[i] = glioma.CSU$Age_month[(i-1)*6+1]
  sex.glioCSU[i] = glioma.CSU$Sex[(i-1)*6+1]
  breedType.glioCSU[i] = glioma.CSU$BreedType[(i-1)*6+1]
  processor.glioCSU[i] = glioma.CSU$Processor[(i-1)*6+1]
  TR.glioCSU[i] = glioma.CSU$TR[(i-1)*6+1]
  TE.glioCSU[i] = glioma.CSU$TE[(i-1)*6+1]
  NEX.glioCSU[i] = glioma.CSU$NEX[(i-1)*6+1]
  thick.glioCSU[i] = glioma.CSU$Thick[(i-1)*6+1]
  freq1.glioCSU[i] = glioma.CSU$FreqPhase1[(i-1)*6+1]
  freq2.glioCSU[i] = glioma.CSU$FreqPhase2[(i-1)*6+1]
}

#Age
round(summary(agemos.glioCSU), 4)
round(c(median(agemos.glioCSU), mad(agemos.glioCSU)), 4)

#Sex
table(sex.glioCSU) #Just F: 1, Just M: 1, F SP: 11; M CAS: 9
f.sex.glioCSU = which(sex.glioCSU == "F SP" | sex.glioCSU == "FEMALE SP" | sex.glioCSU == "FEMALE")
sex.glioCSU[f.sex.glioCSU] = "F"
m.sex.glioCSU = which(sex.glioCSU == "M CAS" | sex.glioCSU == "MALE CAS" | sex.glioCSU == "MALE")
sex.glioCSU[m.sex.glioCSU] = "M"
table(sex.glioCSU) #F: 21, M: 18

#Breedtype
table(breedType.glioCSU) #Brachy: 15; Non-brachy: 24

#Processor
table(processor.glioCSU) #DN: 20, XY: 19

#TR
round(summary(TR.glioCSU), 4)
round(c(median(TR.glioCSU), mad(TR.glioCSU)), 4)

#TE
round(summary(TE.glioCSU), 4)
round(c(median(TE.glioCSU), mad(TE.glioCSU)), 4)

#NEX
round(summary(NEX.glioCSU), 4)
round(c(median(NEX.glioCSU), mad(NEX.glioCSU)), 4)

#Thick
round(summary(thick.glioCSU), 4)
round(c(median(thick.glioCSU), mad(thick.glioCSU)), 4)

#Frequency Phases 1/2
plot(freq1.glioCSU, freq2.glioCSU)
round(summary(freq1.glioCSU), 4)
round(c(median(freq1.glioCSU), mad(freq1.glioCSU)), 4)
length(unique(freq1.glioCSU)) #3

round(summary(freq2.glioCSU), 4)
round(c(median(freq2.glioCSU), mad(freq2.glioCSU)), 4)
length(unique(freq2.glioCSU)) #3

#MR Image Variables
summary(glio.CSU.image3)
round(apply(glio.CSU.image3, 2, median), 4)
round(apply(glio.CSU.image3, 2, mad), 4)

#Combine Meningiom/Glioma for CSU
agemos.meninglio.CSU = c(agemos.meninCSU, agemos.glioCSU)
hist(agemos.meninglio.CSU) #Pretty normally distributed
shapiro.test(agemos.meninglio.CSU) # p = 0.1814

sex.meninglio.CSU = c(sex.meninCSU, sex.glioCSU)
table(sex.meninglio.CSU) #F: 75, M: 70
sex.meninglio.CSU = ifelse(sex.meninglio.CSU == "F", 0, 1)
table(sex.meninglio.CSU)#0 (F): 75, 1 (M): 70

breedType.meninglio.CSU = c(breedType.meninCSU, breedType.glioCSU)
table(breedType.meninglio.CSU) #Brachy: 30, Non-brachy: 115
breedType.meninglio.CSU = ifelse(breedType.meninglio.CSU == "Non-brachycephalic", 0, 1)
table(breedType.meninglio.CSU) # 1 (Brachycephalic): 30, 0 (Non-brachycephalic): 115

processor.meninglio.CSU = c(processor.meninCSU, processor.glioCSU)
table(processor.meninglio.CSU) #DN: 74, XY: 71
processor.meninglio.CSU = ifelse(processor.meninglio.CSU == "XY", 0, 1)
table(processor.meninglio.CSU) # 1 (DN): 74, 0 (XY): 71


TR.meninglio.CSU = c(TR.meninCSU, TR.glioCSU)
hist(TR.meninglio.CSU)
shapiro.test(TR.meninglio.CSU) # p = 2.651E-05

TE.meninglio.CSU = c(TE.meninCSU, TE.glioCSU)
hist(TE.meninglio.CSU)
shapiro.test(TE.meninglio.CSU) # p = 2.651E-05

NEX.meninglio.CSU = c(NEX.meninCSU, NEX.glioCSU)
hist(NEX.meninglio.CSU)
shapiro.test(NEX.meninglio.CSU) # p = 1.37E-09

thick.meninglio.CSU = c(thick.meninCSU, thick.glioCSU)
hist(thick.meninglio.CSU)
shapiro.test(thick.meninglio.CSU) # p = 5.242E-12

freq1.meninglio.CSU = c(freq1.meninCSU, freq1.glioCSU)
hist(freq1.meninglio.CSU)
shapiro.test(freq1.meninglio.CSU) # p = 5.409E-12

freq2.meninglio.CSU = c(freq2.meninCSU, freq2.glioCSU)
hist(freq2.meninglio.CSU)
shapiro.test(freq2.meninglio.CSU) # p < 2.2E-16


#Add biological covariates: Sex, Age, Breed-type
#Add technical covariates: Processor
#Add scanner covariates: TR, TE, NEX, Thick, FreqPhase1, FreqPhase2

#########################  Outside ##############################
# Outside -- Meningioma
#################################################################
l.meninOut = nrow(menin.Out.image3)
agemos.meninOut = sex.meninOut = breedType.meninOut = 
  processor.meninOut = 
  TR.meninOut = TE.meninOut = NEX.meninOut = thick.meninOut = 
  freq1.meninOut = freq2.meninOut = rep(NA, l.meninOut)

for(i in 1:l.meninOut){
  agemos.meninOut[i] = meningioma.Outside$Age_month[(i-1)*6+1]
  sex.meninOut[i] = meningioma.Outside$Sex[(i-1)*6+1]
  breedType.meninOut[i] = meningioma.Outside$BreedType[(i-1)*6+1]
  processor.meninOut[i] = meningioma.Outside$Processor[(i-1)*6+1]
  TR.meninOut[i] = meningioma.Outside$TR[(i-1)*6+1]
  TE.meninOut[i] = meningioma.Outside$TE[(i-1)*6+1]
  NEX.meninOut[i] = meningioma.Outside$NEX[(i-1)*6+1]
  thick.meninOut[i] = meningioma.Outside$Thick[(i-1)*6+1]
  freq1.meninOut[i] = meningioma.Outside$FreqPhase1[(i-1)*6+1]
  freq2.meninOut[i] = meningioma.Outside$FreqPhase2[(i-1)*6+1]
}

#Age
round(summary(agemos.meninOut), 4)
round(c(median(agemos.meninOut), mad(agemos.meninOut)), 4)

#Sex
table(sex.meninOut) #Just F: 1, Just M: 3, F SP: 20; M CAS: 18
f.sex.meninOut = which(sex.meninOut == "F SP" | sex.meninOut == "FEMALE SP" | sex.meninOut == "FEMALE")
sex.meninOut[f.sex.meninOut] = "F"
m.sex.meninOut = which(sex.meninOut == "M CAS" | sex.meninOut == "MALE CAS" | sex.meninOut == "MALE")
sex.meninOut[m.sex.meninOut] = "M"
table(sex.meninOut) #F: 40, M: 39

#Breedtype
table(breedType.meninOut) #Brachy: 7; Non-brachy: 72

#Processor
table(processor.meninOut) #DN: 39, XY: 40

#TR
round(summary(TR.meninOut), 4)
round(c(median(TR.meninOut), mad(TR.meninOut)), 4)

#TE
round(summary(TE.meninOut), 4)
round(c(median(TE.meninOut), mad(TE.meninOut)), 4)

#NEX
round(summary(NEX.meninOut), 4)
round(c(median(NEX.meninOut), mad(NEX.meninOut)), 4)

#Thick
round(summary(thick.meninOut), 4)
round(c(median(thick.meninOut), mad(thick.meninOut)), 4)

#Frequency Phases 1/2
plot(freq1.meninOut, freq2.meninOut)
summary(freq1.meninOut)
round(c(median(freq1.meninOut), mad(freq1.meninOut)), 4)
length(unique(freq1.meninOut)) #8

summary(freq2.meninOut)
round(c(median(freq2.meninOut), mad(freq2.meninOut)), 4)
length(unique(freq2.meninOut)) #19

#MR Image Variables
summary(menin.Out.image3)
round(apply(menin.Out.image3, 2, median), 4)
round(apply(menin.Out.image3, 2, mad), 4)

#########################  Outside ##############################
# Outside -- Glioma
#################################################################
l.glioOut = nrow(glio.Out.image3)
agemos.glioOut = sex.glioOut = breedType.glioOut = 
  processor.glioOut = 
  TR.glioOut = TE.glioOut = NEX.glioOut = thick.glioOut = 
  freq1.glioOut = freq2.glioOut = rep(NA, l.glioOut)

for(i in 1:l.glioOut){
  agemos.glioOut[i] = glioma.Outside$Age_month[(i-1)*6+1]
  sex.glioOut[i] = glioma.Outside$Sex[(i-1)*6+1]
  processor.glioOut[i] = glioma.Outside$Processor[(i-1)*6+1]
  breedType.glioOut[i] = glioma.Outside$BreedType[(i-1)*6+1]
  TR.glioOut[i] = glioma.Outside$TR[(i-1)*6+1]
  TE.glioOut[i] = glioma.Outside$TE[(i-1)*6+1]
  NEX.glioOut[i] = glioma.Outside$NEX[(i-1)*6+1]
  thick.glioOut[i] = glioma.Outside$Thick[(i-1)*6+1]
  freq1.glioOut[i] = glioma.Outside$FreqPhase1[(i-1)*6+1]
  freq2.glioOut[i] = glioma.Outside$FreqPhase2[(i-1)*6+1]
}

#Age
round(summary(agemos.glioOut), 4)
round(c(median(agemos.glioOut), mad(agemos.glioOut)), 4)

#Sex
table(sex.glioOut) #Just F: 0, Just M: 1; MALE: 1; FEMALE SP: 7; F SP: 7; M CAS: 3; MALE CAS: 3
f.sex.glioOut = which(sex.glioOut == "F SP" | sex.glioOut == "FEMALE SP" | sex.glioOut == "FEMALE")
sex.glioOut[f.sex.glioOut] = "F"
m.sex.glioOut = which(sex.glioOut == "M CAS" | sex.glioOut == "MALE CAS" | sex.glioOut == "MALE")
sex.glioOut[m.sex.glioOut] = "M"
table(sex.glioOut) #F: 14, M: 6

#Breedtype
table(breedType.glioOut) #Brachy: 10; Non-brachy: 10

#Processor
table(processor.glioOut) #DN: 9, XY: 11

#TR
round(summary(TR.glioOut), 4)
round(c(median(TR.glioOut), mad(TR.glioOut)), 4)

#TE
round(summary(TE.glioOut), 4)
round(c(median(TE.glioOut), mad(TE.glioOut)), 4)

#NEX
round(summary(NEX.glioOut), 4)
round(c(median(NEX.glioOut), mad(NEX.glioOut)), 4)

#Thick
round(summary(thick.glioOut), 4)
round(c(median(thick.glioOut), mad(thick.glioOut)), 4)

#Frequency Phases 1/2
plot(freq1.glioOut, freq2.glioOut)
round(summary(freq1.glioOut), 4)
round(c(median(freq1.glioOut), mad(freq1.glioOut)), 4)
length(unique(freq1.glioOut)) #7

round(summary(freq2.glioOut), 4)
round(c(median(freq2.glioOut), mad(freq2.glioOut)), 4)
length(unique(freq2.glioOut)) #19

#MR Image Variables
summary(glio.Out.image3)
round(apply(glio.Out.image3, 2, median), 4)
round(apply(glio.Out.image3, 2, mad), 4)

#Combine Meningioma and Glioma
agemos.meninglio.Out = c(agemos.meninOut, agemos.glioOut)
hist(agemos.meninglio.Out) #Pretty normally distributed
shapiro.test(agemos.meninglio.Out) # p = 0.9167

sex.meninglio.Out = c(sex.meninOut, sex.glioOut)
table(sex.meninglio.Out) #F: 54, M: 45
sex.meninglio.Out = ifelse(sex.meninglio.Out == "F", 0, 1)
table(sex.meninglio.Out)#0 (F): 54, 1 (M): 45

breedType.meninglio.Out = c(breedType.meninOut, breedType.glioOut)
table(breedType.meninglio.Out) #Brachy: 17, Non-brachy: 82
breedType.meninglio.Out = ifelse(breedType.meninglio.Out == "Non-brachycephalic", 0, 1)
table(breedType.meninglio.Out) # 1 (Brachycephalic): 17, 0 (Non-brachycephalic): 82

processor.meninglio.Out = c(processor.meninOut, processor.glioOut)
table(processor.meninglio.Out) #DN: 48, XY: 51
processor.meninglio.Out = ifelse(processor.meninglio.Out == "XY", 0, 1)
table(processor.meninglio.Out) # 1 (DN): 48, 0 (XY): 51

TR.meninglio.Out = c(TR.meninOut, TR.glioOut)
hist(TR.meninglio.Out)
shapiro.test(TR.meninglio.Out) # p = 2.878E-10

TE.meninglio.Out = c(TE.meninOut, TE.glioOut)
hist(TE.meninglio.Out)
shapiro.test(TE.meninglio.Out) # p = 2.41E-06

NEX.meninglio.Out = c(NEX.meninOut, NEX.glioOut)
hist(NEX.meninglio.Out)
shapiro.test(NEX.meninglio.Out) # p = 5.508E-10

thick.meninglio.Out = c(thick.meninOut, thick.glioOut)
hist(thick.meninglio.Out)
shapiro.test(thick.meninglio.Out) # p = 9.776E-09

freq1.meninglio.Out = c(freq1.meninOut, freq1.glioOut)
hist(freq1.meninglio.Out)
shapiro.test(freq1.meninglio.Out) # p = 2.561E-10

freq2.meninglio.Out = c(freq2.meninOut, freq2.glioOut)
hist(freq2.meninglio.Out)
shapiro.test(freq2.meninglio.Out) # p = 0.0001523


#CSU
#X-matrix with only 3 Clinical Variables
meninglio.CSU.image3.0 = data.frame(agemos = agemos.meninglio.CSU,
                                    sexMF = sex.meninglio.CSU,
                                    breedTypeBO = breedType.meninglio.CSU)
dim(meninglio.CSU.image3.0) #145 x 3
plot_correlation(meninglio.CSU.image3.0)
plot_histogram(meninglio.CSU.image3.0)

#X-matrix with 3 Image Variables + 3 Clinical Variables
meninglio.CSU.image3.2 = data.frame(meninglio.CSU.image3.1,
                                    agemos = agemos.meninglio.CSU,
                                    sexMF = sex.meninglio.CSU,
                                    breedTypeBO = breedType.meninglio.CSU)
dim(meninglio.CSU.image3.2) #145 x 7
plot_correlation(meninglio.CSU.image3.2)
plot_histogram(meninglio.CSU.image3.2)

#X-matrix with 3 Image Vars + 3 Clinical Covs + 1 technical Covs + 6 scanner covs 
meninglio.CSU.image3.3 = data.frame(meninglio.CSU.image3.1,
                                    agemos = agemos.meninglio.CSU,
                                    sexMF = sex.meninglio.CSU,
                                    breedTypeBO = breedType.meninglio.CSU,
                                    proDX = processor.meninglio.CSU,
                                    TR = TR.meninglio.CSU,
                                    TE = TE.meninglio.CSU,
                                    NEX = NEX.meninglio.CSU,
                                    Thick = thick.meninglio.CSU,
                                    Freq1 = freq1.meninglio.CSU,
                                    Freq2 = freq2.meninglio.CSU)
dim(meninglio.CSU.image3.3) #145 x 14
plot_correlation(meninglio.CSU.image3.3)
plot_histogram(meninglio.CSU.image3.3)

#Correlation tests for predictor variables
ncol3 = ncol(meninglio.CSU.image3.3)
cortest.CSU = matrix(NA, ncol3, ncol3)
colnames(cortest.CSU) = rownames(cortest.CSU) = colnames(meninglio.CSU.image3.3)
for(i in 1:(ncol3-1)){
  for(j in (i+1):ncol3){
    cortest = cor.test(meninglio.CSU.image3.3[, i], meninglio.CSU.image3.3[,j], 
                       method = "pearson")
    cortest.CSU[i,j] = cortest$p.value
    cortest.CSU[j,i] = cortest.CSU[i,j]
  }
}
png("heatmap_corr_13vars_CSU_meninglio.png",
    width = 12, height = 8, units = "in", res = 300)
heatmap(cor(meninglio.CSU.image3.3[,-4]),
        main = "Heatmap of correlations among all 13 covariates [CSU]",
        margins = c(8,8),
        keep.dendro = F)
dev.off()
round(cortest.CSU,3)

#Outside sites
#X-matrix with only 3 Clinical Variables
meninglio.Out.image3.0 = data.frame(agemos = agemos.meninglio.Out,
                                    sexMF = sex.meninglio.Out,
                                    breedTypeBO = breedType.meninglio.Out)
dim(meninglio.Out.image3.0) #99 x 3
plot_correlation(meninglio.Out.image3.0)
plot_histogram(meninglio.Out.image3.0)

#X-matrix with 3 Image Variables + 3 Clinical Variables
meninglio.Out.image3.2 = data.frame(meninglio.Out.image3.1,
                                    agemos = agemos.meninglio.Out,
                                    sexMF = sex.meninglio.Out,
                                    breedTypeBO = breedType.meninglio.Out)
dim(meninglio.Out.image3.2) #99 x 7
plot_correlation(meninglio.Out.image3.2)
plot_histogram(meninglio.Out.image3.2)

meninglio.Out.image3.3 = data.frame(meninglio.Out.image3.1,
                                    agemos = agemos.meninglio.Out,
                                    sexMF = sex.meninglio.Out,
                                    breedTypeBO = breedType.meninglio.Out,
                                    proDX = processor.meninglio.Out,
                                    TR = TR.meninglio.Out,
                                    TE = TE.meninglio.Out,
                                    NEX = NEX.meninglio.Out,
                                    Thick = thick.meninglio.Out,
                                    Freq1 = freq1.meninglio.Out,
                                    Freq2 = freq2.meninglio.Out)
dim(meninglio.Out.image3.3) #99 x 14
plot_correlation(meninglio.Out.image3.3)
plot_histogram(meninglio.Out.image3.3)




#save.image("dataPreprocessing_CCTSIproject_v2.RData")
