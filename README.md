# ComBat_DogBrainMRI
This repository contains the R-codes for the analyses, tables, and figures presented in the manuscript entitled "MRI data harmonization across sites using ComBat enhances classification of meningioma and glioma brain tumors in dogs: a case study" by Nandy et al.

## R scripts

1. 3clinLowerUpper_RF_SMOTE_75iters_050321.R

     Case 0 pre-ComBat lower bounds and upper bounds; no ComBat harmonization involved; only three clinical covariates in the RF model 
  
2. 3imgVars_prepostComBat_051021.R

     Pre-ComBat lower bounds, Case 1 scenarios a and b post-ComBat lower bounds, and upper bounds; only three image variables in the RF model
  
3. ComBat_3img3clin_050721.R 

     Case 1 scenarios c and d post-ComBat lower bounds; three image variables and three clinical covariates in the RF model
  
4. preComBat_3img3clin_LowerUpper_050721.R

     Pre-ComBat lower bounds and upper bounds; three image variables and three clinical covariates in the RF model

5. ComBat_Scenario3_allcovs_090321.R

     Case 2, scenarios a-d, post-ComBat lower bounds
  
6. Figures.R

     Codes generating Figures 1, S2-S7
  
7. TableS6Case1_Case0.R

     Codes generating the entries of and performing comparisons within Tables 1 and S6 (Case 1) and comparisons among Case 0 and Case 1 scenarios
  
8. TableS7_Case2.R

     Codes generating the entries of and performing comparisons within Table S7 (Case 2)
  
9. dataPreprocessing_CCTSIproject_v2.R

     Codes for preprocessing the original data to create: (1) the final data frames that are used for analyses in Cases 0, 1, and 2 and (2) the entries for Tables S1-S3 

## RData files
   ### All the RData files required to run the above scripts are available within this OneDrive folder: https://olucdenver-my.sharepoint.com/:f:/g/personal/debmalya_nandy_cuanschutz_edu/EvumZKP_09FAoLusQBQSJagBBv7xLUn74_86ZeuQByVptA?e=j4u6w0 (passcode: dogMRI@CCTSI2021)
