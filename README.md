# ComBat_DogBrainMRI
This repository contains the R-codes for the analyses, tables, and figures presented in the manuscript entitled "MRI data harmonization across sites using ComBat enhances classification of meningioma and glioma brain tumors in dogs: a case study" by Nandy et al.

## R scripts
### 3clinLowerUpper_RF_SMOTE_75iters_050321.R
  #### Case 0 lower and upper bounds; no ComBat harmonization involved; using only three clinical covariates in the RF model 
### 3imgVars_prepostComBat_051021.R -- pre-ComBat lower bounds, Case 1a, 1b post-ComBat lower bounds, and upper bounds; using only three image variables in the RF model
### ComBat_3img3clin_050721.R -- Case 1c, 1d post-ComBat lower bounds; using three image variables and three clinical covariates in the RF model
### preComBat_3img3clin_LowerUpper_050721.R -- pre-ComBat lower bounds and upper bounds; using three image variables and three clinical covariates in the RF model
### ComBat_Scenario3_allcovs_090321.R -- Case 2, scenarios a-d, post-ComBat lower bounds
### Figures.R -- Codes generating Figures 1A-B and S2-S7
### TableS6Case1_Case0.R -- Codes generating the entries and performing comparisons within Tables 1 and S6 (Case 1) and comparisons among Case 0 and Case 1 scenarios
### TableS7_Case2.R -- Codes generating the entries and performing comparisons within Table S7 (Case 2)
### dataPreprocessing_CCTSIproject_v2.R -- Codes for preprocessing the original data to create the final data frames used for analyses in Cases 0, 1, and 2. Also creates the entries for Tables S1-S3

## RData files
All the RData files required to run the above scripts are available within this OneDrive folder: https://olucdenver-my.sharepoint.com/:f:/g/personal/debmalya_nandy_cuanschutz_edu/EvumZKP_09FAoLusQBQSJagBBv7xLUn74_86ZeuQByVptA?e=j4u6w0 (passcode: dogMRI@CCTSI2021)
