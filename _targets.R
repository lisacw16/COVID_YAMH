# This file has been generated from the template produced by running the command targets::use_targets(). The author followed the comments available in the _targets.R template write this script. Further information about the targets package and set-up can be found in: https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline 

library(targets)
library(plm)
# set target options to load the packages that the scripted targets need to run in the global environment
tar_option_set(
  packages = c("srvyr", "gt", "gtsummary", "modelsummary", "haven", "Hmisc", "weights", "knitr", "tidyverse", "ggpubr", "webshot2", "lmtest", "sandwich", "RColorBrewer",'tseries', "mice", "patchwork"), 
  format = "rds")

# (unchanged from template) tar_make_clustermq() configuration:
options(clustermq.scheduler = "multicore")

# Configure where the R scripts with custom functions will be found:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)

# The rest of this file contains a target list. This target list draws information from the loaded files and the R folder (with the custom functions). The list has been filled according to the data analysis flow (i.e., data cleaning, exploration, analysis, robustness checks):

list(
  
  ######### READ FILES INTO PROGRAM ########
  
  # Note: You do not need to update these paths as they are associated with the working directory - as long as your files are saved within the working directory (e.g., in a folder called MCS_data). 

  tar_target(cov_file,
             "MCS_data/mcs_Cov/spss/spss25/covid-19_wave1_survey_cls.sav", 
             format = "file"),
  tar_target(cov2_file,
             "MCS_data/mcs_Cov/spss/spss25/covid-19_wave2_survey_cls.sav",
             format = "file"),
  tar_target(cov3_file,
             "MCS_data/mcs_Cov/spss/spss25/covid-19_wave3_survey_cls.sav",
             format = "file"),
  tar_target(hh7_grid_file,
             "MCS_data/mcs_wave7/tab/mcs7_hhgrid.tab",
             format = "file"),
  tar_target(hh6_grid_file,
             "MCS_data/mcs_wave6/tab/mcs6_hhgrid.tab",
             format = "file"),
  tar_target(hh5_grid_file,
             "MCS_data/mcs_wave5/tab/mcs5_hhgrid.tab",
             format = "file"),
  tar_target(hh4_grid_file,
             "MCS_data/mcs_wave4/tab/mcs4_hhgrid.tab",
             format = "file"),
  tar_target(hh3_grid_file,
             "MCS_data/mcs_wave3/tab/mcs3_hhgrid.tab",
             format = "file"),
  tar_target(hh2_grid_file,
             "MCS_data/mcs_wave2/tab/mcs2_hhgrid.tab", 
             format = "file"),
  tar_target(hh1_grid_file,
             "MCS_data/mcs_wave1/tab/mcs1_hhgrid.tab",
             format = "file"),
  tar_target(family7_derived_file,
             "MCS_data/mcs_wave7/tab/mcs7_family_derived.tab",
             format = "file"),
  tar_target(family6_derived_file,
             "MCS_data/mcs_wave6/tab/mcs6_family_derived.tab",
             format = "file"),
  tar_target(family5_derived_file,
             "MCS_data/mcs_wave5/tab/mcs5_family_derived.tab",
             format = "file"),
  tar_target(family4_derived_file,
             "MCS_data/mcs_wave4/tab/mcs4_family_derived.tab", 
             format = "file"),
  tar_target(family3_derived_file,
             "MCS_data/mcs_wave3/tab/mcs3_family_derived.tab", 
             format = "file"),
  tar_target(family2_derived_file,
             "MCS_data/mcs_wave2/tab/mcs2_family_derived.tab",
             format = "file"),
  tar_target(family1_derived_file,
             "MCS_data/mcs_wave1/tab/mcs1_family_derived.tab",
             format = "file"),
  tar_target(cm7_derived_file,
             "MCS_data/mcs_wave7/tab/mcs7_cm_derived.tab",
             format = "file"),
  tar_target(cm6_derived_file,
             "MCS_data/mcs_wave6/tab/mcs6_cm_derived.tab",
             format = "file"),
  tar_target(cm5_derived_file,
             "MCS_data/mcs_wave5/tab/mcs5_cm_derived.tab",
             format = "file"),
  tar_target(cm4_derived_file,
             "MCS_data/mcs_wave4/tab/mcs4_cm_derived.tab",
             format = "file"),
  tar_target(cm3_derived_file,
             "MCS_data/mcs_wave3/tab/mcs3_cm_derived.tab", 
             format = "file"),
  tar_target(cm2_derived_file,
             "MCS_data/mcs_wave2/tab/mcs2_cm_derived.tab",
             format = "file"),
  tar_target(cm1_derived_file,
             "MCS_data/mcs_wave1/tab/mcs1_cm_derived.tab",
             format = "file"),
  tar_target(cm4_int_file,
             "MCS_data/mcs_wave4/tab/mcs4_cm_interview.tab",
             format = "file"),
  tar_target(cm5_int_file,
             "MCS_data/mcs_wave5/tab/mcs5_cm_interview.tab",
             format = "file"),
  tar_target(cm6_int_file,
             "MCS_data/mcs_wave6/tab/mcs6_cm_interview.tab",
             format = "file"),
  tar_target(cm7_int_file,
             "MCS_data/mcs_wave7/tab/mcs7_cm_interview.tab",
             format = "file"),
  tar_target(parent5_derived_file,
             "MCS_data/mcs_wave5/tab/mcs5_parent_derived.tab",
             format = "file"),
  tar_target(parent6_derived_file,
             "MCS_data/mcs_wave6/tab/mcs6_parent_derived.tab",
             format = "file"),
  
  ######################  DATASET CONSTRUCTION  ######################
  
  #The `combine_functions_dfs` function: 
  ## 1. cleans the data in each file, 
  ## 2. derives relevant variables, 
  ## 3. combines data files into one large working dataset 
  tar_target(working_data, 
             combine_functions_dfs(cov_file,cm6_int_file,
                                   cm5_int_file,hh7_grid_file,
                                   hh6_grid_file,hh5_grid_file,
                                   hh4_grid_file,hh3_grid_file,
                                   hh2_grid_file,hh1_grid_file,
                                   family7_derived_file,
                                   family6_derived_file,
                                   family5_derived_file,
                                   family4_derived_file,
                                   family3_derived_file,
                                   family2_derived_file,
                                   family1_derived_file,
                                   parent6_derived_file,
                                   parent5_derived_file,
                                   cm7_derived_file,cm6_derived_file,
                                   cm5_derived_file,cm4_derived_file,
                                   cm3_derived_file,cm2_derived_file,
                                   cm1_derived_file)),
  # The `select_analysis_vars` function:
  # 1. selects only necessary variables for analysis
  # 2. relevels categorical variables for analysis
  tar_target(selected_df, 
             select_analysis_vars(working_data)),
  # The `allcovwaves_wide_df` function:
  # 1. constructs a wide dataset containing all waves of data
  tar_target(alloutcomes_wide, 
             allcovwaves_wide_df(cov_file,cov2_file,cov3_file,
                                 cm7_int_file,cm6_int_file,
                                 cm5_int_file,cm4_int_file,
                                 hh7_grid_file,hh6_grid_file,
                                 hh5_grid_file,hh4_grid_file,
                                 hh3_grid_file,hh2_grid_file,
                                 hh1_grid_file,
                                 family7_derived_file,
                                 family6_derived_file,
                                 family5_derived_file,
                                 family4_derived_file,
                                 family3_derived_file,
                                 family2_derived_file,
                                 family1_derived_file,
                                 parent6_derived_file,
                                 parent5_derived_file,
                                 cm7_derived_file,
                                 cm6_derived_file,
                                 cm5_derived_file,
                                 cm4_derived_file,
                                 cm3_derived_file,
                                 cm2_derived_file,
                                 cm1_derived_file)),
  # `selected_df` and `alloutcomes_wide` are going to be the core 
  # analysis datasets. 
  # The `multipleimpute_cv.w#` and `multipleimpute_sibcv.w1` functions:
  # 1. runs a multiple imputation with 30 reps
  # 2. relevels categorical variables for analysis
  tar_target(imputed.w1, 
             multipleimpute.w1_function(selected_df)),
  tar_target(imputed_sib, 
             multipleimpute_sibcv.w1(selected_df)),
  tar_target(imputed.w2,
             multipleimpute.w2_function(alloutcomes_wide)),
  tar_target(imputed.w3,
             multipleimpute.w3_function(alloutcomes_wide)),
  tar_target(imputed_w0.3_long, 
             mi_0.3_long(alloutcomes_wide)),
  
  ########################  PRODUCE FIGURES  ########################
    
  # Figure 1:
  ## 1. Target `Figure_Df` summarises the `selected_df` data.frame in terms of mean and variance across observed mental health measures.
  ## 2. `Figure01_cc.K6` uses information in `Figure_Df` to produce a line graph across two time points (MCS Wave 7 and CLS COVID Wave 1) for K6 outcomes. `Figure01_cc.WB` does the same for SWEMWBS outcomes. 
  ## 3. Combine the two figures by hand in Microsoft Word to make Figure 1
  
  tar_target(Figure1, 
             Figure1_function(imputed.w1)),
   tar_target(FigureX, 
              Figure1B_function(imputed.w1,
                                imputed.w2,
                                imputed.w3)),
  
  ####################  PRODUCE DESCRIPTIVE TABLES  ####################
  
  # Main Descriptive Tables
  
  ## Target 'Table01_cc.descriptives' provides complete-case descriptives of:
  ### 1. overall sample N weighted and unweighted (with and without siblings)
  ### 2. missing observations counted and labelled "number imputed". 
  
  tar_target(Table01_w1.cc.desc, 
             descriptive_table(selected_df)),
  ## Table A02 presents two separate tables, similar to Table 1, side-by-side. The authors generate each table separately and then merge them together by hand using Microsoft Word. 
  tar_target(TableA2_w2.cc.desc, 
             TableA02_w2_descriptives
             (alloutcomes_wide)),
  tar_target(TableA2_w3.cc.desc, 
             TableA02_w3_descriptives
             (alloutcomes_wide)),
  ## !!! The complete-case descriptive tables must be corrected with imputed values !!! #
  tar_target(Table01_w1.mi.desc,
             descriptives_mi.cv.w1(imputed.w1)),
  tar_target(TableA01_w2.mi.desc,
             descriptives_mi.cv.w2(imputed.w2)),
  tar_target(TableA01_w3.mi.desc,
             descriptives_mi.cv.w3(imputed.w3)),
  
  # Extra Descriptive Tables
  
  ## We also create some descriptives to look at patterns of those living away from the parental home or missing outcome responses (these are verbally reported in the manuscript)
  tar_target(descriptives_alone, 
             descriptive_table_alone(selected_df)),
  tar_target(descriptives_na, 
             descriptive_table_missing(selected_df)),
  
  ## The crosstab provides information about the sample of MCS members who participated in all three waves. The code generates two files, each with a crosstab of W1 x W(2-3). These two files must be combined by hand using a word processor. In the manuscript tables, the authors also included  two rows of N via summing each column by hand.
  tar_target(TableA01_crosstab,
             crosstab_table(alloutcomes_wide)),
  
  ######################  PRODUCE ANALYSIS TABLES   ######################
  
  # Lagged Dependent Variable OLS Regressions
  
  tar_target(Table02_mi,
             modelsummary_w1mi(imputed.w1)),
  tar_target(Table03_mi,
             modelsummary_sib(imputed_sib)),
  tar_target(Table04_mi,
             modelsummary_Table4(imputed.w1,
                                 imputed.w2,
                                 imputed.w3)),
  tar_target(TableA3_mi,
             modelsummary_TableA3(imputed.w1,
                                  imputed.w2,
                                  imputed.w3)),
  
  # Fixed Effect Estimation, t = 0:3
  
  tar_target(femodel_TableA04_mi,
             fixedmodels_0.3(imputed_w0.3_long)),
  tar_target(A03_Table_fe, 
             modelsummary_femodels(
               femodel_TableA04_mi,
               "A04_Table_fe"))
)
