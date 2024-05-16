# LDV OLS analysis on 3 COVID waves
## Table 4 is a sample comprised of a repeated cross-section
modelsummary_Table4 <- function(wave1,wave2,wave3) {
  wave1 <- wave1 %>%
    mutate(economic_activity = 
             as.factor(case_when(
               economic_activity == "In University" |
                 economic_activity == "In Training/Edu" ~ 
                 "In Training/Edu",
               .default = economic_activity)),
           economic_activity = 
             relevel(economic_activity, ref = "In Training/Edu")) 
  
  wave1 <- mice::as.mids(wave1)
  wave2 <- mice::as.mids(wave2)
  wave3 <- mice::as.mids(wave3)
  
  k6_wave1 <- with(wave1,
                   coeftest(lm(covid_K6~
                        existing_K6+cov_symp+ethnicity+region+
                        residence*male+
                        economic_activity+moved+
                        fam_ses_par_ed+
                        W6_lowincome+fam_ses_overcrowded+
                        step_7+single_7, 
                      weights = CW1_COMBWT)), vcov. = sandwich)
  k6_wave1 <- mice::pool(k6_wave1)
  wb_wave1 <- with(wave1,
                   coeftest(lm(covid_SWEMWBS~
                        existing_SWEMWBS+
                        cov_symp+ethnicity+region+
                        residence*male+moved+
                        economic_activity+
                        fam_ses_par_ed+
                        W6_lowincome+fam_ses_overcrowded+
                        step_7+single_7, 
                      weights = CW1_COMBWT)), vcov. = sandwich)
  wb_wave1 <- mice::pool(wb_wave1)  
  k6_wave2 <- with(wave2,
                   coeftest(lm(CW2_Covid_K6  ~ 
                        existing_K6+CW2_cov_symp+
                        CW2_moved+Ethnicity+CW2_Country+
                        CW2_residence*Male+
                        CW2_Economic.Activity+
                        fam_ses_par_ed+
                        W6_lowincome+CW2_Overcrowded+
                        W7_steppar+W7_single_par, 
                      weights = CW2_COMBWT)), vcov. = sandwich)
  k6_wave2 <- mice::pool(k6_wave2)
  wb_wave2 <- with(wave2,
                   coeftest(lm(CW2_Covid_SWEMWBS ~
                        existing_SWEMWBS+CW2_cov_symp+
                        CW2_moved+Ethnicity+CW2_Country+
                        CW2_residence*Male+
                        CW2_Economic.Activity+
                        fam_ses_par_ed+
                        W6_lowincome+CW2_Overcrowded+
                        W7_steppar+W7_single_par, 
                      weights = CW2_COMBWT)), vcov. = sandwich)
  wb_wave2 <- mice::pool(wb_wave2)
  k6_wave3 <- with(wave3,
                   coeftest(lm(CW3_Covid_K6  ~ 
                        existing_K6+CW3_cov_symp+
                        CW3_moved+Ethnicity+CW3_Country+
                        CW3_residence*Male+
                        CW3_Economic.Activity+
                        fam_ses_par_ed+W6_lowincome+
                        CW3_Overcrowded+
                        W7_steppar+W7_single_par, 
                      weights = CW3_COMBWT)), vcov. = sandwich)
  k6_wave3 <- mice::pool(k6_wave3)
  wb_wave3 <- with(wave3,
                   coeftest(lm(CW3_Covid_SWEMWBS ~
                        existing_SWEMWBS+CW3_cov_symp+
                        CW3_moved+Ethnicity+CW3_Country+
                        CW3_residence*Male+
                        CW3_Economic.Activity+
                        fam_ses_par_ed+
                        W6_lowincome+CW3_Overcrowded+
                        W7_steppar+W7_single_par, 
                      weights = CW3_COMBWT)), vcov. = sandwich)
  wb_wave3 <- mice::pool(wb_wave3)
  
  models <- list("Wave 1" = wb_wave1,
                 "Wave 2" = wb_wave2,
                 "Wave 3" = wb_wave3,
                 "Wave 1" = k6_wave1,
                 "Wave 2" = k6_wave2,
                 "Wave 3" = k6_wave3)
  
  cm <- c("(Intercept)" = 'Constant', #1
          "existing_K6"  = "LDV (Age 17)", #2
          "existing_SWEMWBS" = "LDV (Age 17)", #2
          "MaleMale" = "Male", #3
          "maleMale" = "Male", #3
          "CW3_residenceLeft parental home" = 
            "Female, Left Parental Home", #4
          "CW2_residenceLeft parental home" = 
            "Female, Left Parental Home", #4
          "residenceLeft parental home" = 
            "Female, Left Parental Home", #4
          'CW3_residenceLiving with parents & siblings' = 
            'Female, Living with Parents & Siblings', #5
          'CW2_residenceLiving with parents & siblings' = 
            'Female, Living with Parents & Siblings', #5
          'residenceLiving with parents & siblings' = 
            'Female, Living with Parents & Siblings', #5
          "CW2_residenceLeft parental home:MaleMale" = 
            'Male, Left the Parental Home', #6
          "CW3_residenceLeft parental home:MaleMale" = 
            'Male, Left the Parental Home', #6
          "residenceLeft parental home:maleMale" = 
            'Male, Left the Parental Home', #6
          'residenceLiving with parents & siblings:maleMale' = 
            'Male, Living with Parents & Siblings', #7
          'CW2_residenceLiving with parents & siblings:MaleMale' = 
            'Male, Living with Parents & Siblings', #7
          'CW3_residenceLiving with parents & siblings:MaleMale' = 
            'Male, Living with Parents & Siblings', #7
          'CW2_movedYes' = 'Living Arrangements Changed since Mar ‘20', #8
          'CW3_movedYes' = 'Living Arrangements Changed since Mar ‘20', #8
          'movedYes' = 'Living Arrangements Changed since Mar ‘20', #8
          "economic_activityEmployed" = "Employed", #9
          "CW2_Economic.ActivityEmployed" = "Employed", #9
          "CW3_Economic.ActivityEmployed" = "Employed", #9
          "economic_activityFurloughed" = "Furloughed", #10
          "CW2_Economic.ActivityFurloughed" = "Furloughed", #10
          "CW3_Economic.ActivityFurloughed" = "Furloughed", #10
          "economic_activityOther Inactive" = "Other Inactive", #11
          "CW2_Economic.ActivityOther Inactive" = "Other Inactive", #11
          "CW3_Economic.ActivityOther Inactive" = "Other Inactive", #11
          "economic_activityUnemployed" = "Unemployed", #12
          "CW2_Economic.ActivityUnemployed" = "Unemployed", #12
          "CW3_Economic.ActivityUnemployed" = "Unemployed", #12
          "fam_ses_par_edYes" = "High Parental Education", #13
          "W6_lowincomeYes" = "Low Family Income", #14
          "step_7Yes" = "Has Stepparent", #15
          "W7_stepparYes" = "Has Stepparent", #15
          "W7_single_parYes" = "Has Single Parent", #16
          "single_7Yes" = "Has Single Parent", #16
          "fam_ses_overcrowdedYes" = "Overcrowded COVID-19 HH", #17
          "CW2_OvercrowdedYes" = "Overcrowded COVID-19 HH", #17
          "CW3_OvercrowdedYes" = "Overcrowded COVID-19 HH" #17
  )
  
  gof <- tibble::tribble(
    ~raw,~clean,~fmt,
    "nobs","N",0)
  
  table <- modelsummary(models,
                        output = "gt",
                        fmt = 2,
                        coef_map = cm,
                        gof_map = gof,
                        estimate = "{estimate}{stars} ({std.error})",
                        statistic = NULL,
                        stars = c('+' = 0.1, '*' = .05, '**' = .01))
  
  table <- table %>%
    tab_row_group(
      id = "constant",
      label = "",
      rows = c(1,2)
    ) %>%
    tab_row_group(
      id = "fam",
      label = md("*Young Adult's Childhood Family Factors*"),
      rows = c(13:17)
    ) %>%
    tab_row_group(
      id = "employment",
      label = md("*(Ref: In Training, Education, or University)*"),
      rows = c(9:12)
    ) %>%
    tab_row_group(
      id = "moved",
      label = md("**Control Factors**"),
      rows = c(8)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Male, Living with Parents, No Siblings)*"),
      rows = c(6,7)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Female, Living with Parents, No Siblings)*"),
      rows = c(4,5)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Female)*"),
      rows = c(3)
    ) %>%
    tab_options(table.width = pct(90)) %>%
    cols_align(align="center",
               columns = c(2:7)) %>%
    tab_spanner(
      label = md("Coef. < 0 ~ better mental health"),
      columns = c(5:7)
    ) %>%
    tab_spanner(
      label = md("K6"),
      columns = c(5:7),
      level = 2
    ) %>%
    tab_spanner(
      label = md("Coef. > 0 ~ better mental health"),
      columns = c(2:4)
    ) %>%
    tab_spanner(
      label = md("SWEMWBS"),
      columns = c(2:4),
      level = 2
    ) %>%
    tab_source_note(source_note = md("*Notes*: OLS regression estimates are weighted. Standard errors are in parentheses. Significance: '+' p < 0.1, '*' p < 0.05, '**' p < 0.01. All models control for country of residence (England, Scotland, Wales, Northern Ireland), ethnicity (non-white), and experience of COVID-19 symptoms. Each wave has a different sample in accordance with the CLS COVID survey sampling strategy. Respondents with missing values on key variables are filled with multiple imputation of chained equations."))
  
  table %>% gtsave(filename = '04_Table_3w.main.rtf',
                   path = 'Figures_Tables/')
  
  return(table)
}
## Table A3 is a a panel sample comprised of only those who responded to every wave
modelsummary_TableA3 <- function(wave1,wave2,wave3) {
  MCSIDs <- wave1 %>% 
    filter(MCSID %in% wave2$MCSID &
             MCSID %in% wave3$MCSID)   
  wave1 <- wave1 %>%
    mutate(economic_activity = 
             as.factor(case_when(
               economic_activity == "In University" |
                 economic_activity == "In Training/Edu" ~ 
                 "In Training/Edu",
               .default = economic_activity)),
           economic_activity = 
             relevel(economic_activity, ref = "In Training/Edu")) %>% 
    filter(MCSID %in% MCSIDs$MCSID)
  wave2 <- wave2 %>%
     filter(MCSID %in% MCSIDs$MCSID)
   wave3 <- wave3 %>%
     filter(MCSID %in% MCSIDs$MCSID)
  print(table(wave1$economic_activity)) 
  wave1 <- mice::as.mids(wave1)
  wave2 <- mice::as.mids(wave2)
  wave3 <- mice::as.mids(wave3)
  
  k6_wave1 <- with(wave1,
                   coeftest(lm(covid_K6~
                        existing_K6+cov_symp+ethnicity+region+
                        residence*male+
                        economic_activity+moved+
                        fam_ses_par_ed+
                        W6_lowincome+fam_ses_overcrowded+
                        step_7+single_7, 
                      weights = CW1_COMBWT)), vcov. = sandwich)
  k6_wave1 <- mice::pool(k6_wave1)
  wb_wave1 <- with(wave1,
                   coeftest(lm(covid_SWEMWBS~
                        existing_SWEMWBS+
                        cov_symp+ethnicity+region+
                        residence*male+moved+
                        economic_activity+
                        fam_ses_par_ed+
                        W6_lowincome+fam_ses_overcrowded+
                        step_7+single_7, 
                      weights = CW1_COMBWT)), vcov. = sandwich)
  wb_wave1 <- mice::pool(wb_wave1)  
  k6_wave2 <- with(wave2,
                   coeftest(lm(CW2_Covid_K6  ~ 
                        existing_K6+CW2_cov_symp+
                        CW2_moved+Ethnicity+CW2_Country+
                        CW2_residence*Male+
                        CW2_Economic.Activity+
                        fam_ses_par_ed+
                        W6_lowincome+CW2_Overcrowded+
                        W7_steppar+W7_single_par, 
                      weights = CW2_COMBWT)), vcov. = sandwich)
  k6_wave2 <- mice::pool(k6_wave2)
  wb_wave2 <- with(wave2,
                   coeftest(lm(CW2_Covid_SWEMWBS ~
                        existing_SWEMWBS+CW2_cov_symp+
                        CW2_moved+Ethnicity+CW2_Country+
                        CW2_residence*Male+
                        CW2_Economic.Activity+
                        fam_ses_par_ed+
                        W6_lowincome+CW2_Overcrowded+
                        W7_steppar+W7_single_par, 
                      weights = CW2_COMBWT)), vcov. = sandwich)
  wb_wave2 <- mice::pool(wb_wave2)
  k6_wave3 <- with(wave3,
                   coeftest(lm(CW3_Covid_K6  ~ 
                        existing_K6+CW3_cov_symp+
                        CW3_moved+Ethnicity+CW3_Country+
                        CW3_residence*Male+
                        CW3_Economic.Activity+
                        fam_ses_par_ed+W6_lowincome+
                        CW3_Overcrowded+
                        W7_steppar+W7_single_par, 
                      weights = CW3_COMBWT)), vcov. = sandwich)
  k6_wave3 <- mice::pool(k6_wave3)
  wb_wave3 <- with(wave3,
                   coeftest(lm(CW3_Covid_SWEMWBS ~
                        existing_SWEMWBS+CW3_cov_symp+
                        CW3_moved+Ethnicity+CW3_Country+
                        CW3_residence*Male+
                        CW3_Economic.Activity+
                        fam_ses_par_ed+
                        W6_lowincome+CW3_Overcrowded+
                        W7_steppar+W7_single_par, 
                      weights = CW3_COMBWT)), vcov. = sandwich)
  wb_wave3 <- mice::pool(wb_wave3)
  
  models <- list("Wave 1" = wb_wave1,
                 "Wave 2" = wb_wave2,
                 "Wave 3" = wb_wave3,
                 "Wave 1" = k6_wave1,
                 "Wave 2" = k6_wave2,
                 "Wave 3" = k6_wave3)
  
  cm <- c("(Intercept)" = 'Constant', #1
          "existing_K6"  = "LDV (Age 17)", #2
          "existing_SWEMWBS" = "LDV (Age 17)", #2
          "MaleMale" = "Male", #3
          "maleMale" = "Male", #3
          "CW3_residenceLeft parental home" = 
            "Female, Left Parental Home", #4
          "CW2_residenceLeft parental home" = 
            "Female, Left Parental Home", #4
          "residenceLeft parental home" = 
            "Female, Left Parental Home", #4
          'CW3_residenceLiving with parents & siblings' = 
            'Female, Living with Parents & Siblings', #5
          'CW2_residenceLiving with parents & siblings' = 
            'Female, Living with Parents & Siblings', #5
          'residenceLiving with parents & siblings' = 
            'Female, Living with Parents & Siblings', #5
          "CW2_residenceLeft parental home:MaleMale" = 
            'Male, Left the Parental Home', #6
          "CW3_residenceLeft parental home:MaleMale" = 
            'Male, Left the Parental Home', #6
          "residenceLeft parental home:maleMale" = 
            'Male, Left the Parental Home', #6
          'residenceLiving with parents & siblings:maleMale' = 
            'Male, Living with Parents & Siblings', #7
          'CW2_residenceLiving with parents & siblings:MaleMale' = 
            'Male, Living with Parents & Siblings', #7
          'CW3_residenceLiving with parents & siblings:MaleMale' = 
            'Male, Living with Parents & Siblings', #7
          'CW2_movedYes' = 'Living Arrangements Changed since Mar ‘20', #8
          'CW3_movedYes' = 'Living Arrangements Changed since Mar ‘20', #8
          'movedYes' = 'Living Arrangements Changed since Mar ‘20', #8
          "economic_activityEmployed" = "Employed", #9
          "CW2_Economic.ActivityEmployed" = "Employed", #9
          "CW3_Economic.ActivityEmployed" = "Employed", #9
          "economic_activityFurloughed" = "Furloughed", #10
          "CW2_Economic.ActivityFurloughed" = "Furloughed", #10
          "CW3_Economic.ActivityFurloughed" = "Furloughed", #10
          "economic_activityOther Inactive" = "Other Inactive", #11
          "CW2_Economic.ActivityOther Inactive" = "Other Inactive", #11
          "CW3_Economic.ActivityOther Inactive" = "Other Inactive", #11
          "economic_activityUnemployed" = "Unemployed", #12
          "CW2_Economic.ActivityUnemployed" = "Unemployed", #12
          "CW3_Economic.ActivityUnemployed" = "Unemployed", #12
          "fam_ses_par_edYes" = "High Parental Education", #13
          "W6_lowincomeYes" = "Low Family Income", #14
          "step_7Yes" = "Has Stepparent", #15
          "W7_stepparYes" = "Has Stepparent", #15
          "W7_single_parYes" = "Has Single Parent", #16
          "single_7Yes" = "Has Single Parent", #16
          "fam_ses_overcrowdedYes" = "Overcrowded COVID-19 HH", #17
          "CW2_OvercrowdedYes" = "Overcrowded COVID-19 HH", #17
          "CW3_OvercrowdedYes" = "Overcrowded COVID-19 HH" #17
          )
  
  gof <- tibble::tribble(
    ~raw,~clean,~fmt,
    "nobs","N",0)
  
  table <- modelsummary(models,
                        output = "gt",
                        fmt = 2,
                        coef_map = cm,
                        gof_map = NULL,
                        estimate = "{estimate}{stars} ({std.error})",
                        statistic = NULL,
                        stars = c('+' = 0.1, '*' = .05, '**' = .01))
  
  table <- table %>%
    tab_row_group(
      id = "constant",
      label = "",
      rows = c(1,2)
    ) %>%
    tab_row_group(
      id = "fam",
      label = md("*Young Adult's Childhood Family Factors*"),
      rows = c(13:17)
    ) %>%
    tab_row_group(
      id = "employment",
      label = md("*(Ref: In Training, Education, or University)*"),
      rows = c(9:12)
    ) %>%
    tab_row_group(
      id = "moved",
      label = md("**Control Factors**"),
      rows = c(8)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Male, Living with Parents, No Siblings)*"),
      rows = c(6,7)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Female, Living with Parents, No Siblings)*"),
      rows = c(4,5)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Female)*"),
      rows = c(3)
    ) %>%
    tab_options(table.width = pct(90)) %>%
    cols_align(align="center",
               columns = c(2:7)) %>%
    tab_spanner(
      label = md("Coef. < 0 ~ better mental health"),
      columns = c(5:7)
    ) %>%
    tab_spanner(
      label = md("K6"),
      columns = c(5:7),
      level = 2
    ) %>%
    tab_spanner(
      label = md("Coef. > 0 ~ better mental health"),
      columns = c(2:4)
    ) %>%
    tab_spanner(
      label = md("SWEMWBS"),
      columns = c(2:4),
      level = 2
    ) %>%
    tab_source_note(source_note = md("*Notes*: OLS regression estimates are weighted with cluster robust standard errors in parentheses. Significance: '+' p < 0.1, '*' p < 0.05, '**' p < 0.01. All models control for country of residence (England, Scotland, Wales, Northern Ireland), ethnicity (non-white), and experience of COVID-19 symptoms. The sample (N = 1,471) is derived from people who participated in Wave 7 of MCS mainstage survey and all waves of the CLS COVID-19 survey. Missing values are filled with multiple imputation of chained equations for 30 iterations."))
  
  table %>% gtsave(filename = 'A03_Table_3w.main.rtf',
                   path = 'Figures_Tables/')
  
  return(table)
}