multipleimpute_sibcv.w1 <- function(data){
  data <- data %>%
    filter(residence != "Living with siblings, no parents") %>%
    mutate(residence = droplevels(residence)) %>%
    filter(num_sibling > 0) %>%
    select(-c(transition,residence2,
              flag_5qs,
              existing_residence,
              Children,Grandparent,Friend,
              CW1_CNUM00,
              CW1_HHNUMWH_6,
              CW1_HHNUM,
              CW1_HHNUMWH_3,
              CW1_COVPER,
              GDOTHS00,
              parents17,
              siblings17,
              W6_income,
              W4_income,
              W3_income,
              W2_income,
              W1_income,
              par_structure_single,
              par_structure_step
              )) 
  print(table(data$sib_bully6, useNA = 'ifany'))
  print(str(data))
  p_missing <- unlist(
    lapply(data, 
           function(x) sum(is.na(x))))/nrow(data)
  print(sort(p_missing[p_missing > 0], decreasing = TRUE))
  
  imp <- mice::mice(data, maxit=0)
  meth <-  imp$method
  predM <- imp$predictorMatrix
  predM[, c("CW1_COMBWT")] <- 0
  predM[, c("MCSID")] <- 0
  meth[c("covid_K6",
         "covid_SWEMWBS",
         "existing_K6",
         "existing_SWEMWBS")] <- "norm"
  meth[c("moved",
         "W6_lowincome",
         "fam_ses_overcrowded",
         "step_7",
         "single_7",
         "fam_ses_par_ed",
         "conflict",
         "cov_symp",
         "sib_bully6")] <- "logreg"
  meth[c("residence",
         "economic_activity",
         "ethnicity",
         "region")] <- "polyreg"
  
  imp2 <- mice::mice(data, m = 30, maxit = 20,
                     method = meth,
                     predictorMatrix = predM,
                     print =  FALSE,
                     seed = 5000)
  
  mice::complete(imp2, action="long", include = TRUE) %>%
    select(-c(starts_with("CW1_WEMWBS"),
              starts_with("CW1_PH")))
}

descriptives_sib.imp <- function(df){
  ma_data <- filter(df, male == "Male")
  fe_data <-filter(df, male == "Female")
  
  # CONTINUOUS VARIBALE SUMMARIES #
  overall <- continuous_sib.summary(mice::as.mids(df),"Overall")
  Female <- continuous_summary(mice::as.mids(fe_data),"Female")
  Male <- continuous_summary(mice::as.mids(ma_data),"Male")
  
  # BINARY VARIBALE SUMMARIES #
  overall_bin <- binary_sib.summary(df,"Overall")
  female_bin <- binary_sib.summary(fe_data,"Female")
  male_bin <- binary_sib.summary(ma_data,"Male")
  
  # BIND #
  rbind(overall,Female,Male) %>%
    select(-c(statistic,df,p.value)) %>%
    rbind(overall_bin,
          female_bin,
          male_bin) %>%
    reshape(idvar = "term",
            timevar = "dataset",
            direction = "wide")
  }

binary_sib.summary <- function(df, string){
  # -- sibbully6 -- #
  sib_bully6_wpct <- with(df, 
                      by(df, 
                         .imp, 
                         function(x) c(
                           weights::wpct(x$sib_bully6,
                                         weight = 
                                           x$CW1_COMBWT,
                                         na.rm=TRUE)
                         )))
  sib_bully6_wpct <- round(100*(
    Reduce("+",sib_bully6_wpct)/length(sib_bully6_wpct)),
    digits = 1)
  sib_bully6_wpct <- as.data.frame(sib_bully6_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = sib_bully6_wpct)
  sib_bully6_count <- with(df,
                       by(df, 
                          .imp, 
                          function(x) c(
                            filter(x, sib_bully6 == "Yes") %>%
                              summarise(n=n())
                          )))
  sib_bully6_count <- round(
    Reduce("+",sib_bully6_count)/length(sib_bully6_count),
    digits = 0)
  sib_bully6_count <- as.data.frame(
    sib_bully6_count,row.names = 'Yes'
    ) %>%
    mutate(id = "Yes") %>%
    rename(n = sib_bully6_count)
  # -- Step -- #
  step_7_wpct <- with(df, 
                      by(df, 
                         .imp, 
                         function(x) c(
                           weights::wpct(x$step_7,
                                         weight = 
                                           x$CW1_COMBWT,
                                         na.rm=TRUE)
                         )))
  step_7_wpct <- round(100*(
    Reduce("+",step_7_wpct)/length(step_7_wpct)),
    digits = 1)
  step_7_wpct <- as.data.frame(step_7_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = step_7_wpct)
  step_7_count <- with(df,
                       by(df, 
                          .imp, 
                          function(x) c(
                            filter(x, step_7 == "Yes") %>%
                              summarise(n=n())
                          )))
  step_7_count <- round(Reduce("+",step_7_count)/length(step_7_count),
                        digits = 0)
  step_7_count <- as.data.frame(step_7_count,row.names = 'Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = step_7_count)
  # -- Single -- #
  single_7_wpct <- with(
    df,by(df,
          .imp, 
          function(x) c(
            weights::wpct(x$single_7,
                          weight = x$CW1_COMBWT,
                          na.rm=TRUE)
            )))
  single_7_wpct <- round(100*(
    Reduce("+",single_7_wpct)/length(single_7_wpct)),
    digits = 1)
  single_7_wpct <- as.data.frame(single_7_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = single_7_wpct)
  single_7_count <- with(df,
                         by(df, 
                            .imp, 
                            function(x) c(
                              filter(x, single_7 == "Yes") %>%
                                summarise(n=n())
                              )))
  single_7_count <- round(
    Reduce("+",single_7_count)/length(single_7_count),
    digits = 0)
  single_7_count <- as.data.frame(single_7_count,
                                  row.names ='Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = single_7_count)
  # -- Income -- #
  income_wpct <- with(
    df,by(df,
          .imp, 
          function(x) c(
            weights::wpct(x$W6_lowincome,
                          weight = x$CW1_COMBWT,
                          na.rm=TRUE)
          )))
  income_wpct <- round(100*(
    Reduce("+",income_wpct)/length(income_wpct)),
    digits = 1)
  income_wpct <- as.data.frame(income_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = income_wpct)
  income_count <- with(df,
                         by(df, 
                            .imp, 
                            function(x) c(
                              filter(x, W6_lowincome == "Yes") %>%
                                summarise(n=n())
                            )))
  income_count <- round(
    Reduce("+",income_count)/length(income_count),
    digits = 0)
  income_count <- as.data.frame(income_count,
                                row.names ='Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = income_count)
  # -- Bind -- #
  bully <- sib_bully6_wpct %>%
    full_join(sib_bully6_count, by = c("id")) %>%
    mutate_sib.summary_table.bin("bully", string)
  step <- step_7_wpct %>%
    full_join(step_7_count, by = c("id")) %>%
    mutate_sib.summary_table.bin("stepparent", string)

  single <- single_7_wpct %>%
    full_join(single_7_count, by = c("id")) %>%
    mutate_sib.summary_table.bin("singleparent", string)
  
  income <- income_wpct %>%
    full_join(income_count, by = c("id")) %>%
    mutate_sib.summary_table.bin("income",string)
  
  rbind(bully,step,single,income)

}

continuous_sib.summary <- function(data,string){
  covid_K6 <- summary(mice::pool(
    with(data,
         lm(covid_K6 ~ 1, 
            weights = CW1_COMBWT))
    )) %>%
    mutate_sib.summary_table.cont("covid_K6",string)
  
  covid_SWEMWBS <- summary(mice::pool(
    with(data,
         lm(covid_SWEMWBS ~ 1, 
            weights = CW1_COMBWT))
    )) %>%
    mutate_sib.summary_table.cont("covid_SWEMWBS",string)
  
  existing_K6 <- summary(mice::pool(
    with(data,
         lm(existing_K6 ~ 1, 
            weights = CW1_COMBWT))
    )) %>%
    mutate_sib.summary_table.cont("existing_K6",string)

  
  existing_SWEMWBS <- summary(mice::pool(
    with(data,
         lm(existing_SWEMWBS ~ 1, 
            weights = CW1_COMBWT))
    )) %>%
    mutate_sib.summary_table.cont("existing_SWEMWBS",string)
  
  rbind(covid_K6,
        existing_K6,
        covid_SWEMWBS,
        existing_SWEMWBS)
}

mutate_sib.summary_table.cont <- function(data,string1,string2){
  data %>%
  mutate(term = string1,
         dataset = string2,
         estimate = round(estimate, digits = 2),
         std.error = round(std.error, digits = 2),
         summary = paste(estimate,"(",std.error,")"))
}

mutate_sib.summary_table.bin <- function(data,string,string2){
  data %>%
    filter(id == "Yes") %>%
    select(-id) %>%
    mutate(term = string) %>%
    rename(estimate = n,
           std.error = wpct) %>%
    mutate(summary = paste(estimate,"(",std.error,")"),
           dataset = string2)
}

modelsummary_sib<- function(data) {
  data <- mice::as.mids(data)
  
  k61 <- with(data,
              coeftest(
              lm(covid_K6 ~ 
                   existing_K6+cov_symp+ethnicity+region+
                   residence*male, 
                 weights = CW1_COMBWT)), vcov. = sandwich)
  k61 <- mice::pool(k61)
  
  k62 <- with(data,
              coeftest(
                lm(covid_K6 ~ 
                   existing_K6+cov_symp+ethnicity+region+
                   residence*male+num_sibling+same_sib_gender+
                   is_oldest_sib+sib_bully,
                 weights = CW1_COMBWT)), vcov. = sandwich)
  k62 <- mice::pool(k62)
  k63 <- with(data,
              coeftest(
              lm(covid_K6 ~
                   existing_K6+cov_symp+ethnicity+region+
                   residence*male+
                   num_sibling+same_sib_gender+
                   is_oldest_sib+sib_bully+
                   moved+economic_activity+
                   fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                   step_7+single_7+
                   conflict, 
                 weights = CW1_COMBWT)), vcov. = sandwich)
  k63 <- mice::pool(k63)
  
  wb1 <- with(data, 
              coeftest(
                lm(covid_SWEMWBS ~ 
                    existing_SWEMWBS+cov_symp+ethnicity+region+
                    residence*male, 
                  weights = CW1_COMBWT)), vcov. = sandwich)
  wb1 <- mice::pool(wb1)
  
  wb2 <- with(data,
               coeftest(lm(covid_SWEMWBS ~
                    existing_SWEMWBS+cov_symp+ethnicity+region+
                    residence*male+num_sibling+same_sib_gender+
                    is_oldest_sib+sib_bully, 
                  weights = CW1_COMBWT)), vcov. = sandwich)
  wb2 <- mice::pool(wb2)
  
  wb3 <- with(data, 
              coeftest(lm(covid_SWEMWBS ~ 
                   existing_SWEMWBS+
                   cov_symp+ethnicity+region+
                   residence*male+
                   num_sibling+same_sib_gender+
                   is_oldest_sib+sib_bully+
                   moved+economic_activity+
                   fam_ses_par_ed+W6_lowincome+
                   fam_ses_overcrowded+
                   step_7+single_7+
                   conflict, 
                 weights = CW1_COMBWT)), vcov. = sandwich)
  wb3 <- mice::pool(wb3)
  
  models <- list("Model 4" = wb1,
                 "Model 5" = wb2,
                 "Model 6" = wb3,
                 "Model 4" = k61,
                 "Model 5" = k62,
                 "Model 6" = k63)
  
  cm <- c("(Intercept)" = 'Constant', #1
          "maleMale" = "Male", #2
          "residenceLeft parental home" = "Left Parental Home", #3
          'residenceLiving with parents & siblings' = 
            'Living with Parents & Siblings', #4
          "movedYes" = "Living Arrangements Changed since Mar ‘20", #5
          "economic_activityIn Training/Edu" = 
            "In Education (Not University)",#6
          "economic_activityEmployed" = "Employed",#7
          "economic_activityFurloughed" = "Furloughed",#8
          "economic_activityUnemployed" = "Unemployed",#9
          "economic_activityOther Inactive" = 
            "Economically Inactive",#10
          "fam_ses_par_edYes" = "High Parental Education",#11
          "W6_lowincomeYes" = "Low Family Income",#12
          "fam_ses_overcrowdedYes" = 
            "Overcrowded COVID-19 HH", #13
          "step_7Yes" = "Has Stepparent",#14
          "single_7Yes" = "Has Single Parent",#15
          "residenceLeft parental home:maleMale" = 
            'Male, Left the Parental Home', #16
          'residenceLiving with parents & siblings:maleMale' = 
            'Male, Living with Parent & Siblings', #17
          "conflictYes" = "Increased COVID-19 HH Conflict", #18
          "num_sibling" = "Number of Siblings", #19
          "same_sib_genderYes" = 
            "Has Same Gender Sibling", #20
          "is_oldest_sibYes" = 
            "Is Oldest Sibling", #21
          "sib_bullyYes" = 
            "Frequent Childhood Sibling Bullying"#22
          ) 
  
  gof <- tibble::tribble(
    ~raw,~clean,~fmt,
    "nobs","Observations",0,
    'nimp',"Imputations",0,
    "adj.r.squared","R\U00B2",2)
  
  table <- modelsummary(models,
                        output = "gt",
                        fmt = 2,
                        coef_map = cm,
                        gof_map = NA,
                        estimate = 
                          "{estimate}{stars} ({std.error})",
                        statistic = NULL,
                        stars = c('+' = 0.1, 
                                  '*' = .05, 
                                  '**' = .01))
  
  table <- table %>%
    tab_row_group(
      id = "constant",
      label = "",
      rows = c(1)
    ) %>%
    tab_row_group(
      id = "conflict",
      label = "",
      rows = c(18)
    ) %>%
    tab_row_group(
      id = "move",
      label = "",
      rows = c(5)
    ) %>%
    tab_row_group(
      id = "ecinactive",
      label = md("*(Ref Young Adult in University)*"),
      rows = c(6:10)
    ) %>%
    tab_row_group(
      id = "fam",
      label = md("*Young Adult's Family Factors*"),
      rows = c(11:15)
    ) %>%
    tab_row_group(
      id = "sibfactors",
      label = "Sibling Factors",
      rows = c(19:22)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Male, Living with Parents, No Siblings)*"),
      rows = c(16,17)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Female, Living with Parents, No Siblings)*"),
      rows = c(3,4)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Female)*"),
      rows = c(2)
    ) %>%
    tab_options(table.width = pct(90)) %>%
    cols_align(align="center",
               columns = c(2,3,4,5,6,7)) %>%
    tab_spanner(
      label = md("Coef. > 0 ~ better mental health"),
      columns = c(2,3,4)
    ) %>%
    tab_spanner(
      label = md("SWEMWBS"),
      columns = c(2,3,4),
      level = 2
    ) %>%
    tab_spanner(
      label = md("Coef. < 0 ~ better mental health"),
      columns = c(5,6,7)
    ) %>%
    tab_spanner(
      label = md("K6"),
      columns = c(5,6,7),
      level = 2
    ) %>%
    tab_source_note(source_note = md("*Notes*: OLS regression estimates are weighted. Cluster robust standard errors are in parentheses. Significance: '+' p < 0.1, '*' p < 0.05, '**' p < 0.01. All models control for country of residence (England, Scotland, Wales, Northern Ireland), ethnicity (non-white), experience of COVID-19 symptoms, and mental health (SWEMWBS or K6) at age 17. Model 5 includes Sibling Factors. Model 6 includes all available control factors, including young adult economic activity and young adult childhood family factors. Only significant control factors are summarised in this table to highlight estimates of sibling factors and our main explanatory factors. The unweighted sample is N = 2,332, and missing values are filled with multiple imputation of chained equations."))
  
  table %>% gtsave(filename = '03_Table_mi.sib.rtf',
                   path = 'Figures_Tables/')

  return(table)
}

summary_sib.imp_stratified<- function(data) {
  data <- mice::as.mids(data)
  fe_data <- data %>% filter(male == "Female")
  ma_data <- data %>% filter(male == "Male")
  
  k61fem <- with(fe_data,
              lm(covid_K6 ~ 
                   existing_K6+cov_symp+ethnicity+region+
                   residence, 
                 weights = CW1_COMBWT))
  k61fem <- mice::pool(k61fem)
  
  k62fem <- with(fe_data,
              lm(covid_K6  ~ 
                   existing_K6+cov_symp+ethnicity+region+
                   residence+moved+
                   economic_activity+
                   fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                   step_7+single_7, 
                 weights = CW1_COMBWT))
  k62fem <- mice::pool(k62fem)
  k63fem <- with(fe_data,
              lm(covid_K6 ~
                   existing_K6+cov_symp+ethnicity+region+
                   residence+ 
                   moved+economic_activity+
                   fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                   step_7+single_7+
                   conflict, 
                 weights = CW1_COMBWT))
  k63fem <- mice::pool(k63fem)
  
  k61masc <- with(ma_data,
                 lm(covid_K6 ~ 
                      existing_K6+cov_symp+ethnicity+region+
                      residence, 
                    weights = CW1_COMBWT))
  k61masc <- mice::pool(k61masc)
  
  k62masc <- with(ma_data,
                 lm(covid_K6  ~ 
                      existing_K6+cov_symp+ethnicity+region+
                      residence+moved+
                      economic_activity+
                      fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                      step_7+single_7, 
                    weights = CW1_COMBWT))
  k62masc <- mice::pool(k62masc)
  k63masc <- with(ma_data,
                 lm(covid_K6 ~
                      existing_K6+cov_symp+ethnicity+region+
                      residence+ 
                      moved+economic_activity+
                      fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                      step_7+single_7+
                      conflict, 
                    weights = CW1_COMBWT))
  k63masc <- mice::pool(k63masc)
  
  wb1fem <- with(fe_data, 
              lm(covid_SWEMWBS ~ 
                   existing_SWEMWBS+cov_symp+ethnicity+region+
                   residence, 
                 weights = CW1_COMBWT))
  wb1fem <- mice::pool(wb1fem)
  
  wb2fem <- with(fe_data,
              lm(covid_SWEMWBS ~
                   existing_SWEMWBS+cov_symp+ethnicity+region+
                   residence+
                   moved+
                   economic_activity+
                   fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                   step_7+single_7, 
                 weights = CW1_COMBWT))
  wb2fem<- mice::pool(wb2fem)
  
  wb3fem <- with(fe_data, 
              lm(covid_SWEMWBS ~ 
                   existing_SWEMWBS+cov_symp+ethnicity+region+
                   residence+ 
                   moved+
                   economic_activity+
                   fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                   step_7+single_7 +
                   conflict, 
                 weights = CW1_COMBWT))
  wb3fem <- mice::pool(wb3fem)
  
  wb1masc <- with(ma_data, 
                 lm(covid_SWEMWBS ~ 
                      existing_SWEMWBS+cov_symp+ethnicity+region+
                      residence, 
                    weights = CW1_COMBWT))
  wb1masc <- mice::pool(wb1masc)
  
  wb2masc <- with(ma_data,
                 lm(covid_SWEMWBS ~
                      existing_SWEMWBS+cov_symp+ethnicity+region+
                      residence+
                      moved+
                      economic_activity+
                      fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                      step_7+single_7, 
                    weights = CW1_COMBWT))
  wb2masc<- mice::pool(wb2masc)
  
  wb3masc <- with(ma_data, 
                 lm(covid_SWEMWBS ~ 
                      existing_SWEMWBS+cov_symp+ethnicity+region+
                      residence+ 
                      moved+
                      economic_activity+
                      fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                      step_7+single_7 +
                      conflict, 
                    weights = CW1_COMBWT))
  wb3masc <- mice::pool(wb3masc)
  
  models <- list("1" = k61fem,
                 "2" = k62fem,
                 "3" = k63fem,
                 "1" = k61masc,
                 "2" = k62masc,
                 "3" = k63masc,
                 "1" = wb1fem,
                 "2" = wb2fem,
                 "3" = wb3fem,
                 "1" = wb1masc,
                 "2" = wb2masc,
                 "3" = wb3masc)
  
  cm <- c("(Intercept)" = 'Constant', #1
          "existing_K6"  = "Lagged Dependent Variable (Age 17)", #2
          "residenceLeft parental home" = "Left Parental Home", #3
          'residenceLiving with parents & siblings' = 
            'Living with Parents & Siblings', #4
          "movedYes" = "Living Arrangements Changed", #5
          "economic_activityIn Training/Edu" = 
            "In Education (Not University)", #6
          "economic_activityEmployed" = "Employed", #7
          "economic_activityFurloughed" = "Furloughed", #8
          "economic_activityUnemployed" = "Unemployed", #9
          "economic_activityOther Inactive" = 
            "Economically Inactive", #10
          "fam_ses_par_edYes" = "High Parental Education", #11
          "W6_lowincomeYes" = "Low Family Income", #12
          "fam_ses_overcrowdedYes" = "Overcrowded COVID-19 HH", #13
          "step_7Yes" = "Has Stepparent", #14
          "single_7Yes" = "Has Single Parent", #15
          "conflictYes" = "Increased Conflict", #16
          "existing_SWEMWBS" = "Lagged Dependent Variable (Age 17)")#2
  
  gof <- tibble::tribble(
    ~raw,~clean,~fmt,
    "nobs","Observations",0,
    'nimp',"Imputations",0,
    "adj.r.squared","R\U00B2",2)
  
  table <- modelsummary(models,
                        output = "gt",
                        fmt = 2,
                        coef_map = cm,
                        gof_map = "none",
                        estimate = "{estimate}{stars} ({std.error})",
                        statistic = NULL,
                        stars = c('+' = 0.1, '*' = .05, '**' = .01))
  
  table <- table %>%
    tab_row_group(
      id = "constant",
      label = "",
      rows = c(1)
    ) %>%
    tab_row_group(
      id = "ldv",
      label = "",
      rows = c(2)
    ) %>%
    tab_row_group(
      id = "conflict",
      label = md("*Covid-19 HH Conflict*"),
      rows = c(16)
    ) %>%
    tab_row_group(
      id = "fam",
      label = md("*Young Adult's Family Factors*"),
      rows = c(11,12,13,14,15)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Young Adult in University)*"),
      rows = c(6,7,8,9,10)
    ) %>%
    tab_row_group(
      id = "move",
      label = md("*(Ref: COVID-19 Living Arrangements Unchanged)*"),
      rows = c(5)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Living with Parents, No Siblings)*"),
      rows = c(3,4)
    ) %>%
    tab_options(table.width = pct(90)) %>%
    cols_align(align="center",
               columns = c(2,3,4,5,6,7)) %>%
    tab_spanner(
      label = "Women, K6",
      columns = c(2,3,4)
    ) %>%
    tab_spanner(
      label = "Men, K6",
      columns = c(5,6,7)
    ) %>%
    tab_spanner(
      label = "Women, SWEMWBS",
      columns = c(8,9,10)
    ) %>%
    tab_spanner(
      label = "Men, SWEMWBS",
      columns = c(11,12,13)
    ) %>%
    tab_source_note(source_note = md("*Notes*: Standard errors in parentheses. Significance: '+' p < 0.1, '*' p < 0.05, '**' p < 0.01. All models control for country of residence (England, Scotland, Wales, Northern Ireland), ethnicity (non-white), and experience of COVID-19 symptoms. All estimates are weighted with provided weights that account for both design and attrition. Missing values filled with multiple imputation."))
  
  table %>% gtsave(filename = 'Table2_stratified_mi.rtf',
                   path = 'Figures_Tables/')
  
  return(table)
}