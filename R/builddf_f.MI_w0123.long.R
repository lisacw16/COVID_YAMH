# the 'moved' variable are not included in these datasets as it is not a time-variant variable.
mi_0.3_long <- function(wide_data){
  selected <- wide_data %>%
    filter(!is.na(CW0_residence) & CW0_residence != 
             "Living with siblings, no parents" &
             !is.na(CW1_residence) & CW1_residence != 
             "Living with siblings, no parents" &
             !is.na(CW2_residence) & CW2_residence != 
             "Living with siblings, no parents" &
             !is.na(CW3_residence) & CW3_residence  != 
             "Living with siblings, no parents") %>%
    mutate(across(matches('^.*_(residence|Country)$'),
                  ~droplevels(.x))) %>%
    mutate(CW3_COMBWT = as.numeric(CW3_COMBWT)) %>%
    filter(!is.na(CW3_COMBWT)) %>%
    filter(!is.na(CW2_COMBWT)) %>%
    filter(!is.na(CW1_COMBWT))
  
  wave0 <- selected %>%
    select(MCSID,
           Male,
           CW0_residence, 
           existing_K6, 
           existing_SWEMWBS,
           CW3_COMBWT,
           CW0_Economic.Activity,
           CW0_Country) %>%
    mutate(cov_symp = as.factor("No"),
           wave = 0,
           fam_ses_overcrowded = as.factor("No"),
           lockdown = as.factor("No")) %>%
    rename(residence = CW0_residence,
           K6 = existing_K6,
           SWEMWBS = existing_SWEMWBS,
           economic_activity = CW0_Economic.Activity,
           weight3 = CW3_COMBWT,
           country = CW0_Country)
  
  wave1 <- selected %>%
    select(MCSID,
           Male,
           CW1_residence,
           CW1_Covid_K6,
           CW1_Covid_SWEMWBS,
           CW1_cov_symp,
           CW1_Overcrowded,
           CW1_Country,
           CW1_Economic.Activity,
           CW3_COMBWT)%>%
    mutate(wave = 1,
           lockdown = as.factor("Yes")) %>%
    rename(K6 = CW1_Covid_K6,
           SWEMWBS = CW1_Covid_SWEMWBS,
           residence = CW1_residence,
           cov_symp = CW1_cov_symp,
           weight3 = CW3_COMBWT,
           fam_ses_overcrowded = CW1_Overcrowded,
           economic_activity = CW1_Economic.Activity,
           country = CW1_Country)
  
  wave2 <- selected %>%
    select(MCSID,
           Male,
           CW2_residence,
           CW2_Covid_K6,
           CW2_Covid_SWEMWBS,
           CW2_cov_symp,
           CW2_Overcrowded,
           CW2_Country,
           CW2_Economic.Activity,
           CW3_COMBWT)%>%
    mutate(wave = 2,
           lockdown = as.factor("No")) %>%
    rename(residence = CW2_residence,
           K6 = CW2_Covid_K6,
           SWEMWBS = CW2_Covid_SWEMWBS,
           cov_symp = CW2_cov_symp,
           weight3 = CW3_COMBWT,
           fam_ses_overcrowded = CW2_Overcrowded,
           economic_activity = CW2_Economic.Activity,
           country = CW2_Country)
  
  wave3 <- selected %>%
    select(MCSID,
           Male,
           CW3_residence,
           CW3_Covid_K6,
           CW3_Covid_SWEMWBS,
           CW3_cov_symp,
           CW3_Overcrowded,
           CW3_Country,
           CW3_Economic.Activity,
           CW3_COMBWT)%>%
    mutate(wave = 3,
           lockdown = as.factor("Yes")) %>%
    rename(residence = CW3_residence,
           K6 = CW3_Covid_K6,
           SWEMWBS = CW3_Covid_SWEMWBS,
           cov_symp = CW3_cov_symp,
           weight3 = CW3_COMBWT,
           fam_ses_overcrowded = CW3_Overcrowded,
           economic_activity = CW3_Economic.Activity,
           country = CW3_Country)
  
  data <- bind_rows(wave0, wave1, wave2, wave3) %>%
    mutate(lockdown = relevel(lockdown, ref= "No"),
           economic_activity = as.factor(economic_activity))
  # begin imputation process with inspecting the data.
  # the following code:
  # 1. prints each column and a sample of the data
  print(head(data))
  print(str(data))
  # 2. identifies and prints the percent missing in the data
  pmissing <- unlist(
    lapply(data, 
           function(x) sum(is.na(x))))/nrow(data)
  print(sort(pmissing[pmissing > 0], decreasing = TRUE))
  
  # begin imputation model
  imp <- mice::mice(data, maxit=0)
  meth <-  imp$method
  predM <- imp$predictorMatrix
  predM[, c("weight3")] <- 0
  predM[, c("MCSID")] <- 0
  meth[c("K6",
         "SWEMWBS")] <- "norm"
  meth[c("fam_ses_overcrowded",
         "cov_symp",
         "Male")] <- "logreg"
  meth[c("residence",
         "economic_activity",
         "country")] <- "polyreg"
  print(imp$meth)
  imp2 <- mice::mice(data, m = 30, maxit = 20,
                     method = meth,
                     predictorMatrix = predM,
                     print =  FALSE,
                     seed = 5000)
  
  mice::complete(imp2, action="long", include = TRUE)
  
}

