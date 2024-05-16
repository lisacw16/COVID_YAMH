fixedmodels_0.3 <- function(data) {
  data1 <- mice::as.mids(data)
  
  K61 <- with(data1,
              plm::plm(K6 ~ cov_symp+country+residence*Male,
                       data = data.frame(mget(ls())),
                       weights = weight3,
                       index = c("MCSID", "wave"),
                       model = "within"))
  K61 <- mice::pool(K61)
  
  WB1 <- with(data1,
              plm::plm(SWEMWBS ~ cov_symp+country+residence*Male,
                       data = data.frame(mget(ls())),
                       weights = weight3,
                       index = c("MCSID", "wave"),
                       model = "within"))
  WB1 <- mice::pool(WB1)
  
  K62 <- with(data1,
              plm::plm(K6 ~ cov_symp+country+residence*Male+
                     #    as.factor(wave)+
                         economic_activity+
                         fam_ses_overcrowded,
                       weights = weight3,
                       data = data.frame(mget(ls())),
                       index = c("MCSID", "wave"),
                       model = "within"))
  K62 <- mice::pool(K62)
  
  WB2 <- with(data1,
              plm::plm(SWEMWBS ~ cov_symp+country+residence*Male+
                      #   as.factor(wave)+
                         economic_activity+
                         fam_ses_overcrowded,
                       data = data.frame(mget(ls())),
                       weights = weight3,
                       index = c("MCSID", "wave"),
                       model = "within"))
  WB2 <- mice::pool(WB2)
  
  list("Model 1" = WB1,
       "Model 2" = WB2,
       "Model 1" = K61,
       "Model 2" = K62)
}

modelsummary_femodels <- function(models, file.name) {
  cm <- c(
    "residenceLeft parental home" = 
      "(Female) Left Parental Home", #1
    "residenceLiving with parents & siblings" =
      "(Female) Living with Parents and Siblings", #2
    "residenceLeft parental home:MaleMale" = 
      "(Male) Left Parental Home", #3
    "residenceLiving with parents & siblings:MaleMale" =
      "(Male) Living with Parents and Siblings", #4 
    "as.factor(wave)1" = "COVID-19 Wave 1 May 2020",
    "as.factor(wave)2" = "COVID-19 Wave 2 Sep-Oct 2020",
    "as.factor(wave)3" = "COVID-19 Wave 3 Feb-Mar 2021"
    )
  
  gof <- tibble::tribble(
    ~raw,            ~clean,    ~fmt,
    "nobs",          "N",       0)
  
  table <- modelsummary(models,
                        output = "gt",
                        fmt = 2,
                        coef_map = cm,
                        estimate = 
                          "{estimate}{stars} ({std.error})",
                        statistic = NULL,
                        gof_map = NA,
                        stars = c('+' = 0.1, 
                                  '*' = .05, 
                                  '**' = .01))
  
  table <- table %>%
    cols_align(align="right",
               columns = c(1)) %>%
    # tab_row_group(
    #   label = md(
    #     "*(Ref: Pre-COVID-19 Age 17)*"),
    #   rows = c(5:7)
    # ) %>%
    tab_row_group(
      label = md(
        "*(Ref: Male, Living with Parents, No Siblings)*"),
      rows = c(3,4)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Female, Living with Parents, No Siblings)*"),
      rows = c(1,2)
    ) %>%
    tab_spanner(
      label = md("Coef. > 0 ~ better mental health"),
      columns = c(2:3)
    ) %>%
    tab_spanner(
      label = md("SWEMWBS"),
      columns = c(2:3),
      level = 2
    ) %>%
    tab_spanner(
      label = md("Coef. < 0 ~ better mental health"),
      columns = c(4:5)
    ) %>%
    tab_spanner(
      label = md("K6"),
      columns = c(4:5),
      level = 2
    ) %>%
    tab_source_note(source_note = md("*Notes*: Fixed effect estimation on a balanced panel of N = 1,462, observed 4 times each. All estimates are weighted, and standard errors are in parentheses. Models control for the experience of COVID-19 symptoms and country of residence (England, Scotland, Wales, or Northern Ireland). Model 2 further controls for young adult economic activity and whether living in an overcrowded household during COVID-19."))
  
  table %>% gtsave(filename = paste0(file.name, '.rtf'),
                   path = 'Figures_Tables/')
  
  return(table)
}