Figure1B_function <- function(data1, data2, data3) {
  Figure1_DF <- figureB_df.function(data1, data2, data3)
  K6 <- Figure1_function.K6(Figure1_DF, k6.se, k6.n)
  SWEMWBS <- Figure1_function.WB(Figure1_DF, wb.se, wb.n)
  
  plot <- SWEMWBS + K6 +
    plot_layout(nrow = 2,
                guides = "collect",
                axes = "collect") &
    theme(panel.background = element_blank(),
          strip.background = element_blank(),
          legend.key=element_blank(),
          legend.title=element_blank(),
          legend.text = element_text(size = 25),
          text = element_text(family = "Times",
                              size = 25),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.y= element_text(colour = "black",
                                    size = 25),
          axis.line.x.bottom=element_line(color="black"),
          axis.line.y.left=element_line(color="black"))
  
  ggsave("Figures_Tables/0X_Figure.pdf", 
         plot = plot,
         height = 18, width = 16,
         dpi = 600)
}

figureB_df.function <- function(data1, data2, data3){
  data <- combine.imputed.xwave.data(data1, data2, data3)
  
  cov3 <- summarise_cv3(data)%>%
    mutate(t=3)
  
  cov2 <- summarise_cv2(data) %>%
    mutate(t=2)
  
  cov <-data %>% 
    filter(!is.na(CW1_COMBWT)) %>%
    summarise_cv()%>%
    mutate(t=1)
  print(table(cov$residence, useNA = 'ifany'))
  aged_17 <- data %>%
    filter(!is.na(CW1_COMBWT)) %>%
    summarise_17() %>%
    mutate(t=0)
  
  rbind(cov3, cov2, cov, aged_17)
}

summarise_cv2 <- function(data){
  print("cv2")
  data1 <- data %>% filter(!is.na(CW2_COMBWT))
  
  k6 <- data1 %>%
    mean.n.se(CW2_Covid_K6,CW2_COMBWT,CW2_residence) %>%
    rename(k6.mean = mean, 
           k6.se = se, 
           k6.n = n,
           residence = CW2_residence) %>%
    mutate(t = "September – October 2020")
  
  wb <- data1 %>%
    mean.n.se(CW2_Covid_SWEMWBS,CW2_COMBWT,CW2_residence) %>%
    rename(wb.mean = mean, 
           wb.se = se, 
           wb.n = n,
           residence = CW2_residence) %>%
    mutate(t = "September – October 2020")
  
  k6 %>%
    full_join(wb, by = c("residence", "male", "t")) 
}
summarise_cv3 <- function(data){
  data1 <- data %>% filter(!is.na(CW3_COMBWT))
  
  k6 <- data1 %>%
    mean.n.se(CW3_Covid_K6,CW3_COMBWT,CW3_residence) %>%
    rename(k6.mean = mean, 
           k6.se = se, 
           k6.n = n,
           residence = CW3_residence) %>%
    mutate(t = "February - March 2021")
  
  wb <- data1 %>%
    mean.n.se(CW3_Covid_SWEMWBS,CW3_COMBWT,CW3_residence) %>%
    rename(wb.mean = mean, 
           wb.se = se, 
           wb.n = n,
           residence = CW3_residence) %>%
    mutate(t = "February - March 2021")
  
  k6 %>%
    full_join(wb, by = c("residence", "male", "t")) 
}

combine.imputed.xwave.data <- function(data1, data2, data3) {
  data1 <- data1 %>%
    filter(.imp > 0) %>%
    group_by(MCSID) %>%
    summarise(residence = first(residence[!is.na(residence)]),
              male = first(male[!is.na(male)]),
              covid_K6 = mean(covid_K6),
              covid_SWEMWBS = mean (covid_SWEMWBS),
              existing_K6 = mean(existing_K6),
              existing_SWEMWBS = mean(existing_SWEMWBS),
              CW1_COMBWT = mean(CW1_COMBWT)) %>%
    ungroup()
  
  data2 <- data2 %>%
    filter(.imp > 0) %>%
    group_by(MCSID) %>%
    summarise(CW2_residence = first(CW2_residence[!is.na(CW2_residence)]),
              male = first(Male[!is.na(Male)]),
              CW2_Covid_K6 = mean(CW2_Covid_K6),
              CW2_Covid_SWEMWBS = mean (CW2_Covid_SWEMWBS),
              CW2_existing_K6 = mean(existing_K6),
              CW2_existing_SWEMWBS = mean(existing_SWEMWBS),
              CW2_COMBWT = mean(CW2_COMBWT)) %>%
    ungroup() 
  
  data3 <- data3 %>%
    filter(.imp > 0) %>%
    group_by(MCSID) %>%
    summarise(CW3_residence = first(CW3_residence[!is.na(CW3_residence)]),
              male = first(Male[!is.na(Male)]),
              CW3_Covid_K6 = mean(CW3_Covid_K6),
              CW3_Covid_SWEMWBS = mean (CW3_Covid_SWEMWBS),
              CW3_existing_K6 = mean(existing_K6),
              CW3_existing_SWEMWBS = mean(existing_SWEMWBS),
              CW3_COMBWT = mean(CW3_COMBWT)) %>%
    ungroup() 
  
  
  data <- data1 %>%
    full_join(data2, by=c("MCSID", "male")) %>%
    full_join(data3, by=c("MCSID", "male")) %>%
    mutate(existing_K6 = case_when(
      !is.na(existing_K6) & 
        is.na(CW2_existing_K6) &
        is.na(CW3_existing_K6) ~ existing_K6,
      is.na(existing_K6) & 
        !is.na(CW2_existing_K6) &
        is.na(CW3_existing_K6) ~ CW2_existing_K6,
      is.na(existing_K6) & 
        is.na(CW2_existing_K6) &
        !is.na(CW3_existing_K6) ~ CW3_existing_K6,
      !is.na(existing_K6) & 
        !is.na(CW2_existing_K6) &
        is.na(CW3_existing_K6) ~ (existing_K6+CW2_existing_K6)/2,
      !is.na(existing_K6) & 
        is.na(CW2_existing_K6) &
        !is.na(CW3_existing_K6) ~ (existing_K6+CW3_existing_K6)/2,
      is.na(existing_K6) & 
        !is.na(CW2_existing_K6) &
        !is.na(CW3_existing_K6) ~ (CW3_existing_K6+CW2_existing_K6)/2,
      !is.na(existing_K6) & 
        !is.na(CW2_existing_K6) &
        !is.na(CW3_existing_K6) ~ (existing_K6+
                                     CW2_existing_K6+
                                     CW3_existing_K6)/3
    ),
    existing_SWEMWBS = case_when(
      !is.na(existing_SWEMWBS) & 
        is.na(CW2_existing_SWEMWBS) &
        is.na(CW3_existing_SWEMWBS) ~ existing_SWEMWBS,
      is.na(existing_SWEMWBS) & 
        !is.na(CW2_existing_SWEMWBS) &
        is.na(CW3_existing_SWEMWBS) ~ CW2_existing_SWEMWBS,
      is.na(existing_SWEMWBS) & 
        is.na(CW2_existing_SWEMWBS) &
        !is.na(CW3_existing_SWEMWBS) ~ CW3_existing_SWEMWBS,
      !is.na(existing_SWEMWBS) & 
        !is.na(CW2_existing_SWEMWBS) &
        is.na(CW3_existing_SWEMWBS) ~ 
        (existing_SWEMWBS+CW2_existing_SWEMWBS)/2,
      !is.na(existing_SWEMWBS) & 
        is.na(CW2_existing_SWEMWBS) &
        !is.na(CW3_existing_SWEMWBS) ~ 
        (existing_SWEMWBS+CW3_existing_SWEMWBS)/2,
      is.na(existing_SWEMWBS) & 
        !is.na(CW2_existing_SWEMWBS) &
        !is.na(CW3_existing_SWEMWBS) ~ 
        (CW3_existing_SWEMWBS+CW2_existing_SWEMWBS)/2,
      !is.na(existing_SWEMWBS) & 
        !is.na(CW2_existing_SWEMWBS) &
        !is.na(CW3_existing_SWEMWBS) ~ (existing_SWEMWBS+
                                     CW2_existing_SWEMWBS+
                                     CW3_existing_SWEMWBS)/3
    )) %>%
    select(-c(CW2_existing_SWEMWBS, CW3_existing_SWEMWBS, CW2_existing_K6, CW3_existing_K6))
  
}
