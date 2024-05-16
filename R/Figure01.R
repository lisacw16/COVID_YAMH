Figure1_function <- function(data) {
  data <- data %>% filter(.imp > 0) %>%
    group_by(MCSID) %>%
    summarise(residence = first(residence[!is.na(residence)]),
              male = first(male[!is.na(male)]),
              covid_K6 = mean(covid_K6),
              covid_SWEMWBS = mean (covid_SWEMWBS),
              existing_K6 = mean(existing_K6),
              existing_SWEMWBS = mean(existing_SWEMWBS),
              CW1_COMBWT = mean(CW1_COMBWT)) %>%
    ungroup()
  
  Figure1_DF <- figure_df.function(data)
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
  
  ggsave("Figures_Tables/01_Figure.pdf", 
         plot = plot,
         height = 18, width = 16,
         dpi = 600)
}

Figure1_function.K6 <- function(data, var, n) {
  pd <- position_dodge(0.05) # move them .05 to the left and right
  data %>%
    ggplot(aes(x=t, 
               y=k6.mean, 
               group = residence, 
               linetype = residence)) +
    facet_wrap(~male, nrow = 1) +
    labs(y = "Mean Psychological Distress (K6)", 
         x = "", 
         linetype = "Young Adult COVID-19 Living Arrangements") +
    geom_errorbar(
       aes(ymin=k6.mean-(1.96*sqrt(({{var}}/{{n}}))), 
           ymax=k6.mean+(1.96*sqrt(({{var}}/{{n}})))), 
       width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd) +
    scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
    theme(strip.text.x = element_blank(),
          axis.text.x = element_text(colour = "black",
                                     size = 25))
}

Figure1_function.WB <- function(data, var, n) {
  pd <- position_dodge(0.05) # move them .05 to the left and right
  data %>%
    ggplot(aes(x=t, 
               y=wb.mean, 
               group = residence, 
               linetype = residence)) +
    facet_wrap(~male, nrow = 1) +
    labs(y = "Mean Mental Well-Being (SWEMWBS)", 
         x = "", 
         linetype = "Young Adult COVID-19 Living Arrangement") +
    geom_errorbar(
      aes(ymin=wb.mean-(1.96*sqrt(({{var}}/{{n}}))), 
          ymax=wb.mean+(1.96*sqrt(({{var}}/{{n}})))), 
      width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd) +
    scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
    theme(strip.text.x = element_text(colour = "black",
                                      size = 25),
          axis.text.x= element_blank())
}

figure_df.function <- function(data) {
  cov <-  summarise_cv(data)
  aged_17 <- summarise_17(data)
  
  rbind(cov, aged_17)
}

summarise_cv <- function(data){
    k6 <- data %>%
      mean.n.se(covid_K6,CW1_COMBWT,residence) %>%
      rename(k6.mean = mean, 
             k6.se = se, 
             k6.n = n) %>%
      mutate(t = "COVID-19")
  
  wb <- data %>%
    mean.n.se(covid_SWEMWBS,CW1_COMBWT,residence) %>%
    rename(wb.mean = mean,
           wb.se = se, 
           wb.n = n) %>%
    mutate(t = "COVID-19")
  
  k6 %>%
    full_join(wb, by = c("residence", "male", "t")) 
}
summarise_17 <- function(data){
  k6 <- data %>%
    mean.n.se(existing_K6,CW1_COMBWT,residence) %>%
    rename(k6.mean = mean, 
           k6.se = se, 
           k6.n = n) %>%
    mutate(t = "Aged 17")
  wb <- data %>%
    mean.n.se(existing_SWEMWBS,CW1_COMBWT,residence) %>%
    rename(wb.mean = mean, 
           wb.se = se, 
           wb.n = n) %>%
    mutate(t = "Aged 17")
  
  k6 %>%
    full_join(wb, by = c("residence", "male", "t")) 
}

mean.n.se <- function(data,var,weight,residence) {
  data %>%
    group_by({{residence}},male) %>%
    summarise(mean = weighted.mean({{var}}, {{weight}}, 
                                   na.rm = TRUE),
              se = wtd.var({{var}}, {{weight}}, 
                           na.rm = TRUE),
              n = n()) %>%
    ungroup() 
}