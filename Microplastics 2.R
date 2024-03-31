library(tidyverse)
library(gtsummary)

MPdata6<-MPdata6 %>% 
  mutate(`Level of Microplastics in Ocean Water`=
           factor(`Level of Microplastics in Ocean Water`,
                  levels = c("Low",
                             "Medium",
                             "High",
                             "Very High")),
         CANCER_CrudePrev0=CANCER_CrudePrev/100,
         BPHIGH_CrudePrev0=BPHIGH_CrudePrev/100,
         DIABETES_CrudePrev0=DIABETES_CrudePrev/100,
         STROKE_CrudePrev0=STROKE_CrudePrev/100,
         HIGHCHOL_CrudePrev0=HIGHCHOL_CrudePrev/100,
         CHD_CrudePrev0=CHD_CrudePrev/100,
         OBESITY_CrudePrev0=OBESITY_CrudePrev/100)

MPdata4a<-MPdata4a %>% 
  mutate(`Level of Microplastics in Ocean Water`=
           factor(`Level of Microplastics in Ocean Water`,
                  levels = c("Low",
                             "Medium",
                             "High",
                             "Very High"))) %>% view()
MPdata4a1<-MPdata4a1 %>% 
  mutate(`Level of Microplastics in Ocean Water`=
           factor(`Level of Microplastics in Ocean Water`,
                  levels = c("Low",
                             "Medium",
                             "High",
                             "Very High")))
#making SVI categories as well
MPdata4a1<-MPdata4a1 %>% 
  mutate("SVI Category" = if_else(RPL_THEMES >=0.75, "Q4",
                                  if_else(RPL_THEMES>=0.50, "Q3",
                                          if_else(RPL_THEMES>=0.25, "Q2",
                                                  "Q1")))) %>% view()

view(MPdata4a1)


#starting regression with our MPdata4a dattaset, it has an environmental factor
#it is called the CVI
library(gtsummary)
MPglm27.1age<-MPdata4a %>% 
  glm(HIGHCHOL_crprev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `Active Physicians per 10,000 population` +
        `CVI Percentile Rank`,
      family = quasipoisson,
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm27.1age, exponentiate = TRUE)
view(MPdata4a1)
MPglm26.1age<-MPdata4a1 %>% 
  glm(DIABETES_crprev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `Active Physicians per 10,000 population` +
        RPL_THEMES +
        `Criteria Air pollutants`,
      family = quasipoisson(link='logit'),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm26.1age, exponentiate = TRUE)

MPglm26a.1age<-MPdata4a1 %>% 
  glm(DIABETES_crprev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `Active Physicians per 10,000 population` +
        `CVI Percentile Rank`,
      family = quasipoisson,
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm26a.1age, exponentiate = TRUE)
###
##trials
view(MPdata4a)

MPglm_trial<-MPdata4a %>% 
  glm(BPHIGH_crprev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `CVI Percentile Rank` +
        `Active Physicians per 10,000 population`,
      family = quasipoisson (link = 'logit'),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm_trial, exponentiate = TRUE)

###

MPglm26.2age<-MPdata4a1 %>% 
  glm(DIABETES_CrudePrev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `CVI Category`+
        offset(log(E_TOTPOP)),
      family = quasipoisson,
      data = .)
tbl_regression(MPglm26.2age, exponentiate = TRUE)

MPglm25.1age<-MPdata4a1 %>% 
  glm(CHD_crprev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `Active Physicians per 10,000 population` +
        RPL_THEMES +
        `Criteria Air pollutants`,
      family = quasipoisson (link='logit'),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm25.1age, exponentiate = TRUE)

MPglm25a.1age<-MPdata4a1 %>% 
  glm(CHD_crprev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `Active Physicians per 10,000 population` +
        `CVI Percentile Rank`,
      family = quasipoisson (link='logit'),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm25a.1age, exponentiate = TRUE)

MPglm24.1age<-MPdata4a1 %>% 
  glm(OBESITY_crprev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `CVI Percentile Rank`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm24.1age, exponentiate = TRUE)

MPglm23.1age<-MPdata4a1 %>% 
  glm(STROKE_crprev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `Active Physicians per 10,000 population` +
        `CVI Percentile Rank`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm23.1age, exponentiate = TRUE)

MPglm22.1age<-MPdata4a1 %>% 
  glm(BPHIGH_crprev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `Active Physicians per 10,000 population` +
        `CVI Percentile Rank`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm22.1age, exponentiate = TRUE)

MPglm21.1age<-MPdata4a1 %>% 
  glm(CANCER_crprev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `Active Physicians per 10,000 population` +
        `CVI Percentile Rank`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm21.1age, exponentiate = TRUE)
view(MPdata4a)
#dochotomizing CVI
library(tidyverse)
MPdata4a1<-MPdata4a1 %>% 
  mutate("CVI Category" = if_else(`CVI Percentile Rank` >=75, "Q4",
                                  if_else(`CVI Percentile Rank`>=50, "Q3",
                                          if_else(`CVI Percentile Rank`>=25, "Q2",
                                                  "Q1"))))
    
MPdata4a<-MPdata4a %>% 
  mutate(`CVI Category`=factor(`CVI Category`,
                               levels = c("Below Median",
                                          "Above Median")))
#making quartiles of criteria air pollutants
MPdata4a1<-MPdata4a1 %>% 
  mutate(Air_Cat = if_else(`Criteria Air pollutants`>=75, "Q4",
                                             if_else(`Criteria Air pollutants`>=50, "Q3",
                                                     if_else(`Criteria Air pollutants`>=25, "Q2",
                                                             "Q1"))))

#making aircat a factorial variable
MPdata4a1<-MPdata4a1 %>% 
  mutate(Air_Cat=as.factor(Air_Cat)) %>% 
  view()
levels(MPdata4a1$Air_Cat)

#renaming a column
MPdata6<-MPdata6 %>% 
  rename("Days with good air" = "% days with Ambient AQI less than 50")

####
#now running regressions with AQI merged database
MPglm31.1age<-MPdata6 %>% 
  glm(CANCER_CrudePrev0 ~ `Level of Microplastics in Ocean Water` +
        `Median Age of the County` +
        `SVI Percentile` +
        `Active Physicians per 10,000 people` +
        `Days with good air`,
      family = quasipoisson(link="logit"),
      weights = `Total Population of the County`,
      data = .)
tbl_regression(MPglm31.1age, exponentiate = TRUE)

MPglm32.1age<-MPdata6 %>% 
  glm(BPHIGH_CrudePrev0 ~ `Level of Microplastics in Ocean Water` +
        `Median Age of the County` +
        `SVI Percentile` +
        `Active Physicians per 10,000 people` +
        `Days with good air`,
      family = quasipoisson(link="logit"),
      weights = `Total Population of the County`,
      data = .)
tbl_regression(MPglm32.1age, exponentiate = TRUE)

MPglm33.1age<-MPdata6 %>% 
  glm(STROKE_CrudePrev0 ~ `Level of Microplastics in Ocean Water` +
        `Median Age of the County` +
        `SVI Percentile` +
        `Active Physicians per 10,000 people` +
        `Days with good air`,
      family = quasipoisson(link="logit"),
      weights = `Total Population of the County`,
      data = .)
tbl_regression(MPglm33.1age, exponentiate = TRUE)

MPglm34.1age<-MPdata6 %>% 
  glm(OBESITY_CrudePrev0 ~ `Level of Microplastics in Ocean Water` +
        `Median Age of the County` +
        `SVI Percentile` +
        `Active Physicians per 10,000 people` +
        `Days with good air`,
      family = quasipoisson(link="logit"),
      weights = `Total Population of the County`,
      data = .)
tbl_regression(MPglm34.1age, exponentiate = TRUE)

MPglm35.1age<-MPdata6 %>% 
  glm(CHD_CrudePrev0 ~ `Level of Microplastics in Ocean Water` +
        `Median Age of the County` +
        `SVI Percentile` +
        `Active Physicians per 10,000 people` +
        `Days with good air`,
      family = quasipoisson(link="logit"),
      weights = `Total Population of the County`,
      data = .)
tbl_regression(MPglm35.1age, exponentiate = TRUE)

MPglm36.1age<-MPdata6 %>% 
  glm(DIABETES_CrudePrev0 ~ `Level of Microplastics in Ocean Water` +
        `Median Age of the County` +
        `SVI Percentile` +
        `Active Physicians per 10,000 people` +
        `Days with good air`,
      family = quasipoisson(link="logit"),
      weights = `Total Population of the County`,
      data = .)
tbl_regression(MPglm36.1age, exponentiate = TRUE)

MPglm37.1age<-MPdata6 %>% 
  glm(HIGHCHOL_CrudePrev0 ~ `Level of Microplastics in Ocean Water` +
        `Median Age of the County` +
        `SVI Percentile` +
        `Active Physicians per 10,000 people` +
        `Days with good air`,
      family = quasipoisson(link="logit"),
      weights = `Total Population of the County`,
      data = .)
tbl_regression(MPglm37.1age, exponentiate = TRUE)

#Making a 3-leveled microplastics  class for all our MPdatasate
view(MPdata4a1)

MPdata4a1<-MPdata4a1 %>% 
  mutate(`Level of Microplastics in Ocean Water`=if_else(
    `Microplastics: Ocean Water`>=1,"High",
    if_else(`Microplastics: Ocean Water`>=0.005,"Medium",
            "Low") 
  )) 
MPdata4a1<-MPdata4a1 %>% 
  mutate(`Level of Microplastics in Ocean Water`=factor(
    `Level of Microplastics in Ocean Water`, levels = c("Low",
                                                     "Medium",
                                                     "High")))
view(MPdata4a1)

###########################################
####################
#Final regression using MPdata4a dataset and age, physicians and CVI as covars

library(tidyverse)
MPglm41.1age<-MPdata4a %>% 
  glm(DIABETES_crprev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `Active Physicians per 10,000 population` +
        `CVI Percentile Rank`,
      family = quasipoisson(link='logit'),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm41.1age, exponentiate = TRUE)


MPglm42.1age<-MPdata4a %>% 
  glm(CHD_crprev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `Active Physicians per 10,000 population` +
        `CVI Percentile Rank`,
      family = quasipoisson(link='logit'),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm42.1age, exponentiate = TRUE)

MPglm43.1age<-MPdata4a %>% 
  glm(STROKE_crprev ~ `Level of Microplastics in Ocean Water` +
        ACS_MEDIAN_AGE +
        `Active Physicians per 10,000 population` +
        `CVI Percentile Rank`,
      family = quasipoisson(link='logit'),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm43.1age, exponentiate = TRUE)

Cstat(MPglm41.1age)
Cstat(MPglm42.1age)
Cstat(MPglm43.1age)
Cstat(MPglm41.1raw)
Cstat(MPglm42.1raw)
Cstat(MPglm43.1raw)

#raw unadjusted models
MPglm41.1raw<-MPdata4a %>% 
  glm(DIABETES_crprev ~ `Level of Microplastics in Ocean Water` ,
      family = quasipoisson(link='logit'),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm41.1raw, exponentiate = TRUE)

MPglm42.1raw<-MPdata4a %>% 
  glm(CHD_crprev ~ `Level of Microplastics in Ocean Water`,
      family = quasipoisson(link='logit'),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm42.1raw, exponentiate = TRUE)

MPglm43.1raw<-MPdata4a %>% 
  glm(STROKE_crprev ~ `Level of Microplastics in Ocean Water`,
      family = quasipoisson(link='logit'),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm43.1raw, exponentiate = TRUE)

#forest plots here now
#diabetes
BoxPlotT2D<-MPdata4 %>% 
  select(`Microplastics level in Ocean Water`,DIABETES_CrudePrev) %>% 
  filter(`Microplastics level in Ocean Water`=="Low"|
           `Microplastics level in Ocean Water`=="Very High") %>% 
  ggplot(mapping = aes(`Microplastics level in Ocean Water`,
                       DIABETES_CrudePrev),
         alpha(0.2)) +
  geom_boxplot(fill = "grey") +
  labs(
    title = "Prevalence of T2D",
    subtitle = "In coastal counties stratified by concentration of MNPs",
    x = "Concentration of Microplastics",
    y = "Prevalence (%)"
  ) +
  theme_classic()

#stroke
BoxPlotStroke<-MPdata4 %>% 
  select(`Microplastics level in Ocean Water`,STROKE_CrudePrev) %>% 
  filter(`Microplastics level in Ocean Water`=="Low"|
           `Microplastics level in Ocean Water`=="Very High") %>% 
  ggplot(mapping = aes(`Microplastics level in Ocean Water`,
                       STROKE_CrudePrev),
         alpha(0.2)) +
  geom_boxplot(fill = "grey") +
  labs(
    title = "Prevalence of Stroke",
    subtitle = "In coastal counties stratified by concentration of MNPs",
    x = "Concentration of Microplastics",
    y = "Prevalence (%)"
  ) +
  theme_classic()

#CHD
BoxPlotCHD<-MPdata4 %>% 
  select(`Microplastics level in Ocean Water`,CHD_CrudePrev) %>% 
  filter(`Microplastics level in Ocean Water`=="Low"|
           `Microplastics level in Ocean Water`=="Very High") %>% 
  ggplot(mapping = aes(`Microplastics level in Ocean Water`,
                       CHD_CrudePrev),
         alpha(0.2)) +
  geom_boxplot(fill = "grey") +
  labs(
    title = "Prevalence of Coronary Heart",
    subtitle = "In coastal counties stratified by concentration of MNPs",
    x = "Concentration of Microplastics",
    y = "Prevalence (%)"
  ) +
  theme_classic()