library(tidyverse)
Coastal_County_MP_Temp_PLACES_SVI_and_AHRF_TEXT<-Coastal_County_MP_Temp_PLACES_SVI_and_AHRF_TEXT %>% 
  mutate(Microplastics_Ocean_Water_CLASS=factor(Microplastics_Ocean_Water_CLASS,
                                                levels = c("Low",
                                                           "Medium",
                                                           "High",
                                                           "Very High"))) %>% view()
  mutate(CANCER_AdjPrev=CANCER_AdjPrev/100) %>% 
  mutate(BPHIGH_AdjPrev=BPHIGH_AdjPrev/100) %>% 
  mutate(CHD_AdjPrev=CHD_AdjPrev/100) %>% 
  mutate(OBESITY_AdjPrev=OBESITY_AdjPrev/100)
view(Coastal_County_MP_Temp_PLACES_SVI_and_AHRF_TEXT)
#installing GT summary package for tblregression funcion
library(gtsummary)
#trying to run an unadjusted model that models association between#
#cancer and microplastic concentration#
MPglm1<-glm(CANCER_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
    family = binomial(link="logit"),
    data = Coastal_County_MP_Temp_PLACES_SVI_and_AHRF_TEXT)
         summary (MPglm1)
exp(coef(MPglm1))         
tbl_regression(MPglm1, exponentiate = TRUE)

#trying to run a model that describes the relation between
#cancer and microplastic concentration adjusting for SVI

MPglm2<-glm(CANCER_AdjPrev ~ Microplastics_Ocean_Water_CLASS + RPL_THEMES,
    family = poisson(link="log"),
    data = Coastal_County_MP_Temp_PLACES_SVI_and_AHRF_TEXT)
exp(coef(summary(MPglm2)))
tbl_regression(MPglm2, exponentiate = TRUE)

#high BP and micro plastics with and without adjustments for SVI

MPglm3<-glm(BPHIGH_AdjPrev ~ Microplastics_Ocean_Water_CLASS + RPL_THEMES,
            family = binomial(link="logit"),
    data = Coastal_County_MP_Temp_PLACES_SVI_and_AHRF_TEXT)
tbl_regression(MPglm3, exponentiate = TRUE)

MPglm3.1<-glm(BPHIGH_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
            family = binomial(link="logit"),
            data = Coastal_County_MP_Temp_PLACES_SVI_and_AHRF_TEXT)
tbl_regression(MPglm3.1, exponentiate = TRUE)

MPglm3<-glm(BPHIGH_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
            family = poisson,
            data = Coastal_County_MP_Temp_PLACES_SVI_and_AHRF_TEXT)

MPglm4<-glm(CHD_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
              family = binomial(link="logit"),
              data = Coastal_County_MP_Temp_PLACES_SVI_and_AHRF_TEXT)
summary(MPglm4)
exp(coef(MPglm4))
tbl_regression(MPglm4, exponentiate = TRUE)

MPglm5<-glm(OBESITY_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
            family = binomial(link="logit"),
            data = Coastal_County_MP_Temp_PLACES_SVI_and_AHRF_TEXT)
tbl_regression(MPglm5, exponentiate = TRUE)

summary(Coastal_County_MP_Temp_PLACES_SVI_and_AHRF_TEXT)

#running regression model of OBESITY and its relation with microplastics
MPglm6<-MPdata %>% 
  select(Microplastics_Ocean_Water_CLASS, OBESITY_AdjPrev) %>% 
  mutate(Microplastics_Ocean_Water_CLASS=factor(Microplastics_Ocean_Water_CLASS)) %>% 
  mutate(OBESITY_AdjPrev=OBESITY_AdjPrev/100) %>% 
  glm(OBESITY_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
      family = binomial(link="logit"),
      data = .)
tbl_regression(MPglm6, exponentiate = TRUE)

MPglm6<-MPdata %>% 
glm(OBESITY_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
    family = poisson(link="logit"),
    weights = `Total Population 2018 countywise`,
    data = .)
tbl_regression(MPglm6, exponentiate = TRUE)
#playing with the table of MPdata  to get the variables that I need
MPdata<-MPdata %>% 
  mutate(Microplastics_Ocean_Water_CLASS=factor(Microplastics_Ocean_Water_CLASS)) %>% 
  mutate(OBESITY_AdjPrev=OBESITY_AdjPrev/100) %>% 
  mutate(CANCER_AdjPrev=CANCER_AdjPrev/100) %>% 
  mutate(BPHIGH_AdjPrev=BPHIGH_AdjPrev/100) %>% 
  mutate(CHD_AdjPrev=CHD_AdjPrev/100) %>% 
#bhul thi obesity be vaar thy gai
MPdata<-MPdata %>% 
  mutate(OBESITY_AdjPrev=OBESITY_AdjPrev*100) %>% 
  mutate(STROKE_AdjPrev=STROKE_AdjPrev/100) %>% 
  view()

#back to regression from mpglm7
MPglm7<-MPdata %>% 
  glm(CANCER_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
      family = binomial(link="logit"),
      data = .)
tbl_regression(MPglm7, exponentiate = TRUE)
view(MPdata)

#Univariate and multivariate analysis for Obesity
MPglm6a<-MPdata %>% 
  glm(OBESITY_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
      family = binomial(link="logit"),
      data = .)
tbl_regression(MPglm6a, exponentiate = TRUE)

MPglm6a.1<-MPdata %>% 
  glm(OBESITY_AdjPrev ~ Microplastics_Ocean_Water_CLASS +
        `Number of active MDs per 10k pop` +
        RPL_THEMES +
        Avg_percent_days_abovePM25,
      family = poisson(link="logit"),
      weights = `Total Population 2018 countywise`,
      data = .)
exp(coef(summary(MPglm6a.1)))
tbl_regression(MPglm6a.1, exponentiate = TRUE)

#just duplicating model 6a.1 to include 
library(MASS)
MPglm6a.1<-MPdata %>% 
  glm.nb(OBESITY_AdjPrev ~ Microplastics_Ocean_Water_CLASS +
        `Number of active MDs per 10k pop` +
        RPL_THEMES +
        Avg_percent_days_abovePM25,
      weights = `Total Population 2018 countywise`,
      data = .)
exp(coef(summary(MPglm6a.1)))
tbl_regression(MPglm6a.1, exponentiate = TRUE)
#Univariate and multivariate regression for Cancer
MPglm7<-MPdata %>% 
  glm(CANCER_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
      family = poisson(link="logit"),
      weights = `Total Population 2018 countywise`,
      data = .)
tbl_regression(MPglm7, exponentiate = TRUE)
#univariate for cancer but now using the poisson regression family
MPglm7.0.1<-MPdata %>% 
  glm(CANCER_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
      family = poisson(link="logit"),
      data = .)
exp(MPglm7.0.1$coefficients)
tbl_regression(MPglm7.0.1, exponentiate = TRUE)
#trying negative binomial regression as well as putting in the
# weights=total population argument 
#thanks to chatGPT and stack overflow
library(MASS)
MPglm7.0.2<-MPdata %>% 
  glm.nb(CANCER_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
         weights = `Total Population 2018 countywise`,
      data = .)
exp(MPglm7.0.2$coefficients)
tbl_regression(MPglm7.0.2, exponentiate = TRUE)
#the same 7.0.2 model but now adjusted for other variables
#so weight kind of gives a relative importacne or 
#literally weight to any vactor assigned to it
#will use the glm.nb now beacuse it is a broader term
#and adjusts for overdispersion
MPglm7.0.3<-MPdata %>% 
  glm.nb(CANCER_AdjPrev ~ Microplastics_Ocean_Water_CLASS +
           `Number of active MDs per 10k pop` +
           RPL_THEMES +
           Avg_percent_days_abovePM25,
         weights = `Total Population 2018 countywise`,
         data = .)
exp(coef(summary(MPglm7.0.3)))
tbl_regression(MPglm7.0.3, exponentiate = TRUE)

MPglm7.1<-MPdata %>% 
  glm(CANCER_AdjPrev ~ Microplastics_Ocean_Water_CLASS +
        `Number of active MDs per 10k pop` +
        RPL_THEMES +
        Avg_percent_days_abovePM25,
      family = poisson(link="logit"),
      weights = `Total Population 2018 countywise`,
      data = .)
exp(coef(summary(MPglm7.1)))
tbl_regression(MPglm7.1, exponentiate = TRUE)

#using the healthcare resource distribution only
MPglm7.1.1<-MPdata %>% 
  glm(CANCER_AdjPrev ~ Microplastics_Ocean_Water_CLASS +
        `Number of active MDs per 10k pop`,
      family = poisson(link="logit"),
      data = .)
exp(coef(summary(MPglm7.1)))
tbl_regression(MPglm7.1, exponentiate = TRUE)

#Uni variate and multivariate regression for CHD
MPglm8<-MPdata %>% 
  glm(CHD_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
      family = poisson(link="logit"),
      weights = `Total Population 2018 countywise`,
      data = .)
exp(coef(summary(MPglm8)))
tbl_regression(MPglm8, exponentiate = TRUE)

MPglm8.1<-MPdata %>% 
  glm(CHD_AdjPrev ~ Microplastics_Ocean_Water_CLASS +
        `Number of active MDs per 10k pop` +
        RPL_THEMES +
        Avg_percent_days_abovePM25,
      weights = `Total Population 2018 countywise`,
      family = poisson(link="logit"),
      data = .)
exp(coef(summary(MPglm8.1)))
tbl_regression(MPglm8.1, exponentiate = TRUE)
#now only microplastic concentration and svi
MPglm8.2<-MPdata %>% 
  glm(CHD_AdjPrev ~ Microplastics_Ocean_Water_CLASS +
        RPL_THEMES,
      family = poisson(link="logit"),
      
      data = .)
exp(coef(summary(MPglm8.2)))
tbl_regression(MPglm8.2, exponentiate = TRUE)
Cstat(MPglm8.2)
#now physician distribution and microplastics
MPglm8.3<-MPdata %>% 
  glm(CHD_AdjPrev ~ Microplastics_Ocean_Water_CLASS +
        `Number of active MDs per 10k pop`,
      family = poisson(link="logit"),
      weights = `Total Population 2018 countywise`,
      data = .)
exp(confint(MPglm8.3))
exp(coef(summary(MPglm8.3)))
tbl_regression(MPglm8.3, exponentiate = TRUE)
#using glm.nb and weights argument for prevalance of chd
MPglm8.1.1<-MPdata %>% 
  glm.nb(CHD_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
         weights = `Total Population 2018 countywise`,
      data = .)
exp(coef(summary(MPglm8.1.1)))
tbl_regression(MPglm8.1.1, exponentiate = TRUE)

#Uni variate and multivariate analysis for Hypertension
MPglm9<-MPdata %>% 
  glm(BPHIGH_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
      family = poisson(link="logit"),
      weights = `Total Population 2018 countywise`,
      data = .)
exp(coef(summary(MPglm9)))
tbl_regression(MPglm9, exponentiate = TRUE)

#using glmnb for hypertension modelling
MPglm9<-MPdata %>% 
  glm.nb(BPHIGH_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
      
      weights = `Total Population 2018 countywise`,
      data = .)
exp(coef(summary(MPglm9)))
tbl_regression(MPglm9, exponentiate = TRUE)


MPglm9.1<-MPdata %>% 
  glm(BPHIGH_AdjPrev ~ Microplastics_Ocean_Water_CLASS +
        `Number of active MDs per 10k pop` +
        RPL_THEMES +
        Avg_percent_days_abovePM25,
      family = poisson(link="logit"),
      weights = `Total Population 2018 countywise`,
      data = .)
exp(coef(summary(MPglm9.1)))
tbl_regression(MPglm9.1, exponentiate = TRUE)

MPglm9.2<-MPdata %>% 
  glm(BPHIGH_AdjPrev ~ Microplastics_Ocean_Water_CLASS +
        RPL_THEMES,
      family = binomial (link="logit"),
      data = .)
exp(coef(summary(MPglm9.2)))
tbl_regression(MPglm9.2, exponentiate = TRUE)

MPglm9.2.1<-MPdata %>% 
  glm(BPHIGH_AdjPrev ~ Microplastics_Ocean_Water_CLASS +
        RPL_THEMES,
      family = poisson (link="logit"),
      data = .)
exp(coef(summary(MPglm9.2.1)))
tbl_regression(MPglm9.2.1, exponentiate = TRUE)

#stroke and microplastic concentrations
MPglm10<-MPdata %>% 
  glm(STROKE_AdjPrev ~ Microplastics_Ocean_Water_CLASS,
      family = poisson(link="logit"),
      weights = `Total Population 2018 countywise`,
      data = .)
tbl_regression(MPglm10, exponentiate = TRUE)

MPglm10.1<-MPdata %>% 
  glm(STROKE_AdjPrev ~ Microplastics_Ocean_Water_CLASS +
        `Number of active MDs per 10k pop` +
        RPL_THEMES +
        Avg_percent_days_abovePM25,
      family = poisson(link="logit"),
      weights = E_TOT,
      data = .)
exp(coef(summary(MPglm10.1)))
tbl_regression(MPglm10.1, exponentiate = TRUE)

#manipulating the MPdata1 dataframe
library(tidyverse)
MPdata1<-MPdata1 %>% 
  mutate(`Microplastics: Ocean Water CLASS`=factor(`Microplastics: Ocean Water CLASS`,
                                                   levels = c("Low",
                                                              "Medium",
                                                              "High",
                                                              "Very High"))) %>% view()
MPdata2<-MPdata2 %>% 
  mutate(`Microplastics: Ocean Water CLASS`=factor(`Microplastics: Ocean Water CLASS`,
                                                   levels = c("Low",
                                                              "Medium",
                                                              "High",
                                                              "Very High"))) %>% view()
MPdata2<-MPdata2 %>% 
  mutate(CANCER_AdjPrev=CANCER_AdjPrev/100,
         DIABETES_AdjPrev=DIABETES_AdjPrev/100,
         BPHIGH_AdjPrev=BPHIGH_AdjPrev/100,
         STROKE_AdjPrev=STROKE_AdjPrev/100,
         CHD_AdjPrev=CHD_AdjPrev/100,
         OBESITY_AdjPrev=OBESITY_AdjPrev/100) %>% 
  View()

MPdata3<-MPdata3 %>% 
  mutate(CANCER_AdjPrev=CANCER_AdjPrev/100,
         DIABETES_AdjPrev=DIABETES_AdjPrev/100,
         BPHIGH_AdjPrev=BPHIGH_AdjPrev/100,
         STROKE_AdjPrev=STROKE_AdjPrev/100,
         CHD_AdjPrev=CHD_AdjPrev/100,
         OBESITY_AdjPrev=OBESITY_AdjPrev/100) %>%
  mutate(`Microplastics: Ocean Water CLASS`=factor(`Microplastics: Ocean Water CLASS`,
                                                   levels = c("Low",
                                                              "Medium",
                                                              "High",
                                                              "Very High"))) %>%
  View()
view(MPdata3)
MPdata4<-MPdata4 %>% 
  mutate(CANCER_AdjPrev=CANCER_AdjPrev/100,
         DIABETES_AdjPrev=DIABETES_AdjPrev/100,
         BPHIGH_AdjPrev=BPHIGH_AdjPrev/100,
         STROKE_AdjPrev=STROKE_AdjPrev/100,
         CHD_AdjPrev=CHD_AdjPrev/100,
         OBESITY_AdjPrev=OBESITY_AdjPrev/100) %>%
  mutate(`Microplastics: Ocean Water CLASS`=factor(`Microplastics: Ocean Water CLASS`,
                                                   levels = c("Low",
                                                              "Medium",
                                                              "High",
                                                              "Very High"))) %>% 
  view(MPdata4)
MPdata5<-MPdata5 %>% 
  mutate(`Microplastics: Ocean Water CLASS`=factor(`Microplastics: Ocean Water CLASS`,
                                                   levels = c("Low",
                                                              "Medium",
                                                              "High",
                                                              "Very High"))) %>%
  mutate(PMcat=factor(PMcat)) %>% 
  view()

summary(MPdata4$Avg_percent_days_abovePM25)
hist(MPdata4$Avg_percent_days_abovePM25)
view(EPHmeanPM25)
MPdata5<-
merge.data.frame(x=MPdata5, y=EPHmeanPM25, 
      by.x = "FIPS",
      by.y = "STATE/ COUNTY FIPS") %>% 
  view(MPdata5)
MPdata4$FIPS=MPdata4$`STATE/
COUNTY
FIPS`
EPHmeanPM25
#cancer prevalance adjusted and unadjusted
MPglm11.1<-MPdata3 %>% 
  glm(CANCER_crprev ~ `Microplastics: Ocean Water CLASS` +
        RPL_THEMES +
        Avg_percent_days_abovePM25 +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
MPglm11.1age<-MPdata4 %>% 
  glm(CANCER_crprev ~ `Microplastics: Ocean Water CLASS` +
        ACS_MEDIAN_AGE +
        RPL_THEMES +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm11.1)))
tbl_regression(MPglm11.1, exponentiate = TRUE)
tbl_regression(MPglm11.1age, exponentiate = TRUE)
library(tidyverse)
library(gtsummary)
library(DescTools)
Cstat(MPglm11.1)
library(ResourceSelection)
hoslem.test(x=MPglm11.1$y, y=fitted(MPglm11.1), g=10)
anova(MPglm11.1, test = "Chisq")

#with adjusted  cancer prevalance
MPglm11.1adj<-MPdata2 %>% 
  glm(CANCER_AdjPrev ~ `Microplastics: Ocean Water CLASS` +
        RPL_THEMES +
        Avg_percent_days_abovePM25 +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm11.1adj, exponentiate = TRUE)

MPglm11<-MPdata2 %>% 
  glm(CANCER_crprev ~ `Microplastics: Ocean Water CLASS`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm11)))
summary(MPglm11)
tbl_regression(MPglm11, exponentiate = TRUE)
Cstat(MPglm11)

#modelling HTN and microplastics
MPglm12.1<-MPdata2 %>% 
  glm(BPHIGH_crprev ~ `Microplastics: Ocean Water CLASS` +
        RPL_THEMES +
        Avg_percent_days_abovePM25 +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm12.1)))
exp(confint(MPglm12.1))
tbl_regression(MPglm12.1, exponentiate = TRUE)

#age adjusted 
view(MPdata4)
MPglm12.1age<-MPdata4 %>% 
  glm(BPHIGH_crprev ~ `Microplastics: Ocean Water CLASS` +
        ACS_MEDIAN_AGE +
        RPL_THEMES +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm12.1age, exponentiate = TRUE)
#for adjusted prevalance
MPglm12.1adj<-MPdata2 %>% 
  glm(BPHIGH_AdjPrev ~ `Microplastics: Ocean Water CLASS` +
        RPL_THEMES +
        Avg_percent_days_abovePM25 +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm12.1adj, exponentiate = TRUE)

MPglm12<-MPdata2 %>% 
  glm(BPHIGH_crprev ~ `Microplastics: Ocean Water CLASS`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm12)))
tbl_regression(MPglm12, exponentiate = TRUE)

#modeling stroke and microplastics
MPglm13.1<-MPdata2 %>% 
  glm(STROKE_crprev ~ `Microplastics: Ocean Water CLASS` +
        RPL_THEMES +
        Avg_percent_days_abovePM25 +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm13.1)))
tbl_regression(MPglm13.1, exponentiate = TRUE)
#age adjusted
MPglm13.1age<-MPdata4 %>% 
  glm(STROKE_crprev ~ `Microplastics: Ocean Water CLASS` +
        ACS_MEDIAN_AGE +
        RPL_THEMES +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm13.1)))
tbl_regression(MPglm13.1age, exponentiate = TRUE)
#modelling adjusted stroke
MPglm13.1adj<-MPdata2 %>% 
  glm(STROKE_AdjPrev ~ `Microplastics: Ocean Water CLASS` +
        RPL_THEMES +
        Avg_percent_days_abovePM25 +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm13.1adj, exponentiate = TRUE)

MPglm13<-MPdata1 %>% 
  glm(STROKE_crprev ~ `Microplastics: Ocean Water CLASS`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm13)))
tbl_regression(MPglm13, exponentiate = TRUE)

#modeling obesity and microplastics
MPglm14.1<-MPdata2 %>% 
  glm(OBESITY_crprev ~ `Microplastics: Ocean Water CLASS` +
        RPL_THEMES +
        Avg_percent_days_abovePM25 +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm14.1)))
tbl_regression(MPglm14.1, exponentiate = TRUE)
#age adjusted
MPglm14.1age<-MPdata4 %>% 
  glm(OBESITY_crprev ~ `Microplastics: Ocean Water CLASS` +
        ACS_MEDIAN_AGE +
        RPL_THEMES +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm14.1)))
tbl_regression(MPglm14.1age, exponentiate = TRUE)
#modelling adjusted prev
MPglm14.1adj<-MPdata2 %>% 
  glm(OBESITY_AdjPrev ~ `Microplastics: Ocean Water CLASS` +
        RPL_THEMES +
        Avg_percent_days_abovePM25 +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm14.1)))
tbl_regression(MPglm14.1adj, exponentiate = TRUE)

MPglm14<-MPdata1 %>% 
  glm(OBESITY_crprev ~ `Microplastics: Ocean Water CLASS`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm14)))
tbl_regression(MPglm14, exponentiate = TRUE)

#modeling CHD and micro plastics
MPglm15.1<-MPdata2 %>% 
  glm(CHD_crprev ~ `Microplastics: Ocean Water CLASS` +
        RPL_THEMES +
        Avg_percent_days_abovePM25 +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm15.1)))
tbl_regression(MPglm15.1, exponentiate = TRUE)
#age adjusted
MPglm15.1age<-MPdata4 %>% 
  glm(CHD_CrudePrev ~ `Microplastics: Ocean Water CLASS` +
        ACS_MEDIAN_AGE +
        RPL_THEMES +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm15.1)))
tbl_regression(MPglm15.1age, exponentiate = TRUE)
#using adjusted model for the same
MPglm15.1adj<-MPdata2 %>% 
  glm(CHD_AdjPrev ~ `Microplastics: Ocean Water CLASS` +
        RPL_THEMES +
        Avg_percent_days_abovePM25 +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm15.1)))
tbl_regression(MPglm15.1adj, exponentiate = TRUE)

MPglm15<-MPdata1 %>% 
  glm(CHD_crprev ~ `Microplastics: Ocean Water CLASS`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm15)))
tbl_regression(MPglm15, exponentiate = TRUE)

#modelling in diabetes prevalence, both adj and unadj
MPglm16.1<-MPdata2 %>% 
  glm(DIABETES_crprev ~ `Microplastics: Ocean Water CLASS` +
        RPL_THEMES +
        Avg_percent_days_abovePM25 +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm16.1)))
tbl_regression(MPglm16.1, exponentiate = TRUE)
#age adjusted
MPglm16.1age<-MPdata4 %>% 
  glm(DIABETES_crprev ~ `Microplastics: Ocean Water CLASS` +
        ACS_MEDIAN_AGE +
        RPL_THEMES +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm16.1)))
tbl_regression(MPglm16.1age, exponentiate = TRUE)

MPglm16.2age<-MPdata4 %>% 
  glm(DIABETES_CrudePrev ~ `Microplastics level in Ocean Water` +
        `Median Age` +
        `SVI Percentile` +
        `Number of active physicians per 10,000 people` +
        offset ( log(`Total Population of County`)),
      family = poisson(link="logit"),
      data = .)
exp(coef(summary(MPglm16.1)))
tbl_regression(MPglm16.1age, exponentiate = TRUE)

#doing adj_prev for diabtees
MPglm16.1adj<-MPdata2 %>% 
  glm(DIABETES_AdjPrev ~ `Microplastics: Ocean Water CLASS` +
        RPL_THEMES +
        Avg_percent_days_abovePM25 +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm16.1adj)))
tbl_regression(MPglm16.1adj, exponentiate = TRUE)

MPglm16<-MPdata2 %>% 
  glm(DIABETES_crprev ~ `Microplastics: Ocean Water CLASS`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
exp(coef(summary(MPglm16)))
tbl_regression(MPglm16, exponentiate = TRUE)

#highcholesterol prevalance
MPglm17.1age<-MPdata4 %>% 
  glm(HIGHCHOL_crprev ~ `Microplastics: Ocean Water CLASS` +
        ACS_MEDIAN_AGE +
        RPL_THEMES +
        `Active Physicians pre 10k population`,
      family = quasipoisson(link="logit"),
      weights = E_TOTPOP,
      data = .)
tbl_regression(MPglm17.1age, exponentiate = TRUE)

#runnning the c statistics
library(DescTools)
library(tidyverse)
library(gtsummary)
Cstat(MPglm16.1age)
Cstat(MPglm15.1age)
Cstat(MPglm14.1age)
Cstat(MPglm13.1age)
Cstat(MPglm12.1age)
Cstat(MPglm11.1age)
summary(MPdata4)
View(MPdata4)
#labelling our variables
MPdata4<-MPdata4 %>% 
  rename("Number of active physicians per 10,000 people" = `Active Physicians pre 10k population`,
         "Microplastics level in Ocean Water"=`Microplastics: Ocean Water CLASS`,
         "SVI Percentile"=RPL_THEMES,
         "Total Population of County"=E_TOTPOP,
         "Prevalence of Hypertension"=BPHIGH_crprev,
         "Prevalence of T2DM"=DIABETES_crprev,
         "Prevalence of Coronary Heart Disease"=CHD_crprev,
         "Prevalence of Stroke"=STROKE_crprev,
         "Median Age"=ACS_MEDIAN_AGE)
view(MPdata4a)
#basic gtsummary working
#argument = select variables ~ give instructions
library(gtsummary)
library(tidyverse)
MPdata4a %>% 
  select(ACS_MEDIAN_AGE,
         E_TOTPOP,
         `Active Physicians per 10,000 population`,
         `Level of Microplastics in Ocean Water`,
         `CVI Percentile Rank`,
         CHD_CrudePrev,
         DIABETES_CrudePrev,
         STROKE_CrudePrev,
         ) %>%
  tbl_summary(
    by = `Level of Microplastics in Ocean Water`,
    statistic = list(E_TOTPOP ~ "{sum}",
                     DIABETES_CrudePrev ~ "{mean} ({sd})",
                     CHD_CrudePrev ~ "{mean} ({sd})",
                     STROKE_CrudePrev ~ "{mean} ({sd})"),
    label = list(`CVI Percentile Rank` ~ "Median CVI Percentile (IQR)",
                 ACS_MEDIAN_AGE ~ "Age",
                 E_TOTPOP ~ "Total Population",
                 DIABETES_CrudePrev ~ "Prevalence of Diabetes",
                 CHD_CrudePrev ~ "Prevalence of Coronary Heart Disease",
                 STROKE_CrudePrev ~ "Prevalence of Stroke"),
    digits = ACS_MEDIAN_AGE ~ 0
  ) %>% 
  add_p() %>% 
  add_q(method = "bonferroni") %>% 
  bold_labels() %>% 
  add_overall()
sd(MPdata4$`Number of active physicians per 10,000 people`)

#stacking tables over each other
stack16<-tbl_regression(MPglm16, exponentiate = TRUE)
stack16.1age<-tbl_regression(MPglm16.1age, exponentiate = TRUE)
tbl_stack(list(
  stack16,
  stack16.1age)
)

library(tidyverse)
library(gtsummary)

#fileriing our data for our boxplots
BoxPlotT2DM<-MPdata4 %>% 
  select(`Microplastics level in Ocean Water`,DIABETES_CrudePrev) %>% 
  filter(`Microplastics level in Ocean Water`=="Low"|
           `Microplastics level in Ocean Water`=="Very High") %>% 
  ggplot(mapping = aes(`Microplastics level in Ocean Water`,
                       DIABETES_CrudePrev),
         alpha(0.2)) +
  geom_boxplot(fill = "grey") +
  labs(
    title = "Prevalence of T2DM",
    subtitle = "In coastal counties stratified by concentration of MNPs",
    x = "Concentration of Microplastics",
    y = "Prevalence"
  ) +
  theme_classic()
#plot 1
MPdata4 %>% 
  ggplot(mapping = aes(`Microplastics level in Ocean Water`,
                       CHD_CrudePrev),
         alpha(0.2)) +
  geom_boxplot(fill = "grey") +
  labs(
    title = "Microplastics and CHD",
    x = "Microplastics class",
    y = "Prevalence"
  ) +
  theme_classic()

#plot 2
MPdata4 %>% 
  ggplot(mapping = aes(`Microplastics level in Ocean Water`,
                       BPHIGH_CrudePrev),
         alpha(0.2)) +
  geom_boxplot(fill = "grey") +
  labs(
    title = "Microplastics and HTN",
    x = "Microplastics class",
    y = "Prevalence"
  ) +
  theme_classic()

#plot 3
MPdata4 %>% 
  ggplot(mapping = aes(`Microplastics level in Ocean Water`,
                       DIABETES_CrudePrev),
         alpha(0.2)) +
  geom_boxplot(fill = "grey") +
  labs(
    title = "Microplastics and T2DM",
    x = "Microplastics class",
    y = "Prevalence"
  ) +
  theme_classic()

#plot 4
MPdata4 %>% 
  ggplot(mapping = aes(`Microplastics level in Ocean Water`,
                       `STROKE_CrudePrev`),
         alpha(0.2)) +
  geom_boxplot(fill = "grey") +
  labs(
    title = "Microplastics and Stroke",
    x = "Microplastics class",
    y = "Prevalence"
  ) +
  theme_classic()

table(MPdata4$`Microplastics level in Ocean Water`,
      MPdata4$`COASTLINE REGION`)

view(MPdata4)

#microplastics class naa 4 mathi 3 bhaag kariye
view(MPdata4)
MPdata4<-MPdata4 %>% 
  mutate(`Microplastics level in Ocean Water` =
           as.character(`Microplastics level in Ocean Water`)) %>% 
  mutate(`Microplastics level in Ocean Water` = 
           if_else(`Microplastics level in Ocean Water`=="Very High"|
                     `Microplastics level in Ocean Water`=="High",
                   "High","None")) %>% view()
MPdata4<-MPdata4 %>% 
  mutate(`Microplastics level in Ocean Water`=if_else(
    `Microplastics: Ocean Water`>=1,"High",
    if_else(`Microplastics: Ocean Water`>=0.005,"Medium",
            "Low") 
  ))
#making factors for microplastic ocean water class
MPdata4<-MPdata4 %>% 
  mutate(`Microplastics level in Ocean Water`=factor(
    `Microplastics level in Ocean Water`, levels = c("Low",
                                                     "Medium",
                                                     "High")))
view(MPdata4)

###
#running regression with 3 factored microplastics
###
MPglm11a.1age<-MPdata4 %>% 
  glm(CANCER_crprev ~ `Microplastics level in Ocean Water` +
        `Median Age` +
        `SVI Percentile` +
        `Number of active physicians per 10,000 people`,
      family = quasipoisson(link="logit"),
      weights = `Total Population of County`,
      data = .)
tbl_regression(MPglm11a.1age, exponentiate = TRUE)

MPglm12a.1age<-MPdata4 %>% 
  glm(`Prevalence of Hypertension` ~ `Microplastics level in Ocean Water` +
        `Median Age` +
        `SVI Percentile` +
        `Number of active physicians per 10,000 people` +
        Avg_percent_days_abovePM25,
      family = quasipoisson(link="logit"),
      weights = `Total Population of County`,
      data = .)
tbl_regression(MPglm12a.1age, exponentiate = TRUE)