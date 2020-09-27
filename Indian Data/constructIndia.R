
constructIndia <- function(){
library(haven)
library(tidyverse)
library(readxl)

fileNameSex <- "IndianCensus2011.dta"
surveyFile <- "WVS.dta"
#chose either 30 or 0. Chosing 30 will reveal the gender prefrence of the respondent. When 0 is chosen we see the gender ratio at the birth year of respondent 
childBearing <- 20
#Import Data set, from indoan 2011 census
IndianCensus <- haven::read_dta(fileNameSex, encoding = 'latin1')
IndianCensus <- IndianCensus %>% select(state,sexratio,byear,rural_male,rural_female,urban_male,urban_female)
IndianCensus <- IndianCensus %>% rename(State = state)
IndianCensus <- IndianCensus %>% rename(childBearingYear = byear)
IndianCensus <- IndianCensus %>% mutate(sexRatioRural = rural_male / rural_female)
IndianCensus <- IndianCensus %>% mutate(sexRatioUrban = urban_male / urban_female) 
#Import Child Sex Ratio Data
dataFile <- "India_Child_Sex_Ratio.csv"
ChildRatio <- read.csv(dataFile)
ChildRatio <- ChildRatio %>%  mutate (childSexRatio2011 = 1000 / numGirls2011)
ChildRatio <- ChildRatio %>%  mutate (childSexRatio2001 = 1000 / numGirls2001)
ChildRatio <- ChildRatio %>%  select(State,childSexRatio2011,childSexRatio2001)
#Import WVS
WVS <- haven::read_dta(surveyFile, encoding = 'latin1')
#wave 6 data
WVS <- WVS %>% filter(S002 == 6)
#India
WVS <- WVS %>% filter(S003 == 356)
#removes columns if all values are the same (columns from other waves)
WVS <- WVS %>% select_if(~ length(unique(.)) > 1)

WVS <- WVS %>%  mutate(State = case_when(
  X048WVS == 356001 ~ "Andhra Pradesh",
  X048WVS == 356002 ~ "Assam",
  X048WVS == 356003 ~ "Bihar",
  X048WVS == 356004 ~ "Chhattisgarh",
  X048WVS == 356005 ~ "Delhi",
  X048WVS == 356006 ~ "Gujarat",
  X048WVS == 356007 ~ "Haryana",
  X048WVS == 356008 ~ "Jharkhand",
  X048WVS == 356009 ~ "Karnataka",
  X048WVS == 356010 ~ "Kerala",
  X048WVS == 356011 ~ "Madhya Pradesh",
  X048WVS == 356012 ~ "Maharastra",
  X048WVS == 356013 ~ "Orissa",
  X048WVS == 356014 ~ "Punjab",
  X048WVS == 356015 ~ "Rajasthan",
  X048WVS == 356016 ~ "Tamil Nadu",
  X048WVS == 356017 ~ "Uttar Pradesh",
  X048WVS == 356018 ~ "West Bengal",
  X048WVS == 356019 ~ "Uttarakhand",
  X048WVS == 356021 ~ "Himachal Pradesh",
  X048WVS == 356022 ~ "Jammu & Kashmir",
  X048WVS == 356023 ~ "Manipur",
  X048WVS == 356024 ~ "Tripura",
  X048WVS == 356025 ~ "Chattisgarh",
  X048WVS == 356026 ~ "NCT of Delhi",
  X048WVS == 356027 ~ "Puducherry"
))

WVS <- WVS %>%  mutate(StateLabel = case_when(
  X048WVS == 356001 ~ "AP",
  X048WVS == 356002 ~ "AS",
  X048WVS == 356003 ~ "BH",
  X048WVS == 356004 ~ "CG",
  X048WVS == 356005 ~ "DE",
  X048WVS == 356006 ~ "GJ",
  X048WVS == 356007 ~ "HY",
  X048WVS == 356008 ~ "JK",
  X048WVS == 356009 ~ "KT",
  X048WVS == 356010 ~ "KR",
  X048WVS == 356011 ~ "MP",
  X048WVS == 356012 ~ "MH",
  X048WVS == 356013 ~ "OR",
  X048WVS == 356014 ~ "PJ",
  X048WVS == 356015 ~ "RJ",
  X048WVS == 356016 ~ "TN",
  X048WVS == 356017 ~ "UP",
  X048WVS == 356018 ~ "WB",
  X048WVS == 356019 ~ "UT",
  X048WVS == 356021 ~ "HP",
  X048WVS == 356022 ~ "J&K",
  X048WVS == 356023 ~ "MR",
  X048WVS == 356024 ~ "TP",
  X048WVS == 356025 ~ "CH",
  X048WVS == 356026 ~ "NCT",
  X048WVS == 356027 ~ "PC"
))

WVS <- WVS %>%  mutate(StateCode = case_when(
  X048WVS == 356001 ~ 28,
  X048WVS == 356002 ~ 18,
  X048WVS == 356003 ~ 10,
  X048WVS == 356004 ~ 22,
  X048WVS == 356005 ~ 07,
  X048WVS == 356006 ~ 24,
  X048WVS == 356007 ~ 06,
  X048WVS == 356008 ~ 20,
  X048WVS == 356009 ~ 29,
  X048WVS == 356010 ~ 32,
  X048WVS == 356011 ~ 23,
  X048WVS == 356012 ~ 27,
  X048WVS == 356013 ~ 21,
  X048WVS == 356014 ~ 03,
  X048WVS == 356015 ~ 08,
  X048WVS == 356016 ~ 33,
  X048WVS == 356017 ~ 09,
  X048WVS == 356018 ~ 19,
  X048WVS == 356019 ~ 05,
  X048WVS == 356021 ~ 02,
  X048WVS == 356022 ~ 01,
  X048WVS == 356023 ~ 14,
  X048WVS == 356024 ~ 16,
  X048WVS == 356025 ~ 22,
  X048WVS == 356026 ~ 07,
  X048WVS == 356027 ~ 34
))

WVS <- WVS %>% rename(byear = X002, Married = X007, Age = X003, Sex = X001, ChildrenNum = X011, Educ = X025, LiveParents = X026, Employment = X028 )
WVS <- WVS %>% rename(wageEarner = X040, wageEarnerEmp = X041, FamSavings = X044, JustProstitution = F119, JustDivorce = F121, JustSexPreMar = F135A)
WVS <- WVS %>% rename(JustManBeatWife = F199, WomenSameRightMen = E233, MenJobs = C001, FinacialSatisfaction = C006, HousewifeFulfilling = D057)
WVS <- WVS %>% rename(menBeterLeader = D059, UniversityBoy = D060, preSchoolMother = D061, womenJobIndependen = D063_B, womenEarnMore = D066_B)
WVS <- WVS %>% rename(menBetterExec = D078,townSize = X049)

WVS <- WVS %>% mutate(childBearingYear = byear + childBearing)

WVS <- WVS %>%
  select(StateLabel,childBearingYear,byear,Married,Age,Sex,ChildrenNum,Educ,LiveParents,Employment
         ,WomenSameRightMen,MenJobs,FinacialSatisfaction,HousewifeFulfilling,menBeterLeader,UniversityBoy,preSchoolMother,womenJobIndependen
         ,womenEarnMore,menBetterExec,State,StateCode,townSize,ChildrenNum)
#Join WVS with birth rate data by age and region
WVS <- left_join(WVS,IndianCensus,by = c("childBearingYear","State"))
WVS <- left_join(WVS,ChildRatio, by = c("State"))
write.csv(WVS,"WVS.csv", row.names = TRUE)
}
