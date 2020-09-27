#constructWVSWorld
#filePath <- "WVS.dta"
constructWVS <- function(){
library(haven)
library(tidyverse)
fileName <- "WVS.dta"
#load dataset
WVS <- haven::read_dta(fileName, encoding = 'latin1')
#wave 6 data
WVS <- WVS %>% filter(S002 == 6)
#removes columns if all values are the same (columns from other waves)
WVS <- WVS %>% select_if(~ length(unique(.)) > 1)
#renaming potentially relevant variables
WVS <- WVS %>% rename(Cntry = S003, Married = X007, Age = X003, Sex = X001, ChildrenNum = X011, Educ = X025, LiveParents = X026, Employment = X028 )
WVS <- WVS %>% rename(wageEarner = X040, wageEarnerEmp = X041, FamSavings = X044, JustProstitution = F119, JustDivorce = F121, JustSexPreMar = F135A)
WVS <- WVS %>% rename(JustManBeatWife = F199, WomenSameRightMen = E233, MenJobs = C001, FinacialSatisfaction = C006, HousewifeFulfilling = D057)
WVS <- WVS %>% rename(menBeterLeader = D059, UniversityBoy = D060, preSchoolMother = D061, womenJobIndependen = D063_B, womenEarnMore = D066_B)
WVS <- WVS %>% rename(menBetterExec = D078)
#add sex ratio column
WVS <- WVS %>% add_column(sexratio12 = 1)
#update sex ratio column based on Cntry code hashtag is used to denote countries added, all sex ratios from wikipedia 
WVS <- WVS %>% mutate(sexratio12 = case_when(
  Cntry == 12 ~ 1.05, #Algeria
  Cntry == 31 ~ 1.06, #Azerberjan
  Cntry == 32 ~ 1.04,
  Cntry == 36 ~ 1.057,
  Cntry == 40 ~ 1.055,
  Cntry == 51 ~ 1.1, #Armenia
  Cntry == 76 ~ 1.05, #Brazil
  Cntry == 100 ~ 1.06,
  Cntry == 112 ~ 1.05, #Belarus
  Cntry == 124 ~ 1.054,
  Cntry == 152 ~ 1.04,
  Cntry == 156 ~ 1.15,
  Cntry == 158 ~ 0,
  Cntry == 170 ~ 1.06, #Colombia
  Cntry == 191 ~ 1.07,
  Cntry == 196 ~ 1.05, #Cyprus
  Cntry == 203 ~ 1.058,
  Cntry == 208 ~ 1.056,
  Cntry == 218 ~ 1.05, #Ecuador
  Cntry == 233 ~ 1.05, #Estonia
  Cntry == 246 ~ 1.047,
  Cntry == 250 ~ 1.047,
  Cntry == 268 ~ 1.05, #Georgia
  Cntry == 275 ~ 0, #Palestine
  Cntry == 276 ~ 1.05, #Germany
  Cntry == 288 ~ 1.03, #Ghana
  Cntry == 332 ~ 1.01, #Haiti
  Cntry == 344 ~ 1.06, #Hong Kong
  Cntry == 348 ~ 1.053,
  Cntry == 352 ~ 1.053,
  Cntry == 356 ~ 1.099,
  Cntry == 368 ~ 1.05, # Iraq
  Cntry == 372 ~ 1.06,
  Cntry == 376 ~ 1.055,
  Cntry == 392 ~ 1.056,
  Cntry == 398 ~ .94, #Kazakhstan
  Cntry == 400 ~ 1.06, #Jordan
  Cntry == 410 ~ 1.057,
  Cntry == 414 ~ 1.05, #Kuwait
  Cntry == 417 ~ 1.07, #Kyrgyzstan
  Cntry == 422 ~ 1.05, #Lebanon
  Cntry == 428 ~ 1.059,
  Cntry == 434 ~ 1.05, #Libya
  Cntry == 440 ~ 1.052,
  Cntry == 458 ~ 1.07, #Malaysia
  Cntry == 484 ~ 1.05,
  Cntry == 504 ~ 1.05, #Morocco
  Cntry == 528 ~ 1.055,
  Cntry == 554 ~ 1.05, #New Zealand
  Cntry == 566 ~ 1.06, #Nigeria
  Cntry == 578 ~ 1.054,
  Cntry == 586 ~ 1.05, #Pakistan
  Cntry == 604 ~ 1.05, #Peru
  Cntry == 608 ~ 1.06,
  Cntry == 616 ~ 1.056,
  Cntry == 634 ~ 1.02, #Qatar
  Cntry == 642 ~ 1.06, #Romania
  Cntry == 643 ~ 1.059,
  Cntry == 646 ~ 1.03, #Rwanda
  Cntry == 702 ~ 1.07, #Singapore
  Cntry == 703 ~ 1.05,
  Cntry == 705 ~ 1.058,
  Cntry == 710 ~ 1.031,
  Cntry == 716 ~ 1.03, #Zimbabwe
  Cntry == 724 ~ 1.064,
  Cntry == 752 ~ 1.058,
  Cntry == 756 ~ 1.052,
  Cntry == 764 ~ 1.05, #Thailand
  Cntry == 780 ~ 1.03, #Trinidad
  Cntry == 788 ~ 1.06, #Tunisia
  Cntry == 792 ~ 1.05,
  Cntry == 804 ~ 1.06, #Ukraine
  Cntry == 818 ~ 1.06, #Egypt
  Cntry == 840 ~ 1.048,
  Cntry == 858 ~ 1.04, #Uruguay
  Cntry == 860 ~ 1.06, #Uzbekistan
  Cntry == 862 ~ 1.05,
  Cntry == 887 ~ 1.05, #Yemen
  Cntry == 5601 ~ 1.05,
  Cntry == 5602 ~ 1.05,
  Cntry == 5603 ~ 1.05,
  Cntry == 27601 ~ 1.053,
  Cntry == 27602 ~ 1.053,
  Cntry == 82601 ~ 1.051))
#create cnlab column
WVS <- WVS %>% add_column(cnlab = 1)
#update cnlab column based on Cntry code
WVS <- WVS %>% mutate(cnlab = case_when(
  Cntry == 12 ~ "AL", #Algeria
  Cntry == 31 ~ "AZ", #Azerberjan
  Cntry == 32 ~ "AR",
  Cntry == 36 ~ "AU",
  Cntry == 40 ~ "AT",
  Cntry == 51 ~ "AM", #Armenia
  Cntry == 76 ~ "BZ", #Brazil
  Cntry == 100 ~ "BG",
  Cntry == 112 ~ "BL", #Belarus
  Cntry == 124 ~ "CA",
  Cntry == 152 ~ "CL",
  Cntry == 156 ~ "CN",
  Cntry == 158 ~ "TW" ,
  Cntry == 170 ~ "CO", #Colombia
  Cntry == 191 ~ "HR",
  Cntry == 196 ~ "CY", #Cyprus
  Cntry == 203 ~ "CZ",
  Cntry == 208 ~ "DK",
  Cntry == 218 ~ "EC", #Ecuador
  Cntry == 233 ~ "ET", #Estonia
  Cntry == 246 ~ "FI",
  Cntry == 250 ~ "FR",
  Cntry == 268 ~ "GR", #Georgia
  Cntry == 275 ~ "PA", #Palestine
  Cntry == 276 ~ "GE", #Germany
  Cntry == 288 ~ "GA", #Ghana
  Cntry == 332 ~ "HT", #Haiti
  Cntry == 344 ~ "HK", #Hong Kong
  Cntry == 348 ~ "HU",
  Cntry == 352 ~ "IS",
  Cntry == 356 ~ "IN",
  Cntry == 368 ~ "IQ", # Iraq
  Cntry == 372 ~ "IE",
  Cntry == 376 ~ "IL",
  Cntry == 392 ~ "JP",
  Cntry == 398 ~ "KZ", #Kazakhstan
  Cntry == 400 ~ "JR", #Jordan
  Cntry == 410 ~ "KR",
  Cntry == 414 ~ "KU", #Kuwait
  Cntry == 417 ~ "KY", #Kyrgyzstan
  Cntry == 422 ~ "LB", #Lebanon
  Cntry == 428 ~ "LV",
  Cntry == 434 ~ "LI", #Libya
  Cntry == 440 ~ "LT",
  Cntry == 458 ~ "ML", #Malaysia
  Cntry == 484 ~ "MX",
  Cntry == 504 ~ "MO", #Morocco
  Cntry == 528 ~ "NL",
  Cntry == 554 ~ "NZ", #New Zealand
  Cntry == 566 ~ "NG", #Nigeria
  Cntry == 578 ~ "NO",
  Cntry == 586 ~ "PK", #Pakistan
  Cntry == 604 ~ "PR", #Peru
  Cntry == 608 ~ "PH",
  Cntry == 616 ~ "PL",
  Cntry == 634 ~ "QR", #Qatar
  Cntry == 642 ~ "RO", #Romania
  Cntry == 643 ~ "RU",
  Cntry == 646 ~ "RW", #Rwanda
  Cntry == 702 ~ "SG", #Singapore
  Cntry == 703 ~ "SK",
  Cntry == 705 ~ "SI",
  Cntry == 710 ~ "ZA",
  Cntry == 716 ~ "ZW", #Zimbabwe
  Cntry == 724 ~ "ES",
  Cntry == 752 ~ "SE",
  Cntry == 756 ~ "CH",
  Cntry == 764 ~ "TL", #Thailand
  Cntry == 780 ~ "TN", #Trinidad
  Cntry == 788 ~ "TU", #Tunisia
  Cntry == 792 ~ "TR",
  Cntry == 804 ~ "UK", #Ukraine
  Cntry == 818 ~ "EG", #Egypt
  Cntry == 840 ~ "US",
  Cntry == 858 ~ "UR", #Uruguay
  Cntry == 860 ~ "UZ", #Uzbekistan
  Cntry == 862 ~ "VE",
  Cntry == 887 ~ "YE", #Yemen
  Cntry == 5601 ~ "BE",
  Cntry == 5602 ~ "BE",
  Cntry == 5603 ~ "BE",
  Cntry == 27601 ~ "DE",
  Cntry == 27602 ~ "DE",
  Cntry == 82601 ~ "GB"))

#take only the columns listed
WVS <- WVS %>%
  select(Cntry,Married,Age,Sex,ChildrenNum,Educ,LiveParents,Employment,wageEarner,wageEarnerEmp,FamSavings,JustProstitution,JustDivorce,JustSexPreMar
         ,JustManBeatWife,WomenSameRightMen,MenJobs,FinacialSatisfaction,HousewifeFulfilling,menBeterLeader,UniversityBoy,preSchoolMother,womenJobIndependen
         ,womenEarnMore,menBetterExec,sexratio12,cnlab)
write.csv(WVS,"WVS.csv", row.names = TRUE)
}

