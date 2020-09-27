#cleanWVS
cleanWVS <- function(){
library(dplyr)
file <- "WVS.csv"
WVS<- read.csv(file)
#remove negative value indicating lack of an answer for any reason
WVS <- WVS %>%mutate_all(~replace(., .<0, NA))

#recode so that higher num implies more tradiitonal gender view
#scale from 1-10
WVS <- WVS %>% mutate_at(vars(JustProstitution,JustDivorce,JustSexPreMar,WomenSameRightMen),~11-.)
#scale from 1-3
WVS <- WVS %>% mutate_at(vars(MenJobs,womenEarnMore),~4-.) 
#scale from 1-4
WVS <- WVS %>% mutate_at(vars(HousewifeFulfilling,menBeterLeader,menBetterExec,UniversityBoy,preSchoolMother),~5-.) 

#replace any remaining strings with NA
WVS <- WVS %>% mutate_at(vars(JustProstitution,JustDivorce,JustSexPreMar,JustManBeatWife,WomenSameRightMen,MenJobs,womenEarnMore,HousewifeFulfilling,menBeterLeader,menBetterExec,UniversityBoy,preSchoolMother),~as.numeric(as.character(.)))


#standardize

#calculate mean and sd for each important column
mean <- WVS %>% summarise_at(vars(MenJobs,womenEarnMore,HousewifeFulfilling,menBeterLeader,menBetterExec,UniversityBoy,preSchoolMother),~mean(.x,na.rm = TRUE))
std <- WVS %>% summarise_at(vars(MenJobs,womenEarnMore,HousewifeFulfilling,menBeterLeader,menBetterExec,UniversityBoy,preSchoolMother),~sd(.x,na.rm = TRUE))

#add new columns where the SD for each row will go
WVS <- WVS %>% add_column(JustProstitution_std = 0,JustDivorce_std = 0,JustSexPreMar_std = 0,JustManBeatWife_std = 0,WomenSameRightMen_std = 0,MenJobs_std = 0,womenEarnMore_std = 0,HousewifeFulfilling_std = 0,menBeterLeader_std = 0,menBetterExec_std = 0,UniversityBoy_std = 0,preSchoolMother_std = 0)

#adding individual SD for each variable 
WVS <- WVS %>% rowwise() %>% mutate(MenJobs_std = (MenJobs - mean$MenJobs)/std$MenJobs)
WVS <- WVS %>% rowwise() %>% mutate(womenEarnMore_std = (womenEarnMore - mean$womenEarnMore)/std$womenEarnMore)
WVS <- WVS %>% rowwise() %>% mutate(HousewifeFulfilling_std = (HousewifeFulfilling - mean$HousewifeFulfilling)/std$HousewifeFulfilling)
WVS <- WVS %>% rowwise() %>% mutate(menBeterLeader_std = (menBeterLeader - mean$menBeterLeader)/std$menBeterLeader)
WVS <- WVS %>% rowwise() %>% mutate(menBetterExec_std = (menBetterExec - mean$menBetterExec)/std$menBetterExec)
WVS <- WVS %>% rowwise() %>% mutate(UniversityBoy_std = (UniversityBoy - mean$UniversityBoy)/std$UniversityBoy)
WVS <- WVS %>% rowwise() %>% mutate(preSchoolMother_std = (preSchoolMother - mean$preSchoolMother)/std$preSchoolMother)

#create gender ideology index
#setting na.rm = FALSE means that if any of these roww contain NA the sum is NA
Sum2 <- WVS %>% select(MenJobs_std,womenEarnMore_std,HousewifeFulfilling_std,menBeterLeader_std,menBetterExec_std,UniversityBoy_std,preSchoolMother_std) %>% rowSums(na.rm=FALSE)
Sum2 <- as.data.frame(Sum2)
WVS <- WVS %>% add_column(Sum2)

#removing potentailly irrelevant columns with lots of NAs, Prostitution, Sex_Pre_Marrige,Divrce, Housewife,
Sum3 <- WVS %>% select(HousewifeFulfilling_std,MenJobs_std,womenEarnMore_std,menBeterLeader_std,menBetterExec_std,UniversityBoy_std,preSchoolMother_std) %>% rowSums(na.rm=FALSE)
Sum3 <- as.data.frame(Sum3)
WVS <- WVS %>% add_column(Sum3)

mean_Gen <- mean(WVS$Sum3,na.rm = TRUE)
std_Gen <- sd(WVS$Sum3,na.rm = TRUE)
WVS <- WVS %>% add_column(GenInd_std = 0)
WVS <- WVS %>% rowwise() %>% mutate(GenInd_std = (Sum3 - mean_Gen)/std_Gen)

write.csv(WVS,file, row.names = TRUE)


}












