
describeIndia <- function(){

library(patchwork)
library(dplyr)
library(ggplot2)
  
file <- "WVS.csv"
WVS<- read.csv(file)

#the sex ratio variable can be changed to reflect urban, rural or overall population. Use the filter option to only take responses from those who live in rural areas. Rural area is defined by townSize < 2. Population columns are sexratio, sexRatioRural,sexRatioUrban 
summarizedData <- WVS %>% select(Sum,State,sexratio,GenInd_std,StateLabel) %>% group_by(State,StateLabel) %>% summarise(mean_genind = mean(GenInd_std, na.rm = TRUE), sexratio = mean(sexratio, na.rm = TRUE))
#create a regression. Y=Gender index X=Sex ratio
regressionTotal <- lm(mean_genind ~ sexratio, data = summarizedData)
#access intercept from regression object
int <- regressionTotal$coefficients[1]
int <- format(round(int, 2), nsmall = 2)
#access slope from regression object
slope <- regressionTotal$coefficients[2]
slope <- format(round(slope, 2), nsmall = 2)
#access standard error from regression object
se <- summary(regressionTotal)
se <- se[[4]][4]
se <- format(round(se, 2), nsmall = 2)
#a string to attach to our graph
equation1 <- paste("Beta = ", slope,"SE = ",se)

p1 <- ggplot(summarizedData, aes(x=sexratio, y=mean_genind))  +  geom_text(label=summarizedData$StateLabel) + annotate(geom="text", x=1.05, y=1.5, label=equation1,
                                                                                                                color="black") + geom_smooth(method='lm',se=FALSE) + theme_bw() + xlab("Sex Ratio (Boy per Girl)") + ylab("Gender Index")
p1

#remove any outliers
summarizedDataExcluding <- WVS %>% select(Sum,State,sexratio,GenInd_std,StateLabel) %>% filter(State != "Kerala", State != "Delhi") %>%  group_by(State,StateLabel) %>% summarise(mean_genind = mean(GenInd_std, na.rm = TRUE), sexratio = mean(sexratio,na.rm = TRUE))
#create a regression. Y=Gender index X=Sex ratio
regressionTotalExcluding <- lm(mean_genind ~ sexratio, data = summarizedDataExcluding)
#access intercept from regression object
int2 <- regressionTotalExcluding$coefficients[1]
int2<- format(round(int2, 2), nsmall = 2)
#access slope from regression object
slope2 <- regressionTotalExcluding$coefficients[2]
slope2 <- format(round(slope2, 2), nsmall = 2)
#access standard error from regression object
se2 <- summary(regressionTotalExcluding)
se2 <- se2[[4]][4]
se2 <- format(round(se2, 2), nsmall = 2)
#a string to attach to our graph
equation2 <- paste("Beta w/o Outliers = ", slope2,"SE = ",se2)

p2 <- ggplot(summarizedDataExcluding, aes(x=sexratio, y=mean_genind))  + geom_text(label=summarizedDataExcluding$StateLabel) + annotate(geom="text", x=1.05, y=1.5, label=equation2,
                                                                                        color="black") + geom_smooth(method='lm',se=FALSE) + theme_bw() + xlab("Sex Ratio (Boy per Girl)") + ylab("Gender Index")
p2


#regress individual gender index variables against sexratio
variableOfInterest <- c("MenJobs_std","womenEarnMore_std","HousewifeFulfilling_std","menBeterLeader_std","menBetterExec_std","UniversityBoy_std","preSchoolMother_std")

plot_list = list()
for(i in 1:7){
  summarizedData <- WVS %>% select(variableOfInterest[i],State,sexRatioRural,StateLabel,townSize,ChildrenNum) %>% filter(townSize < 2,ChildrenNum != 0) %>% group_by(State,StateLabel) %>% summarise(count = n(),mean_var = mean(.data[[variableOfInterest[i]]], na.rm = TRUE), sexratio = mean(sexRatioRural,na.rm = TRUE))
  summarizedData <- summarizedData %>% filter(count >= 30)
  regressionTotal <- lm(mean_var ~ sexratio, data = summarizedData)
  int <- regressionTotal$coefficients[1]
  int <- format(round(int, 2), nsmall = 2)
  slope <- regressionTotal$coefficients[2]
  slope <- format(round(slope, 2), nsmall = 2)
  se <- summary(regressionTotal)
  se <- se[[4]][4]
  se <- format(round(se, 2), nsmall = 2)
  equation1 <- paste("Beta = ", slope,"SE = ",se)
  p <- ggplot(summarizedData, aes(x=sexratio, y=mean_var))  + geom_text(label=summarizedData$StateLabel) + annotate(geom="text", x=1.05, y=1.5, label=equation1,
                                                                                          color="black") + geom_smooth(method='lm',se=FALSE) + theme_bw() + xlab("Sex Ratio (Boy per Girl)") + ylab(variableOfInterest[i])
  plot_list[[i]] = p
  #ggsave("IndiaPlot_Individual",p)
}

for (i in 1:7) {
  fileName <- paste("India_var_Rural_ExcludingSmall_AndNonParents",i,".pdf")
  ggsave(fileName,plot_list[[i]])
}

#Regression and graph for Rural places using sex ratio by year for rural places 
summarizedDataRural <- WVS %>% filter(townSize < 2, ChildrenNum != 0 ) %>% group_by(State,StateLabel) %>% summarise(count = n(),sexRatioRural = mean(sexRatioRural, na.rm = TRUE),mean_genindRural = mean(GenInd_std, na.rm = TRUE))
summarizedDataRural <- summarizedDataRural %>% filter(count >= 30)
regressionTotal <- lm(mean_genindRural ~ sexRatioRural, data = summarizedDataRural)
int <- regressionTotal$coefficients[1]
int <- format(round(int, 2), nsmall = 2)
slope <- regressionTotal$coefficients[2]
slope <- format(round(slope, 2), nsmall = 2)
se <- summary(regressionTotal)
se <- se[[4]][4]
se <- format(round(se, 2), nsmall = 2)
equation1 <- paste("Beta = ", slope,"SE = ",se)

p4 <- ggplot(summarizedDataRural, aes(x=sexRatioRural, y=mean_genindRural))  +  geom_text(label=summarizedDataRural$StateLabel) + annotate(geom="text", x=1.05, y=1.5, label=equation1,
                                                                                                                       color="black") + geom_smooth(method='lm',se=FALSE) + theme_bw() + xlab("Sex Ratio (Boy per Girl)") + ylab("Gender Index") + ggtitle("Rural excluding small samples and non-parents")
p4

#Regression and graph for Urban places usuing sex ratio by year for urban places 
summarizedDataUrban <- WVS %>% filter(townSize >= 2,ChildrenNum != 0) %>% group_by(State,StateLabel) %>% summarise(count = n(),sexRatioUrban = mean(sexRatioUrban, na.rm = TRUE),mean_genindUrban = mean(GenInd_std, na.rm = TRUE))
summarizedDataUrban <- summarizedDataUrban %>% filter(count >= 30)
regressionTotal <- lm(mean_genindUrban ~ sexRatioUrban, data = summarizedDataUrban)
int <- regressionTotal$coefficients[1]
int <- format(round(int, 2), nsmall = 2)
slope <- regressionTotal$coefficients[2]
slope <- format(round(slope, 2), nsmall = 2)
se <- summary(regressionTotal)
se <- se[[4]][4]
se <- format(round(se, 2), nsmall = 2)
equation1 <- paste("Beta = ", slope,"SE = ",se)

p5 <- ggplot(summarizedDataUrban, aes(x=sexRatioUrban, y=mean_genindUrban))  +  geom_text(label=summarizedDataUrban$StateLabel) + annotate(geom="text", x=1.05, y=1.5, label=equation1,
                                                                                                                       color="black") + geom_smooth(method='lm',se=FALSE) + theme_bw() + xlab("Sex Ratio (Boy per Girl)") + ylab("Gender Index") + ggtitle("Urban excluding small samples and non-parents") 
                                                                                                                                                                                                                                
p5
#Regression and graph for urban places using one child sex ratio for 2001
summarizedDataUrban_ChildRatio <- WVS %>% filter(townSize >= 2) %>% group_by(State,StateLabel) %>% summarise(count = n(),sexRatio = mean(childSexRatio2001, na.rm = TRUE),mean_genind = mean(GenInd_std, na.rm = TRUE))
regressionTotal <- lm(mean_genind ~ sexRatio, data = summarizedDataUrban)
int <- regressionTotal$coefficients[1]
int <- format(round(int, 2), nsmall = 2)
slope <- regressionTotal$coefficients[2]
slope <- format(round(slope, 2), nsmall = 2)
se <- summary(regressionTotal)
se <- se[[4]][4]
se <- format(round(se, 2), nsmall = 2)
equation1 <- paste("Beta = ", slope,"SE = ",se)

p6 <- ggplot(summarizedDataUrban, aes(x=sexRatio, y=mean_genind))  +  geom_text(label=summarizedDataUrban$StateLabel) + annotate(geom="text", x=1.05, y=1.5, label=equation1,
                                                                                                                                 color="black") + geom_smooth(method='lm',se=FALSE) + theme_bw() + xlab("Sex Ratio (Boy per Girl)") + ylab("Gender Index") + ggtitle("Urban Excluding Small Samples") 

p6

#Regression and graph for rural places using one child sex ratio for 2001
summarizedDataRural_ChildRatio <- WVS %>% filter(townSize < 2) %>% group_by(State,StateLabel) %>% summarise(count = n(),sexRatio = mean(childSexRatio2001, na.rm = TRUE),mean_genind = mean(GenInd_std, na.rm = TRUE))
regressionTotal <- lm(mean_genind ~ sexRatio, data = summarizedDataRural)
int <- regressionTotal$coefficients[1]
int <- format(round(int, 2), nsmall = 2)
slope <- regressionTotal$coefficients[2]
slope <- format(round(slope, 2), nsmall = 2)
se <- summary(regressionTotal)
se <- se[[4]][4]
se <- format(round(se, 2), nsmall = 2)
equation1 <- paste("Beta = ", slope,"SE = ",se)

p7 <- ggplot(summarizedDataRural, aes(x=sexRatio, y=mean_genind))  +  geom_text(label=summarizedDataRural$StateLabel) + annotate(geom="text", x=1.05, y=1.5, label=equation1,
                                                                                                                                 color="black") + geom_smooth(method='lm',se=FALSE) + theme_bw() + xlab("Sex Ratio (Boy per Girl)") + ylab("Gender Index") + ggtitle("Rural Excluding Small Samples")
p7

ggsave("WVS_plot_India.pdf",p1)
ggsave("WVS_plot_India_wo_outliers.pdf",p2)
#write.table(summarizedData, "India_table.txt")
}
