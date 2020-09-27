
describeWVS <- function(){

library(patchwork)
library(dplyr)
library(ggplot2)
file <- "WVS.csv"
WVS<- read.csv(file)
#dataframe with all countries except TW and PA(sex ratio = 0)
summarizedData <- WVS %>% select(GenInd_std,cnlab,sexratio12) %>% filter(cnlab != "TW" & cnlab != "PA") %>% group_by(cnlab) %>% summarise(mean_genind = mean(GenInd_std, na.rm = TRUE), sexRatio = mean(sexratio12))
#dataframe with all countries except outliers (IN,CN,TR,AM) along with TW and PA from above
summarizedDataNoOutliers <- WVS %>% select(GenInd_std,cnlab,sexratio12) %>% filter(cnlab != "IN" & cnlab != "CN" & cnlab != "TR" & cnlab != "TW" & cnlab != "PA" & cnlab != "AM" & cnlab != "KZ") %>% group_by(cnlab) %>% summarise(mean_genind = mean(GenInd_std, na.rm = TRUE), sexRatio = mean(sexratio12))

#regression for all countries 
regressionTotal <- lm(mean_genind ~ sexRatio, data = summarizedData)
int <- regressionTotal$coefficients[1]
int <- format(round(int, 2), nsmall = 2)
slope <- regressionTotal$coefficients[2]
slope <- format(round(slope, 2), nsmall = 2)
#access standard error from regression object
se <- summary(regressionTotal)
se <- se[[4]][4]
se <- format(round(se, 2), nsmall = 2)
#a string to attach to our graph
equation1 <- paste("Beta = ", slope,"SE = ",se)


#regression excluding outliers
regressionTotalExcluding <- lm(mean_genind ~ sexRatio, data = summarizedDataNoOutliers)
intExcluding <- regressionTotalExcluding$coefficients[1]
intExcluding <- format(round(intExcluding, 2), nsmall = 2)
slopeExcluding <- regressionTotalExcluding$coefficients[2]
slopeExcluding <- format(round(slopeExcluding, 2), nsmall = 2)
#access standard error from regression object
seExcluding <- summary(regressionTotalExcluding)
seExcluding <- seExcluding[[4]][4]
seExcluding <- format(round(seExcluding, 2), nsmall = 2)
#a string to attach to our graph
equation2 <- paste("Beta w/o outliers = ", slopeExcluding, "SE = ",seExcluding)


#plot all countries 
p1 <- ggplot(summarizedData, aes(x=sexRatio, y=mean_genind)) + geom_text(label=summarizedData$cnlab) + annotate(geom="text", x=.98, y=1.2, label=equation1,
              color="red") + geom_smooth(method='lm',se=FALSE) + theme_bw() + xlab("Sex Ratio (Boy per Girl)") + ylab("Gender Index")
p1
#plot without outliers
p2 <- ggplot(summarizedDataNoOutliers, aes(x=sexRatio, y=mean_genind)) + geom_text(label=summarizedDataNoOutliers$cnlab) + geom_smooth(method='lm',se=FALSE) + annotate(geom="text", x=1.03, y=1.2, label=equation2,
              color="red") + theme_bw() + xlab("Sex Ratio (Boy per Girl)") + ylab("Gender Index")
p2

ggsave("WVS_plot.pdf",p1)
ggsave("WVS_plot_wo_outliers.pdf",p2)

}

