rootdir <- "~/Desktop/Econ Research /Week 3"
setwd(rootdir)

library(tidyverse)
library(dplyr)
library(pracma)
library(readxl)
library(stringr)
library(foreign)

filename <- "Australia_Sex_Ratio.xlsx"
data <- read_excel(filename)
colnames(data) <- c("region","age","males","females")

#remove periodic rows summarizing past 5 rows (these rows contain hyphens)
data<-data[!grepl("-", data$age),]
#create birth year variable 
data$byear = 2011-as.numeric(data$age)
#create sex ratio variable
data$sexratio <- data$males / data$females
#create sex ratio to show sex preferce of parents at 30
data$sexratio_a30 <- c()
mnyear <- min(data$byear)
mxyear <- max(data$byear)-30
for (j in mnyear:mxyear){
  data[(data$byear==j),"sexratio_a30"] <-data$sexratio[(data$byear==(j+30))] 
}


# Make WVS region codes
# WVS region codes change. I use WVS Wave 6 code.
data$X048_WVS <- case_when(
  data$region=="New South Wales"~36009,
  data$region=="Queensland"~36004,
  data$region=="South Australia"~36005,
  data$region=="Tasmania"~36007,
  data$region=="Victoria"~36003,
  data$region=="Western Australia"~36006,
  data$region=="NSW/ACT"~36002)

# Make ISSP region codes 
data$KR_REG <- case_when(
  data$region=="New South Wales"~1,
  data$region=="Queensland"~3,
  data$region=="South Australia"~4,
  data$region=="Tasmania"~6,
  data$region=="Victoria"~2,
  data$region=="Western Australia"~5,
  data$region=="NSW/ACT"~7)

write.dta(data,file="AustraliaCensus.dta")