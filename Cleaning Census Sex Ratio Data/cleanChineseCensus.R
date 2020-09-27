library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(haven)
library(foreign)

path <- "~/Desktop/Econ Research /Week 3"
setwd(path)
#raw DataSet
filename <- "Chinese_Data.xlsx"
Chinese_Data <- read_excel("Chinese_Data.xlsx", col_names = FALSE)
#transpose
Chinese_Data <- t(Chinese_Data)
Chinese_Data <- as.data.frame(Chinese_Data)
#I just need male and female values
Chinese_Data <- Chinese_Data %>% filter(V2 != "Total")
#names of regions
colnames(Chinese_Data) <- c("Age","Category", "National",
                            "Bejing",
                            "Tianjin",
                            "Hebei",
                            "Shanxi",
                            "Inner Mongolia",
                            "Liaoning",
                            "Jilin",
                            "Heilongjiang",
                            "Shanghai",
                            "Jiang Su",
                            "Zhejiang",
                            "Anhui",
                            "Fujian",
                            "Jiangxi",
                            "Shandong",
                            "Henan",
                            "Hubei",
                            "Hunan",
                            'Guangdong',
                            "Guangxi",
                            "Hainan",
                            "Chongqing",
                            "Sichuan",
                            "Guizhou",
                            "Yunnan",
                            "Tibet",
                            "Shaanxi",
                            "Gansu",
                            "Qinghai",
                            "Ningxia",
                            "Xinjiang")
#remove aggregate data
Chinese_Data <- Chinese_Data[-c(1,2),]
#repeat each row 5 times becuase the data is labeld as batches of 5, we will have to manally remove a few rows that we didnt need to replicate (0,1-4,100)
Chinese_Data <- Chinese_Data %>% slice(rep(1:n(), each = 5))
#remove excess rows for the ages that dont come in batches of 5
Chinese_Data <- Chinese_Data[-c(1:4),]
Chinese_Data <- Chinese_Data[-c(3:6),]
Chinese_Data <- Chinese_Data[-c(209:212),]
Chinese_Data <- Chinese_Data[-c(204:207),]
Chinese_Data <- Chinese_Data[-c(7:8),]
#a manual way to add a new column with indivdual ages, not age ranges, each individual values from the same rage will have the same gender count values 
Chinese_Data$Age <- c(0,0,c(1:4),c(1:4),c(5:9),c(5:9),c(10:14),c(10:14),c(15:19),c(15:19),c(20:24),c(20:24),c(25:29),c(25:29),c(30:34),c(30:34),c(35:39),c(35:39),c(40:44),c(40:44),c(45:49),c(45:49),c(50:54),c(50:54),c(55:59),c(55:59),c(60:64),c(60:64),c(65:69),c(65:69),c(70:74),c(70:74),c(75:79),c(75:79),c(80:84),c(80:84),c(85:89),c(85:89),c(90:94),c(90:94),c(95:99),c(95:99),100,100)
#sort the rows in the datagrame by ascending age 
Chinese_Data <- Chinese_Data %>% arrange(Age)


#take our big dataframe and make a new one where each region is stacked on top of each other (the format we've been using) with the columns region,age,male,female
df <- data.frame(region = as.character(),age = as.integer(), males = as.integer(), females = as.integer())
for(j in 3:ncol(Chinese_Data)){
for(i in 0:100){
region <- colnames(Chinese_Data)[j]
age <- i
males <- Chinese_Data[2 * i + 1,j]
females <- Chinese_Data[2 * i + 2,j]
df_i <- data.frame(region, age,males,females)
df <- rbind(df,df_i)
}
}
#make sure these are numeric so we can do calcualtions 
df$age <- as.numeric(df$age)
df$males <- as.numeric(df$males)
df$females <- as.numeric(df$females)
#add sex ratio variable 
data <- df %>% mutate(sexRatio = males / females)

# Make WVS region codes
# WVS region codes change. I use WVS Wave 6 code.
data$X048_WVS <- case_when(
  data$region=="Bejing"~156001,
  data$region=="Hebei"~156002,
  data$region=="Shanxi"~156003,
  data$region=="Liaoning"~156005,
  data$region=="Jilin"~156006,
  data$region=="Heilongjiang"~156007,
  data$region=="Shanghai"~156008,
  data$region=="Jiangsu"~156009,
  data$region=="Zhejiang"~156010,
  data$region=="Anhui"~156011,
  data$region=="Fujian"~156012,
  data$region=="Jiangxi"~156013,
  data$region=="Shandong"~156014,
  data$region=="Henan"~156015,
  data$region=="Hubei"~156016,
  data$region=="Hunan"~156017,
  data$region=="Guangdong"~156018,
  data$region=="Guangxi"~156019,
  data$region=="Sichuan"~156020,
  data$region=="Guizhou"~156021,
  data$region=="Shaannxi"~156024,
  data$region=="Chongqing"~156032,
  data$region=="Gansu"~156033,
  data$region=="Qinghai"~156034
  )

# Make ISSP region codes 
data$KR_REG <- case_when(
  data$region=="Bejing"~110000,
  data$region=="Tianjin"~120000,
  data$region=="Hebei"~130000,
  data$region=="Shanxi"~140000,
  data$region=="Inner Mongolia"~150000,
  data$region=="Liaoning"~210000,
  data$region=="Jilin"~220000,
  data$region=="Heilongjiang"~230000,
  data$region=="Shanghai"~310000,
  data$region=="Jiangsu"~320000,
  data$region=="Zhejiang"~330000,
  data$region=="Anhui"~340000,
  data$region=="Jujian"~350000,
  data$region=="Jiangxi"~360000,
  data$region=="Shandon"~370000,
  data$region=="Henan"~410000,
  data$region=="Hubei"~420000,
  data$region=="Hunan"~430000,
  data$region=="Guangdong"~440000,
  data$region=="Guangxi"~450000,
  data$region=="Chongqing"~500000,
  data$region=="Sichuan"~510000,
  data$region=="Guizhou"~520000,
  data$region=="Yunnan"~530000,
  data$region=="Shaanxi"~610000,
  data$region=="Gansu"~620000,
  data$region=="Qinhai"~630000,
  data$region=="Ningxia Hui"~640000,
  data$region=="Xinajing Uygur"~650000
  )

write.dta(data,file="ChineseCensus.dta")
