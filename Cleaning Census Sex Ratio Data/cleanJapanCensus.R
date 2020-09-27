#Differnt Regions in Japan as defined by ISSP and WVS 
#Hokkaido <- c("Hokkaido")
#Tohoku <- c("Akita","Aomori","Fukushima","Iwate","Miyagi","Yamagata")
#Kanto <- c("Chiba", "Gunma","Ibaraki","Kanagawa","Saitama","Tochigi","Tokyo")
#Minami_Kanto <- c("Saitama","Kanagawa","Chiba")
#Kita_Kanto <- c("Ibaraki","Tochigi","Gunma")
#Chubu <- c("Aichi","Fukui", "Gifu","Ishikawa","Nagano","Niigata","Shizuoka","Toyama","Yamanashi")
#Kinki <- c("Hyogo","Kyoto","Mie","Nara","Osaka","Shiga","Wakayama")
#Chugoku <- c("Hiroshima","Okayama","Shimane","Tottori","Yamaguchi")
#Shikoku <- c("Ehime","Kagawa","Kochi","Tokushima")
#Kyushu <- c("Fukuoka","Kagoshima","Kumamoto","Miyazaki","Nagasaki","Oita","Okinawa","Saga")

library(readxl)
library(haven)
library(dplyr)
library(tidyr)
library(tidyverse)
library(foreign)

path <- "~/Desktop/Econ Research /Week 3"
setwd(path)
#raw DataSet
filename <- "Japanese_Raw_Data.xlsx"
data_raw <- read_excel(filename)

data <- data.frame(region = as.character(),age = as.integer(), males = as.integer(), females = as.integer())
for(i in 1:46){
  df_i <- data.frame(data_raw[,1],data_raw[,(3 * i - 1)],data_raw[,(3 * i) ],data_raw[,(3 * i + 1 )])
  if(grepl("Prefecture",labels(data_raw[,(3 * i - 1)])[[2]])){
  df_i$region <- substring(labels(data_raw[,(3 * i - 1)])[[2]],1,(str_locate(labels(data_raw[,(3 * i - 1)])[[2]],"Prefecture")[1] - 2))
  } else {
  df_i$region <- labels(data_raw[,(3 * i - 1)])[[2]]
  }
  colnames(df_i) <- c("age","total","males","females","region")
  df_i <- df_i[,c(5,1,3,4)]
  df_i <- df_i[-1,]
  data <- rbind(data,df_i)
}
data$age <- as.numeric(data$age)
data$males <- as.numeric(data$males)
data$females <- as.numeric(data$females)

data <- data %>% filter(region != "National")

#replace hyphens with NA
data[ data == "-" ] <- NA

#Hokkaido
Hokkaido <- data %>% filter(region == "Hokkaido") %>% mutate(sexRatio = as.numeric(males) / as.numeric(females))

#Tohoku
Tohoku <- data %>% filter(region == "Akita" | region == "Aomori" | region == "Fukushima" | region == "Iwate" | region == "Miyagi" | region == "Yamagata")
Tohoku <- Tohoku %>% group_by(age) %>% summarise(males = sum(as.numeric(males), na.rm = TRUE), females = sum(as.numeric(females), na.rm = TRUE))
Tohoku <- Tohoku %>%  mutate(sexRatio = males / females) %>% add_column(region = "Tohoku", .before = "age")

#Kanto
Kanto <- data %>% filter(region == "Chiba" | region == "Gunma" | region == "Ibaraki" | region == "Kanagawa" | region == "Saitama" | region == "Tochigi" | region == "Tokyo") 
Kanto <- Kanto %>% group_by(age) %>% summarise(males = sum(as.numeric(males), na.rm = TRUE), females = sum(as.numeric(females), na.rm = TRUE))
Kanto <- Kanto %>%  mutate(sexRatio = males / females)%>% add_column(region = "Kanto", .before = "age")

#Kinki
Kinki <- data %>% filter(region == "Hyogo" | region == "Kyoto" | region == "Mie" | region == "Nara" | region == "Osaka" | region == "Shiga" | region == "Wakayama") 
Kinki <- Kinki %>% group_by(age) %>% summarise(males = sum(as.numeric(males), na.rm = TRUE), females = sum(as.numeric(females), na.rm = TRUE))
Kinki <- Kinki %>%  mutate(sexRatio = males / females) %>% add_column(region = "Kinki", .before = "age")

#Chugoku
Chugoku <- data %>% filter(region == "Hiroshima" | region == "Okayama" | region == "Shimane" | region == "Tottori" | region == "Yamaguchi") 
Chugoku <- Chugoku %>% group_by(age) %>% summarise(males = sum(as.numeric(males), na.rm = TRUE), females = sum(as.numeric(females), na.rm = TRUE))
Chugoku <- Chugoku %>%  mutate(sexRatio = males / females) %>% add_column(region = "Chugoku", .before = "age")

#Shikoku
Shikoku <- data %>% filter(region == "Ehime" | region == "Kagawa" | region == "Kochi" | region == "Tokushima") 
Shikoku <- Shikoku %>% group_by(age) %>% summarise(males = sum(as.numeric(males), na.rm = TRUE), females = sum(as.numeric(females), na.rm = TRUE))
Shikoku <- Shikoku %>%  mutate(sexRatio = males / females) %>% add_column(region = "Shikoku", .before = "age")

#Kyushu
Kyushu <- data %>% filter(region == "Fukuoka" | region == "Kagoshima" | region == "Kumamoto" | region == "Miyazaki" | region == "Nagasaki" | region == "Oita" | region == "Okinawa" | region == "Saga") 
Kyushu <- Kyushu %>% group_by(age) %>% summarise(males = sum(as.numeric(males), na.rm = TRUE), females = sum(as.numeric(females), na.rm = TRUE))
Kyushu <- Kyushu %>%  mutate(sexRatio = males / females) %>% add_column(region = "Kyushu", .before = "age")

#Minami Kanto (South Kanto)
Minami_Kanto <- data %>% filter(region == "Chiba"  | region == "Kanagawa" | region == "Saitama") 
Minami_Kanto <- Minami_Kanto %>% group_by(age) %>% summarise(males = sum(as.numeric(males), na.rm = TRUE), females = sum(as.numeric(females), na.rm = TRUE))
Minami_Kanto <- Minami_Kanto %>%  mutate(sexRatio = males / females) %>% add_column(region = "Minami Kanto", .before = "age")

#Kita Kanto (Notrh Kanto)
Kita_Kanto <- data %>% filter(region == "Ibaraki"  | region == "Tochigi" | region == "Gunma") 
Kita_Kanto <- Kita_Kanto %>% group_by(age) %>% summarise(males = sum(as.numeric(males), na.rm = TRUE), females = sum(as.numeric(females), na.rm = TRUE))
Kita_Kanto <- Kita_Kanto %>%  mutate(sexRatio = males / females) %>% add_column(region = "Kita Kanto", .before = "age")

#Tokyo (usually seperated from larger Kanto region)
Tokyo <- data %>% filter(region == "Tokyo") 
Tokyo <- Tokyo %>% group_by(age) %>% summarise(males = sum(as.numeric(males), na.rm = TRUE), females = sum(as.numeric(females), na.rm = TRUE))
Tokyo <- Tokyo %>%  mutate(sexRatio = males / females) %>% add_column(region = "Tokyo", .before = "age")

#Tokai (an unoofical subregion)
Tokai <- data %>% filter(region == "Shizuoka" | region == "Aichi" | region == "Gifu" | region == "Mie") 
Tokai <- Tokai %>% group_by(age) %>% summarise(males = sum(as.numeric(males), na.rm = TRUE), females = sum(as.numeric(females), na.rm = TRUE))
Tokai <- Tokai %>%  mutate(sexRatio = males / females) %>% add_column(region = "Tokai", .before = "age")

#Hokuriku_Shinetsu
Hokuriku_Shinetsu <- data %>% filter(region == "Ishikawa" | region == "Fukui" | region == "Niigata" | region == "Toyama" | region == "Nagano")
Hokuriku_Shinetsu <- Hokuriku_Shinetsu %>% group_by(age) %>% summarise(males = sum(as.numeric(males), na.rm = TRUE), females = sum(as.numeric(females), na.rm = TRUE))
Hokuriku_Shinetsu <- Hokuriku_Shinetsu %>%  mutate(sexRatio = males / females) %>% add_column(region = "Hokuriku Shinetsu", .before = "age")

#Koshin-etsu
Koshin_etsu <- data %>% filter(region == "Yamanashi" | region == "Niigata" |  region == "Nagano")
Koshin_etsu <- Koshin_etsu %>% group_by(age) %>% summarise(males = sum(as.numeric(males), na.rm = TRUE), females = sum(as.numeric(females), na.rm = TRUE))
Koshin_etsu <- Koshin_etsu %>%  mutate(sexRatio = males / females) %>% add_column(region = "Koshin etsu", .before = "age")

#Tokai_Hokuriku
Tokai_Hokuriku <- data %>% filter(region == "Shizuoka" | region == "Aichi" | region == "Gifu" | region == "Mie" | region == "Ishikawa" | region == "Fukui" | region == "Niigata" | region == "Toyama") 
Tokai_Hokuriku <- Tokai_Hokuriku %>% group_by(age) %>% summarise(males = sum(as.numeric(males), na.rm = TRUE), females = sum(as.numeric(females), na.rm = TRUE))
Tokai_Hokuriku <- Tokai_Hokuriku %>%  mutate(sexRatio = males / females) %>% add_column(region = "Tokai Hokurikuu", .before = "age")

#Merge dataframes vertiaclly 
data <- rbind(Hokkaido,Tohoku,Kanto,Kinki,Chugoku,Shikoku,Kyushu,Minami_Kanto,Kita_Kanto,Tokyo,Tokai,Hokuriku_Shinetsu,Koshin_etsu,Tokai_Hokuriku)



# Make WVS region codes
# WVS region codes change. I use WVS Wave 6 code.
data$X048_WVS <- case_when(
  data$region=="Hokkaido"~392011,
  data$region=="Tohoku"~392012,
  data$region=="Minami Kanto"~392014,
  data$region=="Kita Kanto"~392013,
  data$region=="Tokyo"~392015,
  data$region=="Tokai"~392016,
  data$region=="Hokuriku Shinetsu"~392017,
  data$region=="Kinki"~392018,
  data$region=="Chugoku"~392019,
  data$region=="Shikoku"~392020,
  data$region=="Kyushu"~392021)

# Make ISSP region codes 
data$KR_REG <- case_when(
  data$region=="Hokkaido"~1,
  data$region=="Tohoku"~2,
  data$region=="Kanto"~3,
  data$region=="Koshin etsu"~4,
  data$region=="Tokai Hokuriku"~5,
  data$region=="Kinki"~6,
  data$region=="Chugoku"~7,
  data$region=="Shikoku"~8,
  data$region=="Kyushu"~9)

write_dta(data,"JapanCensus.dta")

