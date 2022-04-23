install.packages("tidyverse")
install.packages("readr")
install.packages("plyr")
install.packages("dplyr")
install.packages("scales")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("patchwork")


#load library
library(tidyverse)
library(readr)
library(plyr)
library(dplyr)
library(scales)
library(lubridate)
library(ggplot2)
library(patchwork)

#Year_2022
# change the working directory to file location
setwd("C:/Users/ankit/Desktop/Cyclist/2022/xls")
list.files()

# read the csv files directly from file location
file_2022 <- list.files("C:/Users/ankit/Desktop/Cyclist/2022/xls", pattern = ".csv")
file_2022

# merge the files
cyclist_2022 <- lapply(file_2022, read_csv) %>%
  bind_rows

#summary of the merged file
summary(cyclist_2022)



# Analysis -Rideable Types: Member and Casual 
#create_table
table<- as.data.frame(table(cyclist_2022$rideable_type, cyclist_2022$member_casual))
names(table) = c("rideable_type", "member_casual", "count")
table

#plot rideable_type and member_casual
year_2022 <-ggplot(table, mapping = aes(x= rideable_type, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  facet_wrap(~rideable_type)+
  labs(title = "Year 2022")
year_2022

#year_2021
# change the working directory to file location
setwd("C:/Users/ankit/Desktop/Cyclist/2021/xls")

# read the csv files directly from file location
file_2021 <- list.files("C:/Users/ankit/Desktop/Cyclist/2021/xls", pattern = ".csv")
file_2021

# merge the files
v1 <- lapply(file_2021, read_csv) %>%
  bind_rows

#summary of the merged file
summary(v1)


#remove na
v1_na <- na.omit(v1)

# assign m1_na to cyclist_2021
cyclist_2021 <- v1_na

# summary
summary(cyclist_2021)

# 2) Analysis -Ride types and member casual 
#create_table
table<- as.data.frame(table(cyclist_2021$rideable_type, cyclist_2021$member_casual))
names(table) = c("rideable_type", "member_casual", "count")
table

#plot rideable_type and member_casual
year_2021 <-ggplot(table, mapping = aes(x= rideable_type, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  facet_wrap(~rideable_type)+
  labs(title = "Year 2021")
year_2021


#year_2020
# change the working directory to file location
setwd("C:/Users/ankit/Desktop/Cyclist/2020/xls/cyclist_2020_mod")

# read the csv files directly from file location
v1<- list.files("C:/Users/ankit/Desktop/Cyclist/2020/xls/cyclist_2020_mod", pattern = ".csv")
v1

# merge
v2 <- lapply(v1, read_csv) %>%
  bind_rows

# summary
summary(v2)

#remove na
v3 <- na.omit(v2)
summary(v3)

#assign name to df
cyclist_2020 <- v3
summary(cyclist_2020)

# 2) Analysis -Ride types and member casual 
#create_table
table<- as.data.frame(table(cyclist_2020$rideable_type, cyclist_2020$member_casual))
names(table) = c("rideable_type", "member_casual", "count")
table

#plot rideable_type and member_casual
year_2020 <-ggplot(table, mapping = aes(x= rideable_type, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  facet_wrap(~rideable_type)+
  labs(title = "Ride_types: Member and Casual", subtitle = "Year: 2020")
year_2020




