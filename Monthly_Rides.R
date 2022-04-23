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



#Year_2022
# Analysis - Monthly Rides: Member and Casual
# mutate cyclist_2022
data <-cyclist_2022
data

data_1 <- mutate(data, start_date = as.Date(started_at))
data_1

data_2 <-mutate(data_1, start_month_num = as.numeric(format(start_date, '%m')))
data_2

summary(data_2)

# create table
table<- as.data.frame(table(data_2$start_month_num, data_2$member_casual))
names(table) = c("month", "member_casual", "count")
table

#plot month and member_casual
year_2022 <-ggplot(table, mapping = aes(x= month, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_x_discrete(labels = c("1" = "Jan", "2" = "Feb"))+
  labs(title = "Year 2022")
year_2022

#year 2021
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


# 3) Analysis - Monthly rides by member_casual
# mutate cyclist_2021
data <-cyclist_2021
data

data_1 <- mutate(data, start_date = as.Date(started_at))
data_1

data_2 <-mutate(data_1, start_month_num = as.numeric(format(start_date, '%m')))
data_2

summary(data_2)

# create table
table<- as.data.frame(table(data_2$start_month_num, data_2$member_casual))
names(table) = c("month", "member_casual", "count")
table

#plot month and member_casual
year_2021<-ggplot(table, mapping = aes(x= month, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_x_discrete(labels = c("1" = "Jan", "2" = "Feb", "3" ="Mar", "4"= "April", "5"= "May", "6"= "June", "7"="July", "8"= "Aug", "9"= "Sept", "10"= "Oct", "11"= "Nov", "12"="Dec"))+
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


# 3) Analysis - Monthly rides by member_casual
# mutate cyclist_2020
data <-cyclist_2020
data

data_1 <- mutate(data, start_date = as.Date(started_at))
data_1

data_2 <-mutate(data_1, start_month_num = as.numeric(format(start_date, '%m')))
data_2

summary(data_2)

# create table
table<- as.data.frame(table(data_2$start_month_num, data_2$member_casual))
names(table) = c("month", "member_casual", "count")
table

#plot month and member_casual
year_2020 <-ggplot(table, mapping = aes(x= month, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_x_discrete(labels = c("1" = "Jan", "2" = "Feb", "3" ="Mar", "4"= "April", "5"= "May", "6"= "June", "7"="July", "8"= "Aug", "9"= "Sept", "10"= "Oct", "11"= "Nov", "12"="Dec")) +
  scale_y_continuous(labels = comma)+
  labs(title = "Year 2020")
year_2020

#year 2019
setwd("C:/Users/ankit/Desktop/Cyclist/2019/xls")

#read the files
read <- list.files("C:/Users/ankit/Desktop/Cyclist/2019/xls", pattern = ".csv")
read

#merge the fils
merge <- lapply(read,read_csv) %>%
  bind_rows

colnames(merge)
summary(merge)

#drop the col of the dataframe
merge_v1 <- select(merge, -c(gender, birthyear))
summary(merge_v1)

#remove NA from the dataframe
cyclist_2019 <- na.omit(merge_v1)
summary(cyclist_2019)



# 3) Analysis - Monthly rides by member_casual
# mutate cyclist_2019
data <-cyclist_2019
data

data_1 <- mutate(data, start_date = as.Date(start_time))
data_1

data_2 <-mutate(data_1, start_month_num = as.numeric(format(start_date, '%m')))
data_2

summary(data_2)

# create table
table<- as.data.frame(table(data_2$start_month_num, data_2$member_casual))
names(table) = c("month", "member_casual", "count")
table

#plot month and member_casual
year_2019 <-ggplot(table, mapping = aes(x= month, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_x_discrete(labels = c("1" = "Jan", "2" = "Feb", "3" ="Mar", "4"= "April", "5"= "May", "6"= "June", "7"="July", "8"= "Aug", "9"= "Sept", "10"= "Oct", "11"= "Nov", "12"="Dec")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Year 2019")
year_2019

#year 2018
setwd("C:/Users/ankit/Desktop/Cyclist/2018/xls")

#read the files
v1 <- list.files("C:/Users/ankit/Desktop/Cyclist/2018/xls", pattern = ".csv")
v1

#merge the fils
v2 <- lapply(v1,read_csv) %>%
  bind_rows
summary(v2)


#remove cols
v3 <- select(v2,-c(gender, birthyear))

#remove na
cyclist_2018 <- na.omit(v3)
summary(cyclist_2018)


# 3) Analysis - Monthly rides by member_casual
# mutate cyclist_2018
data <-cyclist_2018
data

colnames(data)
data_1 <- mutate(data, start_date = as.Date(start_time))
data_1

data_2 <-mutate(data_1, start_month_num = as.numeric(format(start_date, '%m')))
data_2

summary(data_2)

# create table
table<- as.data.frame(table(data_2$start_month_num, data_2$usertype))
names(table) = c("month", "member_casual", "count")
table

#plot month and member_casual
year_2018 <-ggplot(table, mapping = aes(x= month, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_x_discrete(labels = c("1" = "Jan", "2" = "Feb", "3" ="Mar", "4"= "April", "5"= "May", "6"= "June", "7"="July", "8"= "Aug", "9"= "Sept", "10"= "Oct", "11"= "Nov", "12"="Dec")) +
  scale_y_continuous(labels = comma)+
  labs(title = "Monthly: Members and Casuals", subtitle = "Year: 2018")
year_2018


#year_2017
setwd("C:/Users/ankit/Desktop/Cyclist/2017/xls")

#read the files
v1 <- list.files(path = "C:/Users/ankit/Desktop/Cyclist/2017/xls", pattern = ".csv" )
v1

#merge files
v2 <- lapply(v1,read_csv) %>%
  bind_rows()

#summary 
summary(v2)

#change col name
names(v2)[names(v2) == "usertype"] <- "member_casual" 

remove <-subset(v2, member_casual == "Dependent")
remove

#removing the subset from the dataframe v2
data_frame_mod <- anti_join(v2,remove)
data_frame_mod

#remove col gender and birthyear
cyclist_2017 <- select(data_frame_mod, -c(gender, birthyear))
summary(cyclist_2017)


# 3) Analysis - Monthly rides by member_casual
# mutate cyclist_2017
data <-cyclist_2017
data

colnames(data)

data_1 <- mutate(data, start_date = as.Date(start_time))
data_1

data_2 <-mutate(data_1, start_month_num = as.numeric(format(start_date, '%m')))
data_2

summary(data_2)

# create table
table<- as.data.frame(table(data_2$start_month_num, data_2$member_casual))
names(table) = c("month", "member_casual", "count")
table

#plot month and member_casual
year_2017 <-ggplot(table, mapping = aes(x= month, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_x_discrete(labels = c("1" = "Jan", "2" = "Feb", "3" ="Mar", "4"= "April", "5"= "May", "6"= "June", "7"="July", "8"= "Aug", "9"= "Sept", "10"= "Oct", "11"= "Nov", "12"="Dec")) +
  scale_y_continuous(labels = comma)+
  labs(title = "Year 2017")
year_2017

#year 2016
# directory 
setwd("C:/Users/ankit/Desktop/Cyclist/2016/xls")

# read files
v1 <- list.files( path = "C:/Users/ankit/Desktop/Cyclist/2016/xls", pattern = ".csv")
v1

#merge files
v2 <- lapply(v1, read_csv) %>%
  bind_rows

#summary
summary(v2)

# remove col
v3 <- select(v2, -c(starttime, stoptime, gender, birthyear))
summary(v3)

# rename col
names(v3)[names(v3) =="usertype"] = "member_casual"
summary(v3)

remove <-subset(v3, member_casual == "Dependent")
remove

#removing the subset from the dataframe v2
data_frame_mod <- anti_join(v3,remove)
data_frame_mod

# remove na and assign final name
cyclist_2016 <- na.omit(data_frame_mod)
summary(cyclist_2016)


# 3) Analysis - Monthly rides by member_casual
# mutate cyclist_2016
data <-cyclist_2016
data

colnames(data)

data_1 <- mutate(data, start_date = as.Date(start_time))
data_1

data_2 <-mutate(data_1, start_month_num = as.numeric(format(start_date, '%m')))
data_2

summary(data_2)

# create table
table<- as.data.frame(table(data_2$start_month_num, data_2$member_casual))
names(table) = c("month", "member_casual", "count")
table

#plot month and member_casual
year_2016 <-ggplot(table, mapping = aes(x= month, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_x_discrete(labels = c("1" = "Jan", "2" = "Feb", "3" ="Mar", "4"= "April", "5"= "May", "6"= "June", "7"="July", "8"= "Aug", "9"= "Sept", "10"= "Oct", "11"= "Nov", "12"="Dec")) +
  scale_y_continuous(labels = comma)+
  labs(title = "Year: 2016")
year_2016



``

