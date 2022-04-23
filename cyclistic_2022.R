#install packages

install.packages("tidyverse")
install.packages("readr")
install.packages("plyr")
install.packages("dplyr")
install.packages("scales")
install.packages("lubridate")

#load library
library(tidyverse)
library(readr)
library(plyr)
library(dplyr)
library(scales)
library(lubridate)


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



#1) Analysis- Number of member and casual riders
#create table
step_1 <- as.data.frame(table(cyclist_2022$member_casual))
names(step_1)=c("member_casual","count")
step_1

# plot member_casual 
step_2<-ggplot(step_1 ,mapping =  aes(x= member_casual, y= count, fill = member_casual))+
  geom_bar(stat = "identity") + 
  geom_text(aes(label = count), vjust = 0)
step_2

# fix scale problem 
step_3 <- step_2+scale_y_continuous(labels = comma)
step_3

# add title to the plot
step_4 <-step_3+labs(title = "Riders: Member and Casual", subtitle = "Year: 2022")
step_4




# 2) Analysis -Ride types and member casual 
#create_table
step_1<- as.data.frame(table(cyclist_2022$rideable_type, cyclist_2022$member_casual))
names(step_1) = c("rideable_type", "member_casual", "count")
step_1

#plot rideable_type and member_casual
step_2 <-ggplot(step_1, mapping = aes(x= rideable_type, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(aes(label = count), hjust = 1, vjust = 0)+
  facet_wrap(~rideable_type)
step_2

# add title to the plot
step_3 <-step_2+labs(title = "Ride_types: Member and Casual", subtitle = "Year: 2022")
step_3


# 3) Analysis - Monthly rides by member_casual
# mutate cyclist_2022
data <-cyclist_2022
data

data_1 <- mutate(data, start_date = as.Date(started_at))
data_1

data_2 <-mutate(data_1, start_month_num = as.numeric(format(start_date, '%m')))
data_2

summary(data_2)

# create table
step_1<- as.data.frame(table(data_2$start_month_num, data_2$member_casual))
names(step_1) = c("month", "member_casual", "count")
step_1

#plot month and member_casual
step_2 <-ggplot(step_1, mapping = aes(x= month, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(aes(label = count), hjust = 1, vjust = 0)
step_2

#change the x-axis format
step_3 <- step_2 + 
  scale_x_discrete(labels = c("1" = "Jan", "2" = "Feb"))
step_3

# add title to the graph
step_4 <-step_3+labs(title = "Monthly: Members and Casuals", subtitle = "Year: 2022")
step_4

#4. analysis- day and member_casual
#create table
step_1 <- as.data.frame(table(cyclist_2022$day_of_week, cyclist_2022$member_casual))
names(step_1) = c("day_of_week", "member_casual", "count")
step_1

#plot day and member_casual
step_2 <-ggplot(step_1, mapping = aes(x= day_of_week, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(aes(label = count), hjust = 1, vjust = 0)
step_2

#change the x-axis format
step_3 <- step_2 + 
  scale_x_discrete(labels = c("1" = "Sun", "2" = "Mon", "3" = "Tue", "4"= "Wed", "5"= "Thus", "6"="Fri", "7" = "Sat"))
step_3

# add title to the graph
step_4 <-step_3+labs(title = "Day: Member and Casual", subtitle = "Year: 2022")
step_4








