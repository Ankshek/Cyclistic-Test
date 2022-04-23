#install packages
install.packages("tidyverse")
install.packages("readr")
install.packages("plyr")
install.packages("dplyr")
install.packages("scales")
install.packages("lubridate")

#load packages
library(tidyverse)
library(readr)
library(plyr)
library(dplyr)
library(scales)
library(lubridate)

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

#1) Analysis- Number of member and casual riders
#create table
step_1 <- as.data.frame(table(cyclist_2017$member_casual))
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
step_4 <-step_3+labs(title = "Riders: Member and Casual", subtitle = "Year: 2017")
step_4


# 2) Analysis -Ride types and member casual - Information unavailable


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
  scale_x_discrete(labels = c("1" = "Jan", "2" = "Feb", "3" ="Mar", "4"= "April", "5"= "May", "6"= "June", "7"="July", "8"= "Aug", "9"= "Sept", "10"= "Oct", "11"= "Nov", "12"="Dec")) +
  scale_y_continuous(labels = comma)
step_3

# add title to the graph
step_4 <-step_3+labs(title = "Monthly: Members and Casuals", subtitle = "Year: 2017")
step_4

#4. analysis- day and member_casual
#create table
step_1 <- as.data.frame(table(cyclist_2017$day_of_week, cyclist_2017$member_casual))
names(step_1) = c("day_of_week", "member_casual", "count")
step_1

#plot day and member_casual
step_2 <-ggplot(step_1, mapping = aes(x= day_of_week, y= count, fill = member_casual))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(aes(label = count), hjust = 1, vjust = 0)
step_2

#change the x-axis format
step_3 <- step_2 + 
  scale_x_discrete(labels = c("1" = "Sun", "2" = "Mon", "3" = "Tue", "4"= "Wed", "5"= "Thus", "6"="Fri", "7" = "Sat"))+
  scale_y_continuous(labels = comma)

step_3

# add title to the graph
step_4 <-step_3+labs(title = "Day: Member and Casual", subtitle = "Year: 2017")
step_4


