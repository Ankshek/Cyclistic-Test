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


#1) Analysis- Number of member and casual riders
#create table
step_1 <- as.data.frame(table(cyclist_2018$usertype))
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
step_4 <-step_3+labs(title = "Riders: Member and Casual", subtitle = "Year: 2018")
step_4


# 2) Analysis -Ride types and member casual - Information unavailable


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
step_1<- as.data.frame(table(data_2$start_month_num, data_2$usertype))
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
step_4 <-step_3+labs(title = "Monthly: Members and Casuals", subtitle = "Year: 2018")
step_4

#4. analysis- day and member_casual
#create table
step_1 <- as.data.frame(table(cyclist_2018$day_of_week, cyclist_2018$usertype))
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
step_4 <-step_3+labs(title = "Day: Member and Casual", subtitle = "Year: 2018")
step_4


