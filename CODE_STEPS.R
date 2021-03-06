# Code for reading data and processing the database

# setting up the directory and loading packages

setwd("~/R/COURSERA/Reproducible research/repdata_data_activity")
rm(list=ls())
library(tidyverse); library(ggplot2)

# Reading and processing the data to respond to the questions in the assignment

activity <- read.table("~/R/COURSERA/Reproducible research/repdata_data_activity/activity.csv",header=TRUE,sep=",")
activity$date<-as.Date(as.character(activity$date))
activity<-as_tibble(activity)
activity$time<-activity$interval/100 # simbolizes time of day (need to change to time format)

# make a column that incorporates the day of the week the number of steps was performed

activity$dia<-weekdays(activity$date)

# Introduce a column in the Data Frame, that registers if the steps where recorded on a "weekend" day or a weekday.

activity$cat_day<-"weekday"
activity$cat_day[(activity$dia=="s�bado" | activity$dia=="domingo")*(1:dim(activity)[1])]<-"weekend"

# Code to process the different questions
    #Question 2: Histogram of the total number of steps taken each day

sum_steps_day<-aggregate(activity$steps~activity$date+activity$dia+activity$cat_day,data=activity,FUN=sum)
colnames(sum_steps_day)<-c("date","dia","cat_day","steps")
ggplot(sum_steps_day,aes(x=steps))+geom_histogram(colour="black",fill="red",frequency=TRUE)+theme_bw()
ggsave("plot1.png")

    #Question 3: Mean and median number of steps taken each day

mean_steps_day<-aggregate(activity$steps~activity$date+activity$dia+activity$cat_day,data=activity,FUN=mean)
med_steps_day<-aggregate(activity$steps~activity$date+activity$dia+activity$cat_day,data=activity,FUN=median)
colnames(mean_steps_day)<-c("date","dia","cat_day","steps")
summary(mean_steps_day)
colnames(med_steps_day)<-c("date","dia","cat_day","steps")
summary(med_steps_day)

# Question 4: Time series plot of the average number of steps taken each day

ggplot(mean_steps_day,aes(x=date,y=steps),pch=20)+geom_line()+geom_point()+theme_bw()+ylab("Average steps")
ggsave("Plot2.png")

# Question 5: The 5-minute interval that, on average, contains the maximum number of steps

steps_inter<-aggregate(activity$steps~activity$interval,data=activity,FUN = mean)
colnames(steps_inter)<-c("interval","steps")
ggplot(steps_inter,aes(x=interval,y=steps),pch=20)+geom_line()+geom_point()+theme_bw()+ylab("Average steps")+ggtitle("Average number of steps by 5-minute interval")
ggsave("plot3.png")
fivemin_inter<-steps_inter[sum(as.numeric(max(steps_inter$steps)==steps_inter$steps)*(1:dim(steps_inter)[1])),]
print("5-minute interval at which its registered the maximum number of steps and number of steps taken")
print(fivemin_inter)

# Code to describe and show a strategy for imputing missing data
# the first step is analyzing if the missing data corresponds to a weekday or weekend day and the specific interval?
missing<-filter(activity,is.na(activity$steps))
# average steps per day per time of the day
steps_inter_cat_day<-aggregate(activity$steps~activity$interval+activity$cat_day+activity$dia+activity$time,data=activity,FUN = mean)
steps_inter_cat<-aggregate(activity$steps~activity$interval+activity$cat_day+activity$time,data=activity,FUN = mean)
colnames(steps_inter_cat_day)<-c("interval","cat_day","dia","time","steps")
colnames(steps_inter_cat)<-c("interval","cat_day","time","steps")
ggplot(mean_steps_day,aes(x=dia,y=steps),pch=20)+geom_boxplot ()+geom_point()+theme_bw()+ylab("Average steps")
ggsave("plot4.png")
steps_cat_day<-aggregate(steps~cat_day,data=steps_inter_cat, mean)
steps_day<-aggregate(steps~dia,data=mean_steps_day,mean)
# Data base with imputed values
list_na <- colnames(activity)[ apply(activity, 2, anyNA) ]
average_missing <- apply(activity[,colnames(activity) %in% list_na],2,mean, na.rm =  TRUE)
activity_2<-activity%>%mutate(steps=ifelse(is.na(steps),steps_inter_cat_day$steps,average_missing))
# Box plot number of steps during weekend and weekdays
ggplot(mean_steps_day,aes(x=cat_day,y=steps),pch=20)+geom_boxplot ()+geom_point()+theme_bw()+ylab("Average steps")
ggsave("Plot5.png")
ggplot(steps_inter_cat,aes(x=time,y=steps,group=factor(cat_day),colour=factor(cat_day)))+geom_line()+geom_point()+theme_classic()+labs(x="Hora del d�a",colour="",y="Average steps")+
  scale_x_continuous("Time of day",)
ggsave("Plot6.png")

# Histogram of the total number of steps taken each day after missing values are imputed

sum_steps_day_2<-aggregate(activity_2$steps~activity_2$date+activity_2$dia+activity_2$cat_day,data=activity_2,FUN=sum)
colnames(sum_steps_day_2)<-c("date","dia","cat_day","steps")
ggplot(sum_steps_day_2,aes(x=steps))+geom_histogram(colour="black",fill="red",frequency=TRUE)+theme_bw()
ggsave("plot7.png")

# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

steps_inter_cat_day_2<-aggregate(activity_2$steps~activity_2$interval+activity_2$cat_day+activity_2$dia+activity_2$time,data=activity,FUN = mean)
colnames(steps_inter_cat_day)<-c("interval","cat_day","dia","time","steps")
mean_steps_day_2<-aggregate(activity_2$steps~activity_2$date+activity_2$dia+activity_2$cat_day,data=activity_2,FUN=mean)
colnames(mean_steps_day_2)<-c("date","dia","cat_day","steps")
ggplot(mean_steps_day,aes(x=cat_day,y=steps),pch=20)+geom_boxplot ()+geom_point()+theme_bw()+ylab("Average steps")
ggsave("Plot8.png")
# It seems that there is more activity during weekends at working hours. They also wake-up later during weekends




