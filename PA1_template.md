---
title: "Coursera_rep_w2"
author: "Gabriel Olivares G"
date: "5/3/2021"
output: 
  html_document: 
    keep_md: yes
keep_md: TRUE
---



## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```r
# Code for reading data and processing the database

# setting up the directory and loading packages

setwd("~/R/COURSERA/Reproducible research/repdata_data_activity")
rm(list=ls())
library(tidyverse); library(ggplot2)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.3     v purrr   0.3.4
## v tibble  3.0.6     v dplyr   1.0.4
## v tidyr   1.1.2     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
# Reading and processing the data to respond to the questions in the assignment

activity <- read.table("~/R/COURSERA/Reproducible research/repdata_data_activity/activity.csv",header=TRUE,sep=",")
activity$date<-as.Date(as.character(activity$date))
activity<-as_tibble(activity)
activity$time<-activity$interval/100 # simbolizes time of day (need to change to time format)

# make a column that incorporates the day of the week the number of steps was performed

activity$dia<-weekdays(activity$date)

# Introduce a column in the Data Frame, that registers if the steps where recorded on a "weekend" day or a weekday.

activity$cat_day<-"weekday"
activity$cat_day[(activity$dia=="sábado" | activity$dia=="domingo")*(1:dim(activity)[1])]<-"weekend"

# Code to process the different questions
    #Question 2: Histogram of the total number of steps taken each day

sum_steps_day<-aggregate(activity$steps~activity$date+activity$dia+activity$cat_day,data=activity,FUN=sum)
colnames(sum_steps_day)<-c("date","dia","cat_day","steps")
ggplot(sum_steps_day,aes(x=steps))+geom_histogram(colour="black",fill="red",frequency=TRUE)+theme_bw()
```

```
## Warning: Ignoring unknown parameters: frequency
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
ggsave("plot1.png")
```

```
## Saving 7 x 5 in image
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```r
    #Question 3: Mean and median number of steps taken each day

mean_steps_day<-aggregate(activity$steps~activity$date+activity$dia+activity$cat_day,data=activity,FUN=mean)
med_steps_day<-aggregate(activity$steps~activity$date+activity$dia+activity$cat_day,data=activity,FUN=median)
colnames(mean_steps_day)<-c("date","dia","cat_day","steps")
summary(mean_steps_day)
```

```
##       date                dia              cat_day              steps        
##  Min.   :2012-10-02   Length:53          Length:53          Min.   : 0.1424  
##  1st Qu.:2012-10-16   Class :character   Class :character   1st Qu.:30.6979  
##  Median :2012-10-29   Mode  :character   Mode  :character   Median :37.3785  
##  Mean   :2012-10-30                                         Mean   :37.3826  
##  3rd Qu.:2012-11-16                                         3rd Qu.:46.1597  
##  Max.   :2012-11-29                                         Max.   :73.5903
```

```r
colnames(med_steps_day)<-c("date","dia","cat_day","steps")
summary(med_steps_day)
```

```
##       date                dia              cat_day              steps  
##  Min.   :2012-10-02   Length:53          Length:53          Min.   :0  
##  1st Qu.:2012-10-16   Class :character   Class :character   1st Qu.:0  
##  Median :2012-10-29   Mode  :character   Mode  :character   Median :0  
##  Mean   :2012-10-30                                         Mean   :0  
##  3rd Qu.:2012-11-16                                         3rd Qu.:0  
##  Max.   :2012-11-29                                         Max.   :0
```

```r
# Question 4: Time series plot of the average number of steps taken each day

ggplot(mean_steps_day,aes(x=date,y=steps),pch=20)+geom_line()+geom_point()+theme_bw()+ylab("Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
ggsave("Plot2.png")
```

```
## Saving 7 x 5 in image
```

```r
# Question 5: The 5-minute interval that, on average, contains the maximum number of steps

steps_inter<-aggregate(activity$steps~activity$interval,data=activity,FUN = mean)
colnames(steps_inter)<-c("interval","steps")
ggplot(steps_inter,aes(x=interval,y=steps),pch=20)+geom_line()+geom_point()+theme_bw()+ylab("Average steps")+ggtitle("Average number of steps by 5-minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
ggsave("plot3.png")
```

```
## Saving 7 x 5 in image
```

```r
fivemin_inter<-steps_inter[sum(as.numeric(max(steps_inter$steps)==steps_inter$steps)*(1:dim(steps_inter)[1])),]
print("5-minute interval at which its registered the maximum number of steps and number of steps taken")
```

```
## [1] "5-minute interval at which its registered the maximum number of steps and number of steps taken"
```

```r
print(fivemin_inter)
```

```
##     interval    steps
## 104      835 206.1698
```

```r
# Code to describe and show a strategy for imputing missing data
# the first step is analyzing if the missing data corresponds to a weekday or weekend day and the specific interval?
missing<-filter(activity,is.na(activity$steps))
# average steps per day per time of the day
steps_inter_cat_day<-aggregate(activity$steps~activity$interval+activity$cat_day+activity$dia+activity$time,data=activity,FUN = mean)
steps_inter_cat<-aggregate(activity$steps~activity$interval+activity$cat_day+activity$time,data=activity,FUN = mean)
colnames(steps_inter_cat_day)<-c("interval","cat_day","dia","time","steps")
colnames(steps_inter_cat)<-c("interval","cat_day","time","steps")
ggplot(mean_steps_day,aes(x=dia,y=steps),pch=20)+geom_boxplot ()+geom_point()+theme_bw()+ylab("Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-4.png)<!-- -->

```r
ggsave("plot4.png")
```

```
## Saving 7 x 5 in image
```

```r
steps_cat_day<-aggregate(steps~cat_day,data=steps_inter_cat, mean)
steps_day<-aggregate(steps~dia,data=mean_steps_day,mean)
# Data base with imputed values
list_na <- colnames(activity)[ apply(activity, 2, anyNA) ]
average_missing <- apply(activity[,colnames(activity) %in% list_na],2,mean, na.rm =  TRUE)
activity_2<-activity%>%mutate(steps=ifelse(is.na(steps),steps_inter_cat_day$steps,average_missing))
# Box plot number of steps during weekend and weekdays
ggplot(mean_steps_day,aes(x=cat_day,y=steps),pch=20)+geom_boxplot ()+geom_point()+theme_bw()+ylab("Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-5.png)<!-- -->

```r
ggsave("Plot5.png")
```

```
## Saving 7 x 5 in image
```

```r
ggplot(steps_inter_cat,aes(x=time,y=steps,group=factor(cat_day),colour=factor(cat_day)))+geom_line()+geom_point()+theme_classic()+labs(x="Hora del día",colour="",y="Average steps")+
  scale_x_continuous("Time of day",)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-6.png)<!-- -->

```r
ggsave("Plot6.png")
```

```
## Saving 7 x 5 in image
```

```r
# Histogram of the total number of steps taken each day after missing values are imputed

sum_steps_day_2<-aggregate(activity_2$steps~activity_2$date+activity_2$dia+activity_2$cat_day,data=activity_2,FUN=sum)
colnames(sum_steps_day_2)<-c("date","dia","cat_day","steps")
ggplot(sum_steps_day_2,aes(x=steps))+geom_histogram(colour="black",fill="red",frequency=TRUE)+theme_bw()
```

```
## Warning: Ignoring unknown parameters: frequency
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-1-7.png)<!-- -->

```r
ggsave("plot7.png")
```

```
## Saving 7 x 5 in image
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```r
# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

steps_inter_cat_day_2<-aggregate(activity_2$steps~activity_2$interval+activity_2$cat_day+activity_2$dia+activity_2$time,data=activity,FUN = mean)
colnames(steps_inter_cat_day)<-c("interval","cat_day","dia","time","steps")
mean_steps_day_2<-aggregate(activity_2$steps~activity_2$date+activity_2$dia+activity_2$cat_day,data=activity_2,FUN=mean)
colnames(mean_steps_day_2)<-c("date","dia","cat_day","steps")
ggplot(mean_steps_day,aes(x=cat_day,y=steps),pch=20)+geom_boxplot ()+geom_point()+theme_bw()+ylab("Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-8.png)<!-- -->

```r
ggsave("Plot8.png")
```

```
## Saving 7 x 5 in image
```

```r
# It seems that there is more activity during weekends at working hours. They also wake-up later during weekends
```
