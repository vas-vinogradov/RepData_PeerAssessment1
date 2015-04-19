---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Load the data (i.e. read.csv())


```r
data<-read.csv("..\\repdata-data-activity\\activity.csv",colClasses=c("numeric","Date","numeric"))
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?
Calculate the total number of steps taken per day


```r
stepsbyday<-sapply(split(data$steps,data$date),sum,na.rm=TRUE)
```

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(stepsbyday,breaks=10)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Calculate and report the mean and median of the total number of steps taken per day


```r
mean(stepsbyday)
```

```
## [1] 9354.23
```

```r
median(stepsbyday)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsbyinterval<-sapply(split(data$steps,data$interval),mean,na.rm=TRUE)
plot(unique(data$interval),stepsbyinterval,type="l",xlab="5-minute interval", ylab="average number of steps taken")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
data$interval[which.max(stepsbyinterval)]
```

```
## [1] 835
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
length(which(is.na(data$steps)))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
===
*We will use total mean to replace data*

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data_without_na<-data
data_without_na$steps[which(is.na(data$steps))]<-mean(data$step,na.rm=TRUE)
stepsbydaywithdata<-sapply(split(data_without_na$steps,data_without_na$date),sum)
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(stepsbydaywithdata,breaks=10)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
mean(stepsbydaywithdata)
```

```
## [1] 10766.19
```

```r
median(stepsbydaywithdata)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(stepsbydaywithdata-stepsbyday,breaks=10)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
mean(stepsbydaywithdata-stepsbyday)
```

```
## [1] 1411.959
```

```r
median(stepsbydaywithdata-stepsbyday)
```

```
## [1] 0
```


## Are there differences in activity patterns between weekdays and weekends?


```r
wd1<-function(dt){if(weekdays(dt)=="воскресенье" || weekdays(dt)=="суббота") {1} else {0}}
data$weekday=factor(sapply(data$date,wd1))
we<-sapply(split(data$steps[data$weekday==1],data$interval[data$weekday==1]),mean,na.rm=TRUE)
wd<-sapply(split(data$steps[data$weekday==0],data$interval[data$weekday==0]),mean,na.rm=TRUE)
steps<-c(we,wd)
weekdays<-factor(c(rep(1,length(we)),rep(0,length(wd))),c(0,1),c("weekday","weekend"))

df<-data.frame(steps,weekdays,unique(data$interval))
names(df)<-c("s","wd","int")

library(ggplot2)
qplot(int,s,data=df,geom="line",facets=wd~.)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
