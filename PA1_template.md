---
title: 'Reproducible Research: Peer Assessment 1'
author: "Sahil Kanolkar"
date: "Sunday, May 17, 2015"
output: html_document
---

### Loading and Preprocessing the Data

The activity data is loaded using the read.csv() function as follows:


```r
activity <- read.csv("activity.csv", colClasses = c("numeric", "character","numeric"))
head(activity)
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

The plotting system used is lattice plotting system and it can loaded as follows:


```r
library(lattice)
```

The dates for various acivities recorded can be extracted as follows:


```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

### Mean total number of steps taken per day

1. The total number of steps can be found out by using the aggregate function and this data can then better be visualized using a histogram given below.


```r
totalSteps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
hist(totalSteps$steps, main = "Total steps taken each day", xlab = "Day", col = "blue")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

2. Mean and Median of the total number of steps taken per day is as follows:


```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```

### Average Daily Activity pattern

Time-Series plot for the 5 minute interval and the average number of steps taken, averaged across all days.


```r
time <- tapply(activity$steps, activity$interval,mean,na.rm=TRUE)
plot(row.names(time), time, type = "l", xlab = "5-min interval", 
     ylab = "Average across all Days", main = "Average number of steps taken")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

The 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps is


```r
max <- which.max(time)
names(max)
```

```
## [1] "835"
```

### Imputing Missing Values

1. The total number of missing values in the dataset are

```r
sum(is.na(activity))
```

```
## [1] 2304
```

2. Fill in the missing values with the mean

```r
StepsAverage <- aggregate(steps ~ interval, data = activity, FUN = mean)
NAvalues <- numeric()
for (i in 1:nrow(activity)) {
    data <- activity[i, ]
    if (is.na(data$steps)) {
        steps <- subset(StepsAverage, interval == data$interval)$steps
    } else {
        steps <- data$steps
    }
    NAvalues <- c(NAvalues, steps)
}
```

3. Creating a new dataset

```r
new_activity <- activity
new_activity$steps <- NAvalues
```

4. Histogram of the new dataset with mean and median

```r
totalSteps2 <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(totalSteps2$steps, main = "Total steps taken each day", xlab = "Day", col = "blue")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
mean(totalSteps2$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps2$steps)
```

```
## [1] 10766.19
```

### Differences in the activity patterns between weekdays and weekends

1. Weekdays function can be used for this purpose. A factor variable can be created as follows:


```r
day <- weekdays(activity$date)
daylevel <- vector()
for (i in 1:nrow(activity)) {
    if (day[i] == "Saturday") {
        daylevel[i] <- "Weekend"
    } else if (day[i] == "Sunday") {
        daylevel[i] <- "Weekend"
    } else {
        daylevel[i] <- "Weekday"
    }
}
activity$daylevel <- daylevel
activity$daylevel <- factor(activity$daylevel)

stepsByDay <- aggregate(steps ~ interval + daylevel, data = activity, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")
```

2. Panel plot


```r
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

Thus it can be seen that the number of steps are higher in weekends than in weekdays.
