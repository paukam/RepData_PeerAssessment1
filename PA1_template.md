---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
activityDS <- read.table(unz("activity.zip","activity.csv"), sep=",", header = TRUE)
summary(activityDS)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```
## What is mean total number of steps taken per day?
  1. Make a histogram of the total number of steps taken each day


```r
stepsByDay <- aggregate(steps ~ date, data = activityDS, FUN = sum, na.rm = TRUE)
hist(stepsByDay$steps, xlab = "Steps per Day", main = "Total number of steps taken per day", col = "blue") 
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
  
  2. Calculate and report the mean and median total number of steps taken per day


```r
mean(stepsByDay$steps)
```

```
## [1] 10766.19
```

```r
median(stepsByDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
  1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgStepsMean <- aggregate(steps ~ interval, data = activityDS, FUN = mean, na.rm = TRUE)
plot(avgStepsMean$interval, avgStepsMean$steps, type = "l", xlab = "Inervals", ylab = "Average no. of steps", main = "The average number of steps taken per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
  
  2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
#the maximum number of steps?
maxSteps <- max(avgStepsMean$steps)
#for which interval are the numbers of steps per interval are the highest?
maxInterval <- avgStepsMean$interval[which(avgStepsMean$steps == maxSteps)]
maxSteps <- round(maxSteps, digits = 0)
```

The maximum number of steps (**206**) contains 5-min interval no. **835**

## Imputing missing values
  1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activityDS))
```

```
## [1] 2304
```
  2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
**the mean for that 5-minute interval**

```r
missingValues <- subset(activityDS, is.na(steps))
hist(missingValues$interval, main="NAs per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
  
  3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
  

```r
  newDS <- activityDS
  newDS$steps <- ifelse(is.na(activityDS$steps),
  round(avgStepsMean$steps[match(activityDS$interval,
  avgStepsMean$interval)],0), activityDS$steps)
  summary(newDS)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 27.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0
```
  4. Make a histogram of the total number of steps taken each day

```r
NewStepsByDay <- aggregate(steps ~ date, data = newDS, FUN = sum)
hist(NewStepsByDay$steps, xlab = "Steps per Day", main = "Total number of steps taken per day", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

    5. Calculate and report the mean and median total number of steps taken per day.
    

```r
mean(NewStepsByDay$steps)
```

```
## [1] 10765.64
```

```r
median(NewStepsByDay$steps)
```

```
## [1] 10762
```
  
  Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
*Imputing missing values didn’t change the mean value, the median value is slightly reduced.*
  
## Are there differences in activity patterns between weekdays and weekends?
  1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
newDS$dayType <- ifelse(weekdays(as.Date(newDS$date)) == "sobota" | weekdays(as.Date(newDS$date)) == "niedziela", "weekend", "weekday")
newDS$dayType <- factor(newDS$dayType)
```

  2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
  

```r
stepsByIntervalDayType <- aggregate(steps ~ interval + dayType, data = newDS, FUN = mean)
library(ggplot2)
plot <- ggplot(stepsByIntervalDayType, aes(interval, steps))
plot + geom_line(color = "blue") + facet_grid(dayType~.) + labs(x = "Intervals", y = "Average Steps", title = "Activity Patterns")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->







