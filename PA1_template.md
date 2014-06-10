# Reproducible Research: Peer Assessment 1

Please set the working directory to the repository root with `setwd()` before sourcing this file.

Setting knitr options.

```r
require(knitr)
opts_chunk$set(fig.path="figures/")
```

Load required libraries.

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Loading and preprocessing the data

The project makes use of data from a personal activity monitoring device.
This device collects data at 5 minute intervals through out the day.
The data consists of two months of data from an anonymous individual collected
during the months of October and November, 2012 and include the number of steps
taken in 5 minute intervals each day.

The variables included in the dataset activity.zip are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total
of 17,568 observations in this dataset.

We load the dataset.

```r
activity <- read.csv(unz("activity.zip", "activity.csv"))
```

We create a dataset without NAs.

```r
activity_no_NA <- activity[complete.cases(activity),]
```

## What is mean total number of steps taken per day?

We create a dataset with the total number of steps taken per day.

```r
steps_per_day <- activity_no_NA %.% group_by(date) %.% summarise(total_steps = sum(steps))
```

We make a histogram of the total number of steps taken each day.

```r
with(steps_per_day, hist(total_steps, main = "Total steps per day", xlab = ""))
```

![plot of chunk hist_steps_per_day](figures/hist_steps_per_day.png) 

We calculate and report the mean and median total number of steps taken per day.

```r
with(steps_per_day, summary(total_steps))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8840   10800   10800   13300   21200
```

## What is the average daily activity pattern?

We create a dataset with the average number of steps taken per 5-minute interval,
averaged across all days (y-axis)

```r
average_daily_activity <- activity_no_NA %.% group_by(interval) %.% summarise(average_activity = mean(steps))
```

We make a time series plot of the 5-minute interval (x-axis) and the average
number of steps taken, averaged across all days (y-axis)

```r
with(average_daily_activity, plot(interval, average_activity, type = "l", main = "Average daily activity pattern", xlab = "Interval", ylab = "Number of steps"))
```

![plot of chunk plot_average_daily_activity](figures/plot_average_daily_activity.png) 

Which 5-minute interval, on average across all the days in the dataset, contains
the maximum number of steps?

```r
interval_with_max_activity <- average_daily_activity %.% arrange(desc(average_activity)) %.% head(1)
interval_with_max_activity
```

```
## Source: local data frame [1 x 2]
## 
##   interval average_activity
## 1      835            206.2
```
The 5-minute interval, on average across all the days in the dataset, with the
maximum number of steps is 835.

## Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA).
The presence of missing days may introduce bias into some calculations or summaries
of the data.

We calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

```r
total_number_missing <- sum(!complete.cases(activity), na.rm=TRUE)
total_number_missing
```

```
## [1] 2304
```
The total number of missing values in the dataset is 2304.

We create a new dataset that is equal to the original dataset but with the missing data filled in
with average steps for that 5-minute interval.

```r
activity_with_mean_steps_added <- left_join(activity, average_daily_activity)
```

```
## Joining by: "interval"
```

```r
activity_corrected <- activity_with_mean_steps_added[,c(1,2,3)]
for (i in 1:nrow(activity_corrected))
  if (is.na(activity_corrected[i,1])) activity_corrected[i,1] <- activity_with_mean_steps_added[i,4]
```

We create a dataset with the average number of steps taken per 5-minute interval,
averaged across all days (y-axis), using the original dataset with missing data filled.

```r
steps_per_day_corrected <- activity_corrected %.% group_by(date) %.% summarise(total_steps = sum(steps))
```

We make a histogram of the total number of steps taken each day, using the original
dataset with missing data filled.

```r
with(steps_per_day_corrected, hist(total_steps, main = "Total steps per day", xlab = ""))
```

![plot of chunk hist_steps_per_day_corrected](figures/hist_steps_per_day_corrected.png) 

We calculate and report the mean and median total number of steps taken per day,
using the original dataset with missing data filled.

```r
with(steps_per_day_corrected, summary(total_steps))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9820   10800   10800   12800   21200
```

These values doesn't differ from the estimates from the first part of the project.
It seems that excluding missing values or filling them doesn't affect the estimates.

## Are there differences in activity patterns between weekdays and weekends?

We create a new factor variable in the dataset with two levels - "weekday" and "weekend"
indicating whether a given date is a weekday or weekend day.

```r
for (i in 1:nrow(activity_corrected))
  if (any(c('Mon','Tue','Wed','Thu','Fri') == weekdays(as.Date(activity_corrected[i,2]), abbreviate = TRUE))) {
    activity_corrected[i,4] <- "weekday"    
  } else {
    activity_corrected[i,4] <- "weekend"    
  }
activity_corrected[,4] <- as.factor(activity_corrected[,4])
names(activity_corrected)[4] <- "weekday"
```

We create a dataset with the average number of steps taken per weekday and 5-minute interval,
averaged across all days (y-axis). Then we create others two dataset with rows filtered
for weekdays and weekend days.

```r
average_daily_activity_corrected <- activity_corrected %.% group_by(weekday, interval) %.% summarise(average_activity = mean(steps))
average_daily_activity_weekday <- average_daily_activity_corrected[average_daily_activity_corrected$weekday == "weekday",]
average_daily_activity_weekend <- average_daily_activity_corrected[average_daily_activity_corrected$weekday == "weekend",]
```

We make a panel plot containing a time series plot of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
par(mfrow=c(2,1),oma=c(5,5,5,0))
par(mar=c(2, 3, 3, 2))
with(average_daily_activity_weekend, plot(interval, average_activity, type = "l", main = "Weekend", xlab = "", ylab = ""))
par(mar=c(3, 3, 2, 2))
with(average_daily_activity_weekday, plot(interval, average_activity, type = "l", main = "Weekday", xlab = "", ylab = ""))
mtext("Average daily activity pattern", outer = TRUE, side = 3) 
mtext("Number of steps", outer = TRUE, side = 2)
mtext("Interval", outer = TRUE, side = 1) 
```

![plot of chunk plot_average_daily_activity_weekdays](figures/plot_average_daily_activity_weekdays.png) 
