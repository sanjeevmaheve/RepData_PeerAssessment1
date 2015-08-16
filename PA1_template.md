# Reproducible Research: Peer Assessment 1


```r
library(knitr)
# Setting the global options.
opts_chunk$set(echo = TRUE, results = "asis")
```

## Loading and preprocessing the data

```r
library(dplyr)
```

```
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

```r
# unzip input file if not done already.
if(!file.exists("./activity.csv")) {
    unzip ("activity.zip", exdir = "./")
}
# Load the data as per column type information.
df <- read.csv("activity.csv", 
                colClasses = c("numeric", "Date", "numeric"))
# Process/transform the data into a format suitable for analysis.
monitoringData <- df %>% filter (complete.cases(.))
```

## What is mean total number of steps taken per day?

```r
library(datasets)
# Group the dataset using date for summarizing.
# The group_by() function is used to generate summary statistics 
# from the data frame within strata defined by a variable.
stats1 <- group_by(monitoringData, date) %>% 
    summarize(sumTotalSteps = sum(steps, na.rm = FALSE))
# Plot the histogram showing the frequency of number of steps.
hist(stats1$sumTotalSteps,
     col = "red",
     ylab = 'Frequency',
     xlab = 'Number of steps per day',
     main = "Mean total number of steps taken per day")
grid()
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
# Calculate and report the mean and median of the total number of 
# steps taken per day.
mean(stats1$sumTotalSteps)
```

[1] 10766.19

```r
median(stats1$sumTotalSteps)
```

[1] 10765

## What is the average daily activity pattern?

```r
# Group the dataset using date for summarizing.
# The group_by() function is used to generate summary statistics 
# from the data frame within strata defined by a variable.
stats2 <- group_by(monitoringData, interval) %>% 
    summarize(meanTotalSteps = mean(steps, na.rm = FALSE))
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
plot(stats2$interval, 
     stats2$meanTotalSteps, col='red',
     main = "Average number of steps taken, averaged across all days",
     ylab='Average number of steps', 
     xlab='Intervals (at the spacing of 5 mins)', type='l')
grid()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
filter(stats2, meanTotalSteps == max(stats2$meanTotalSteps))$interval
```

[1] 835

## Imputing missing values

```r
# Note that there are a number of days/intervals where there are missing 
# values (coded as NA). The presence of missing days may introduce bias 
# into some calculations or summaries of the data.

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
library(scales)
sum(!complete.cases(df))
```

[1] 2304

```r
# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use 
# the mean/median for that day, or the mean for that 5-minute interval, etc.
newDataset <- df[is.na(df$steps),]
uniqueInterval <- unique(newDataset$interval)
newDataset$steps <- stats2[stats2$interval==uniqueInterval,]$meanTotalSteps
# Remove the row.names inserted in previous step.
rownames(newDataset) <- NULL
# Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
df[is.na(df$steps),]$steps <- newDataset$steps
stats3 <- group_by(df, date) %>% 
    summarize(sumTotalSteps = sum(steps, na.rm = FALSE))
# Plot the histogram showing the frequency of number of steps.
#par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
hist(stats3$sumTotalSteps,
     col = "skyblue",
     ylab = 'Frequency',
     xlab = 'Number of steps per day', border=F,
     main = "Mean total number of steps taken per day")
hist(stats1$sumTotalSteps,
     col = "red",
     ylab = 'Frequency',
     xlab = 'Number of steps per day', border=F, add=T,
     main = "Mean total number of steps taken per day")
grid();box()
legend("topright", c("Imputed", "Original"), 
       col=c("skyblue", "red"), lwd=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
# Calculate and report the mean and median of the total number of 
# steps taken per day.
mean(stats3$sumTotalSteps)
```

[1] 10766.19

```r
median(stats3$sumTotalSteps)
```

[1] 10766.19

The mean value has no change (as expected) after imputing missing data because we put the mean value for that particular 5-min interval. The median value differs.

## Are there differences in activity patterns between weekdays and weekends?

```r
# For this part the weekdays() function may be of some help here. Use the 
# dataset with the filled-in missing values for this part.

# Create a new factor variable in the dataset with two levels – “weekday” 
# and “weekend” indicating whether a given date is a weekday or weekend day.
weekdayList <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
df <- mutate (df, dayType=factor((weekdays(date) %in% weekdayList), 
                                 levels=c(FALSE, TRUE), 
                                 labels=c('weekend', 'weekday')))
# Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekdays or weekends (y-axis).
library(lattice)
stats4 <- aggregate(steps ~ interval + dayType, df, mean)

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
xyplot(steps ~ interval | dayType, 
       data=stats4,
       main="Average number of steps taken across all weekday/weekend",
       xlab="Intervals (at the spacing of 5 mins)", 
       ylab="Average number of steps", 
       layout=c(1,2), 
       type=c('l','g'))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
