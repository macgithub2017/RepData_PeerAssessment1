# Reproducible Research: Peer Assessment 1
# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
##### 1. Load the data (i.e. read.csv())

```r
if(!file.exists('activity.csv')){
    unzip('repdata_data_activity.zip')
}
actData <- read.csv('activity.csv')
```
-----

## What is mean total number of steps taken per day?

```r
SumStepsPD <- tapply(actData$steps, actData$date, sum, na.rm=TRUE)
```

##### 1. Make a histogram of the total number of steps taken each day

```r
qplot(SumStepsPD, xlab='Total steps per day', ylab='Frequency', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

##### 2. Calculate and report the mean and median total number of steps taken per day

```r
MeanStepsPD <- mean(SumStepsPD)
MedianStepsPD <- median(SumStepsPD)
```
* Mean: `9354.22950819672`
* Median:  `10395`

-----

## What is the average daily activity pattern?

```r
AveStepsPTB <- aggregate(x=list(MeanSteps=actData$steps), by=list(interval=actData$interval), FUN=mean, na.rm=TRUE)
```

##### 1. Make a time series plot

```r
ggplot(data=AveStepsPTB, aes(x=interval, y=MeanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
MostSteps <- which.max(AveStepsPTB$MeanSteps)
TimeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", AveStepsPTB[MostSteps,'interval'])
```

* Most Steps at: `8:35`

----

## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset 

```r
numMissingValues <- length(which(is.na(actData$steps)))
```

* Number of missing values: `2304`

##### 2. Devise a strategy for filling in all of the missing values in the dataset.
##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
actDataImputed <- actData
actDataImputed$steps <- impute(actData$steps, fun=mean)
```


##### 4. Make a histogram of the total number of steps taken each day 

```r
stepsBDImputed <- tapply(actDataImputed$steps, actDataImputed$date, sum)
qplot(stepsBDImputed, xlab='Total steps per day (Imputed)', ylab='Frequency', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##### ... and Calculate and report the mean and median total number of steps taken per day. 

```r
MeanStepsBDImputed <- mean(stepsBDImputed)
MedianStepsBDImputed <- median(stepsBDImputed)
```
* Mean (Imputed): `10766.1886792453`
* Median (Imputed):  `10766.1886792453`


----

## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
actDataImputed$dateType <-  ifelse(as.POSIXlt(actDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

##### 2. Make a panel plot containing a time series plot


```r
AveActDataImputed <- aggregate(steps ~ interval + dateType, data=actDataImputed, mean)

library(lattice)
xyplot(steps~interval |dateType, data = AveActDataImputed, 
       type = 'l',
       xlab = 'Interval',
       ylab = 'Number of Steps',
       layout = c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
