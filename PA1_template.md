# Reproducible Research: Peer Assessment 1
Author: Frederic Grenier (mitienka)  

## Loading and preprocessing the data  


```r
        activitydata <- read.csv(unz("activity.zip","activity.csv"))
```

## What is mean total number of steps taken per day?  


```r
        library(plyr)
        dailysteps <- ddply(activitydata,.(date),summarize,dailytotal=sum(steps))

        hist(dailysteps$dailytotal, breaks=20, main="Histogram of Total number of steps taken each day", 
             xlab="Daily number of steps")        
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
        meansteps <- mean(dailysteps$dailytotal,na.rm=TRUE)
        mediansteps <- median(dailysteps$dailytotal,na.rm=TRUE)
```

  The mean total number of steps taken per day is : **10766.19**  
  The median total number of steps taken per day is : **10765**  

## What is the average daily activity pattern?  


```r
        intervalsteps <- ddply(activitydata,.(interval),summarize,intervalmean=mean(steps,na.rm=TRUE))
        plot(intervalsteps$interval,intervalsteps$intervalmean,type="l",main="",xlab="Interval",ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
        intervalMaxAverage <- intervalsteps[intervalsteps$intervalmean==max(intervalsteps$intervalmean),]$interval
```

  The 5-minute interval containing the maximum number of steps, averaged across all days, is interval **835**  


## Imputing missing values  

### Determining the total number of rows with missing data  

```r
        compcases <- complete.cases(activitydata)
        incompleteRows <- length(compcases)-sum(compcases)
```
  There is 2304 rows with missing data.

### Filling strategy



## Are there differences in activity patterns between weekdays and weekends?
