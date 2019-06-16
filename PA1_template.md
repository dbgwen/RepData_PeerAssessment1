---
title: 'Reproducible Research: Peer Assessment 1'
author: "Deborah Passey"
date: "6/15/2019"
  html_document:
    keep_md: true

---

# Peer Assignment 1 - RMarkdown File  



## Loading and preprocessing the data  

  ### Loading Data  


```r
        library("data.table")
        path <- getwd()
        download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = paste(path, "dataFiles.zip",sep = "/"))
        unzip(zipfile = "dataFiles.zip")
        activity <- read.csv("activity.csv")
```
  
  ### Preprocessing the Data  
  

```r
    summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
    summary(activity$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

```r
    summary(activity$interval)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0   588.8  1177.5  1177.5  1766.2  2355.0
```

```r
    library(lubridate)
    day <- function(x) format(as.Date(x), "%A", na.rm=TRUE)
    activity$day <- day(activity$date)
```
  
## What is mean total number of steps taken per day?  
  

```r
    library(dplyr)
    totalsteps <- summarise(group_by(activity, date), totalsteps = sum(steps, na.rm=TRUE))
    print(totalsteps)
```

```
## # A tibble: 61 x 2
##    date       totalsteps
##    <fct>           <int>
##  1 2012-10-01          0
##  2 2012-10-02        126
##  3 2012-10-03      11352
##  4 2012-10-04      12116
##  5 2012-10-05      13294
##  6 2012-10-06      15420
##  7 2012-10-07      11015
##  8 2012-10-08          0
##  9 2012-10-09      12811
## 10 2012-10-10       9900
## # ... with 51 more rows
```

```r
    meansteps <- as.integer(mean(totalsteps$totalsteps), na.rm=TRUE)
    mediansteps <- as.integer(median(totalsteps$totalsteps))
```
  
  ### Histogram of The Total Steps Taken Each Day  

```r
    library(lattice)
    histogram(~totalsteps, data=totalsteps, main="Histogram of Total Steps Taken Each Day", col="blue", xlab="Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
    
  ### Mean Steps Taken Each Day  

```r
    print(meansteps)
```

```
## [1] 9354
```
    
  ### Median Steps Taken Each Day

```r
    print(mediansteps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?  

```r
library(dplyr)
    intervals <- summarise(group_by(activity, interval), meansteps = mean(steps, na.rm=TRUE))
      par(mar=c(4,4,4,4), bg="azure", family="HersheySans", lwd=0.25)
    with(intervals, plot(meansteps, type="l", xlab="Interval", ylab="Average Steps per Day", main="Average Daily Pattern"))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

  ### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
      library(dplyr)  
      top_n(intervals, 1, meansteps)
```

```
## # A tibble: 1 x 2
##   interval meansteps
##      <int>     <dbl>
## 1      835      206.
```
  
The 835th interval has the maximum number of steps.   
  
## Imputing Missing Values  
  ### Total Number of Missing Variables  

```r
    library(dplyr)
    missing <- activity %>%
                filter(is.na(steps))
    table(missing)
```

```
## < table of extent 0 x 61 x 288 x 6 >
```
      
      
  ### Looking at Missingness Pattern  

```r
    library(mice)
    md.pattern(activity)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```
##       date interval day steps     
## 15264    1        1   1     1    0
## 2304     1        1   1     0    1
##          0        0   0  2304 2304
```
      
  ### Imputing Missing Values Using Predictive Mean Matching

```r
    imputed_data <- mice(activity, method="mean")
```

```
## 
##  iter imp variable
##   1   1  steps
##   1   2  steps
##   1   3  steps
##   1   4  steps
##   1   5  steps
##   2   1  steps
##   2   2  steps
##   2   3  steps
##   2   4  steps
##   2   5  steps
##   3   1  steps
##   3   2  steps
##   3   3  steps
##   3   4  steps
##   3   5  steps
##   4   1  steps
##   4   2  steps
##   4   3  steps
##   4   4  steps
##   4   5  steps
##   5   1  steps
##   5   2  steps
##   5   3  steps
##   5   4  steps
##   5   5  steps
```

```
## Warning: Number of logged events: 26
```

```r
    completedData <- complete(imputed_data,1)
    summary(completedData)
```

```
##      steps                date          interval          day           
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8   Class :character  
##  Median :  0.00   2012-10-03:  288   Median :1177.5   Mode  :character  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5                     
##  3rd Qu.: 37.38   2012-10-05:  288   3rd Qu.:1766.2                     
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0                     
##                   (Other)   :15840
```

```r
    summary(activity)
```

```
##      steps                date          interval          day           
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8   Class :character  
##  Median :  0.00   2012-10-03:  288   Median :1177.5   Mode  :character  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5                     
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2                     
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0                     
##  NA's   :2304     (Other)   :15840
```
  
  ### Histogram of Imputed Data  

```r
    library(dplyr)
    intervals <- summarise(group_by(completedData, interval), meansteps = mean(steps, na.rm=TRUE))
       library(lattice)
    histogram(~totalsteps, data=totalsteps, main="Histogram of Total Steps Taken Each Day - Imputed Data", col="green", xlab="Steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
    
    
  ### Mean and Median Steps of Imputed Data  

```r
     library(dplyr)
    totalsteps2 <- summarise(group_by(completedData, date), totalsteps = sum(steps, na.rm=TRUE))
    print(totalsteps)
```

```
## # A tibble: 61 x 2
##    date       totalsteps
##    <fct>           <int>
##  1 2012-10-01          0
##  2 2012-10-02        126
##  3 2012-10-03      11352
##  4 2012-10-04      12116
##  5 2012-10-05      13294
##  6 2012-10-06      15420
##  7 2012-10-07      11015
##  8 2012-10-08          0
##  9 2012-10-09      12811
## 10 2012-10-10       9900
## # ... with 51 more rows
```

```r
    meansteps2 <- as.integer(mean(totalsteps2$totalsteps), na.rm=TRUE)
    mediansteps2 <- as.integer(median(totalsteps2$totalsteps))

    print(meansteps2)
```

```
## [1] 10766
```

```r
    print(mediansteps2)
```

```
## [1] 10766
```
  
  ### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?      

Overall, the imputed data made the mean and median equal. in the original data set the mean was lower than the median.  

## Are there differences in activity patterns between weekdays and weekends?  
    ### Create "weekday" and "weekend" variables  

```r
    activity$date <- as.Date(activity$date)
    wkdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
    activity$weekday <- factor((weekdays(activity$date) %in% wkdays), 
                       levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))  
    summary(activity$weekday)
```

```
## weekend weekday 
##    4608   12960
```
  
    ### Create Panel Plot of Activity Patterns by Weekday and Weekend  

```r
  library(dplyr)
    intervals2 <- summarise(group_by(activity, interval, weekday), meansteps = mean(steps, na.rm=TRUE))
    library(lattice)
    xyplot(meansteps ~ interval | weekday, data = intervals2, type="l", xlab="Interval", ylab="Steps", main="Activity Patterns by Weekday and Weekend")    
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
  
There are differences in the activity patterns when looking at the weekday and weekend plots.
