---
title: "Reproducible Research: Peer Assessment 1"
author: "Deborah Passey"
date: "6/15/2019"
output: html_document
---

# Peer Assignment 1 - RMarkdown File  



## Loading and preprocessing the data  

  ### Loading Data  


```r
        library("data.table")
```

```
## data.table 1.12.2 using 2 threads (see ?getDTthreads).  Latest news: r-datatable.com
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```r
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
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked _by_ '.GlobalEnv':
## 
##     day
```

```
## The following objects are masked from 'package:data.table':
## 
##     hour, isoweek, mday, minute, month, quarter, second, wday,
##     week, yday, year
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
    day <- function(x) format(as.Date(x), "%A", na.rm=TRUE)
    activity$day <- day(activity$date)
```
  
## What is mean total number of steps taken per day?  
  
  ## Histogram of The Total Steps Taken Each Day  

```r
    library(lattice)
    histogram(~steps | day, data=activity, type="count")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)
    
  ### Mean Steps Taken Each Day  

```r
 library(dplyr)
    meansteps <- activity %>%
    group_by(day) %>%
    summarise(meansteps = mean(steps, na.rm=TRUE)) 
    print(meansteps)
```

```
## # A tibble: 7 x 2
##   day       meansteps
##   <chr>         <dbl>
## 1 Friday         42.9
## 2 Monday         34.6
## 3 Saturday       43.5
## 4 Sunday         42.6
## 5 Thursday       28.5
## 6 Tuesday        31.1
## 7 Wednesday      40.9
```
    
  ### Median Steps Taken Each Day

```r
    library(dplyr)
    mediansteps <- activity %>%
    group_by(day) %>%
    summarise(mediansteps = median(steps, na.rm=TRUE))
    print(mediansteps)
```

```
## # A tibble: 7 x 2
##   day       mediansteps
##   <chr>           <dbl>
## 1 Friday              0
## 2 Monday              0
## 3 Saturday            0
## 4 Sunday              0
## 5 Thursday            0
## 6 Tuesday             0
## 7 Wednesday           0
```

## What is the average daily activity pattern?  

```r
      par(mar=c(4,4,4,4), bg="azure", family="HersheySans", lwd=0.25)
    with(activity, plot(interval, steps, type="l", xlab="Interval", xlim=c(0, 2500), ylab="Average Steps per Day"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

  ### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
      library(dplyr)  
      max_step <- activity %>%
      group_by(day, interval) %>%
      summarise(max_step = max(steps))
      
      par(mar=c(4,4,4,4), bg="azure", family="HersheySans", lwd=0.25)
      with(max_step, plot(interval, max_step, type="l", xlab="Interval", xlim=c(0, 2500), ylab="Maximum Number of Steps"))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
  
The interval between 500-1000 contains the the maximum number of steps, when looking across all days in the dataset.  
  
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
    
    
  ### Imputing Missing Values Using Predictive Mean Matching  

```r
    library(mice)
    md.pattern(activity)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```
##       date interval day steps     
## 15264    1        1   1     1    0
## 2304     1        1   1     0    1
##          0        0   0  2304 2304
```

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
  
  ### Histogram of Imputed Steps  

```r
    histogram(~steps | day, data=completedData, type="count")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)
    
    
  ### Mean and Median Steps of Imputed Data  

```r
    meansteps2 <- completedData %>%
    group_by(day) %>%
    summarise(meansteps2 = mean(steps, na.rm=TRUE))

    mediansteps2 <- completedData %>%
    group_by(day) %>%
    summarise(mediansteps2 = median(steps))
    
    print(meansteps2)
```

```
## # A tibble: 7 x 2
##   day       meansteps2
##   <chr>          <dbl>
## 1 Friday          41.7
## 2 Monday          35.2
## 3 Saturday        42.8
## 4 Sunday          42.0
## 5 Thursday        29.5
## 6 Tuesday         31.1
## 7 Wednesday       40.5
```

```r
    print(mediansteps2)
```

```
## # A tibble: 7 x 2
##   day       mediansteps2
##   <chr>            <dbl>
## 1 Friday               0
## 2 Monday               0
## 3 Saturday             0
## 4 Sunday               0
## 5 Thursday             0
## 6 Tuesday              0
## 7 Wednesday            0
```
  
  ### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?      

Overall, the imputed data did not change the mean or median. However, when looking a the mean and median for each day, there are slight differences between the data set with missing values and the imputed data set. These differences were very minimal. When looking at the 3rd quartile, there are more steps taken for the imputed data set. This higlights the differeces:
  

```r
  print(meansteps)
```

```
## # A tibble: 7 x 2
##   day       meansteps
##   <chr>         <dbl>
## 1 Friday         42.9
## 2 Monday         34.6
## 3 Saturday       43.5
## 4 Sunday         42.6
## 5 Thursday       28.5
## 6 Tuesday        31.1
## 7 Wednesday      40.9
```

```r
  print(meansteps2)
```

```
## # A tibble: 7 x 2
##   day       meansteps2
##   <chr>          <dbl>
## 1 Friday          41.7
## 2 Monday          35.2
## 3 Saturday        42.8
## 4 Sunday          42.0
## 5 Thursday        29.5
## 6 Tuesday         31.1
## 7 Wednesday       40.5
```

```r
  print(mediansteps)
```

```
## # A tibble: 7 x 2
##   day       mediansteps
##   <chr>           <dbl>
## 1 Friday              0
## 2 Monday              0
## 3 Saturday            0
## 4 Sunday              0
## 5 Thursday            0
## 6 Tuesday             0
## 7 Wednesday           0
```

```r
  print(mediansteps2)
```

```
## # A tibble: 7 x 2
##   day       mediansteps2
##   <chr>            <dbl>
## 1 Friday               0
## 2 Monday               0
## 3 Saturday             0
## 4 Sunday               0
## 5 Thursday             0
## 6 Tuesday              0
## 7 Wednesday            0
```
  
  
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
    library(lattice)
    xyplot(steps ~ interval | weekday, data = activity, type="l")    
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)
  
    
There are differences in the activity patterns when looking at the weekday and weekend plots.

library(knitr)
knit2html(PA1_template.html)
