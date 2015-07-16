# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
In order to complete all the analysis required in this assignment the dplyr 
library is required.


```r
library(dplyr);
```

The first thing that needs to be done is to unzip the activity.zip file which
contains the data which will be used in this assignment.


```r
unzip("activity.zip", overwrite = TRUE);
```

The next step is to get an understanding of what the data in this file looks
like. To do this the file will be read and then the first 10 rows will be 
displayed.


```r
activityData <- read.csv("activity.csv");
head(activityData, 10);
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

## What is mean total number of steps taken per day?
To determine the mean total number of steps taken per day the mean will be taken
of the data set grouped by day. 


```r
activityData_byDayMean <- activityData %>% group_by(date) %>% summarise_each(funs(mean))
activityData_byDayMean
```

```
## Source: local data frame [61 x 3]
## 
##          date    steps interval
## 1  2012-10-01       NA   1177.5
## 2  2012-10-02  0.43750   1177.5
## 3  2012-10-03 39.41667   1177.5
## 4  2012-10-04 42.06944   1177.5
## 5  2012-10-05 46.15972   1177.5
## 6  2012-10-06 53.54167   1177.5
## 7  2012-10-07 38.24653   1177.5
## 8  2012-10-08       NA   1177.5
## 9  2012-10-09 44.48264   1177.5
## 10 2012-10-10 34.37500   1177.5
## ..        ...      ...      ...
```

Now that the mean is calculated by day, a histogram can be created to look at 
the distribution of steps per day.


```r
hist(activityData_byDayMean$steps, main="Steps Histogram", ylab="Frequency", xlab="# of Steps per 5 mins", col="lightblue", labels=TRUE, ylim=c(0,20))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

From the histogram above it is clear that the highest level of frequency is in the 30-50 steps per 5 min period. 

Next a summary of the data will be considered with the focus being specifically on the values in steps.


```r
summary(activityData_byDayMean)
```

```
##          date        steps            interval   
##  2012-10-01: 1   Min.   : 0.1424   Min.   :1178  
##  2012-10-02: 1   1st Qu.:30.6979   1st Qu.:1178  
##  2012-10-03: 1   Median :37.3785   Median :1178  
##  2012-10-04: 1   Mean   :37.3826   Mean   :1178  
##  2012-10-05: 1   3rd Qu.:46.1597   3rd Qu.:1178  
##  2012-10-06: 1   Max.   :73.5903   Max.   :1178  
##  (Other)   :55   NA's   :8
```

With the NA values being excluded the mean for steps per day is 37.3826. The median is 37.3785

## What is the average daily activity pattern?
To answer the question of what the average daily activity pattern is, the data must
first be grouped by interval.


```r
activityData_byIntMean <- activityData %>% group_by(interval) %>% summarise_each(funs(mean(., na.rm=TRUE)))
activityData_byIntMean
```

```
## Source: local data frame [288 x 3]
## 
##    interval     steps date
## 1         0 1.7169811   31
## 2         5 0.3396226   31
## 3        10 0.1320755   31
## 4        15 0.1509434   31
## 5        20 0.0754717   31
## 6        25 2.0943396   31
## 7        30 0.5283019   31
## 8        35 0.8679245   31
## 9        40 0.0000000   31
## 10       45 1.4716981   31
## ..      ...       ...  ...
```

With the data now grouped, a time series plot can be created showing the average number of steps over time.
This provides a visual of the individuals activity levels throughout the day.


```r
plot.ts(x=activityData_byIntMean$interval, y=activityData_byIntMean$steps, ylim=c(0,210), type="l", main="Daily Activity Pattern", xlab="5 min Interval", ylab="Average # of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
