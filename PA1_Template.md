#Reproducible Research 
###Assignment 1
###Amine Moussa


```r
 library(knitr)
 opts_chunk$set(echo = TRUE)
 library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
 library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.2.5
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
 library(ggplot2)
```
###Loading and preprocessing the data
 

```r
 data <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character","integer"))
 
 data$date <- ymd(data$date)
 str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
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
 
###What is mean total number of steps taken per day?
 

```r
 steps <- data %>%
   filter(!is.na(steps)) %>%
   group_by(date) %>%
   summarize(steps = sum(steps)) %>%
   print
```

```
## Source: local data frame [53 x 2]
## 
##          date steps
##        (date) (dbl)
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```

```r
 ggplot(steps, aes(x = steps)) +
   geom_histogram(fill = "firebrick", binwidth = 1000) +
   labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
 mean_steps <- mean(steps$steps, na.rm = TRUE)
 median_steps <- median(steps$steps, na.rm = TRUE)
 mean_steps
```

```
## [1] 10766.19
```

```r
 median_steps
```

```
## [1] 10765
```
###What is the average daily activity pattern?


```r
 interval <- data %>%
   filter(!is.na(steps)) %>%
   group_by(interval) %>%
   summarize(steps = mean(steps))
 
 ggplot(interval, aes(x=interval, y=steps)) +
   geom_line(color = "firebrick")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
 interval[which.max(interval$steps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
##      (int)    (dbl)
## 1      835 206.1698
```

```r
 sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
 data_full <- data
 nas <- is.na(data_full$steps)
 avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
 data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]
 
 sum(is.na(data_full$steps))
```

```
## [1] 0
```

```r
 steps_full <- data_full %>%
   filter(!is.na(steps)) %>%
   group_by(date) %>%
   summarize(steps = sum(steps)) %>%
   print
```

```
## Source: local data frame [61 x 2]
## 
##          date    steps
##        (date)    (dbl)
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## ..        ...      ...
```

```r
 ggplot(steps_full, aes(x = steps)) +
   geom_histogram(fill = "firebrick", binwidth = 1000) +
   labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-2.png)

```r
 mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
 median_steps_full <- median(steps_full$steps, na.rm = TRUE)
 
 mean_steps_full
```

```
## [1] 10766.19
```

```r
 median_steps_full
```

```
## [1] 10766.19
```
 
###Are there differences in activity patterns between weekdays and weekends?
 

```r
 data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "Saturday" | weekdays(data_full$date) == "Sunday", "weekend", "weekday"))
 data_full$weektype <- as.factor(data_full$weektype)
 head(data_full)
```

```
##       steps       date interval weektype
## 1 1.7169811 2012-10-01        0  weekday
## 2 0.3396226 2012-10-01        5  weekday
## 3 0.1320755 2012-10-01       10  weekday
## 4 0.1509434 2012-10-01       15  weekday
## 5 0.0754717 2012-10-01       20  weekday
## 6 2.0943396 2012-10-01       25  weekday
```

```r
 interval_full <- data_full %>%
   group_by(interval, weektype) %>%
   summarise(steps = mean(steps))
 s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
   geom_line() +
   facet_wrap(~weektype, ncol = 1, nrow=2)
 print(s)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
 
