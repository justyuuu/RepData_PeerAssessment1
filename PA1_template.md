---
title: "reproducible reasearch project2"
output: html_document
---


```r
knitr::opts_chunk$set(echo = TRUE)
```

## loading and processing the data


```r
setwd("/Users/justyuuu/Desktop/data science/reproducible research")
if(!file.exists("./data"))dir.create(".data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./data/repdata_data_activity.zip", method = "curl")
unzip("./data/repdata_data_activity.zip",exdir = "./data")
setwd("/Users/justyuuu/Desktop/data science/reproducible research/data")
activity <- read.csv("activity.csv")
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

## mean total number of steps taken per day 


```r
totalsteps <- tapply(activity$steps, activity$date, sum)
hist(totalsteps,main = "total number of steps taken perday")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
mean(totalsteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(totalsteps, na.rm = TRUE)
```

```
## [1] 10765
```

imputing missing values


```r
averagesteps <- aggregate(steps~interval, data = activity, FUN=mean, na.rm=TRUE)
plot(averagesteps$interval, averagesteps$steps,type = "l",xlab = "steps", ylab = "interval", main = "average number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
averagesteps$interval[which.max(averagesteps$steps)]
```

```
## [1] 835
```

## imputing missing values


```r
sum(is.na(activity))
```

```
## [1] 2304
```

```r
nomissing <- activity
avemissing  <- function(nmsteps, nminterval){
        if(is.na(nmsteps))
                new <- averagesteps$steps[which(averagesteps$interval==nminterval)]
        else
               new <- nmsteps
        return(new)
}
nomissing$steps <- mapply(avemissing, nomissing$steps, nomissing$interval)
hist(tapply(nomissing$steps, nomissing$date, sum),xlab="steps", main ="total number of steps per day without missing value")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
 
## Are there differences in activity patterns between weekdays and weekends


```r
nomissing$date <- as.Date(nomissing$date)
library(ggplot2)
isweekday <- function(x){
        if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
                return("weekend")
        else
                return("weekday")
        }

nomissing$daytype <- mapply(isweekday, nomissing$date)
ggplot(nomissing) +facet_wrap(~daytype, ncol = 1, nrow = 2) + geom_line(aes(x=interval, y=steps))+ labs(title = "average steps by type of day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
