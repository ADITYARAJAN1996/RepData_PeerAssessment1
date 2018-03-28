Reproducible Research: Peer Assessment 1
========================================

########################################## 

Loading and preprocessing the data
----------------------------------

####################################### 

*1. Load the data*

    acti_data <- read.csv("activity.csv")
    summary(acti_data)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

    head(acti_data)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

####################################################################################### 

What is mean total number of steps taken per day?
-------------------------------------------------

*1.Calculate the total number of steps taken per day*

    steps <- aggregate(acti_data$steps, by = list(Date = acti_data$date), FUN = sum)
    library(ggplot2)
    names(steps)[names(steps) == "x"] <- "Total"
    temp <- as.Date(steps$Date, "%Y-%m-%d")
    steps$Date <- format(temp, format = "%m-%d")
    head(steps)

    ##    Date Total
    ## 1 10-01    NA
    ## 2 10-02   126
    ## 3 10-03 11352
    ## 4 10-04 12116
    ## 5 10-05 13294
    ## 6 10-06 15420

*2. Make a histogram of the total number of steps taken each day*

    hist1 <- ggplot(data = na.omit(steps), aes(Total)) + 
        geom_histogram(binwidth = 1500, colour = "white") +
        xlab("Total Number of Steps Taken Each Day") +
        ylab("Count") +
        ggtitle("Histogram of the Total Number of Steps Taken Each Day")
    print(hist1)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

*3. Calculate and report the mean and median total number of steps taken
per day*

    mean(na.omit(steps$Total))

    ## [1] 10766.19

    median(na.omit(steps$Total))

    ## [1] 10765

######################################################################################## 

What is the average daily activity pattern?
-------------------------------------------

*1. Make a time series plot*

    five_min_steps <- aggregate(steps ~ interval, data = acti_data, FUN =mean)
    TimeSeries1 <- ggplot(data = five_min_steps, aes(x = interval, y = steps)) + 
        geom_line() +
        xlab("Time Intervals (5 Minutes is an unit)") + 
        ylab("Total Number of Steps") +
        ggtitle("Average Number of Steps Taken of the 5-Minute Interval")
    print(TimeSeries1) 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

*2. Which 5-minute interval, on average across all the days in the
dataset, contains the maximum number of steps?*

    head(five_min_steps)

    ##   interval     steps
    ## 1        0 1.7169811
    ## 2        5 0.3396226
    ## 3       10 0.1320755
    ## 4       15 0.1509434
    ## 5       20 0.0754717
    ## 6       25 2.0943396

    five_min_steps[which(five_min_steps$steps == max(five_min_steps$steps)),]

    ##     interval    steps
    ## 104      835 206.1698

########################################################################################### 

Imputing missing values
-----------------------

*1. Calculate and report the total number of missing values in the
dataset*

    sapply(X = acti_data, FUN = function(x) sum(is.na(x)))

    ##    steps     date interval 
    ##     2304        0        0

*2. Devise a strategy for filling in all of the missing values in the
dataset.*

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    replace_with_mean <- function(num) replace(num, is.na(num), mean(num, na.rm = TRUE))
    meanday <- (acti_data %>% group_by(interval) %>% mutate(steps = replace_with_mean(steps)))
    head(meanday)

    ## # A tibble: 6 x 3
    ## # Groups:   interval [6]
    ##    steps date       interval
    ##    <dbl> <fct>         <int>
    ## 1 1.72   2012-10-01        0
    ## 2 0.340  2012-10-01        5
    ## 3 0.132  2012-10-01       10
    ## 4 0.151  2012-10-01       15
    ## 5 0.0755 2012-10-01       20
    ## 6 2.09   2012-10-01       25

    sum(is.na(meanday))

    ## [1] 0

*3. Create a new dataset that is equal to the original dataset but with
the missing data filled in*

    new_dataset <- as.data.frame(meanday)
    head(new_dataset)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

    summary(new_dataset)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##                   (Other)   :15840

*4. Make a histogram of the total number of steps taken each day and
Calculate and report the mean and median total number of steps taken per
day.*

    new_steps <- aggregate(new_dataset$steps, by = list(new_dataset$date), FUN = sum)
    names(new_steps)[names(new_steps) == "x"] <- "Total"
    names(new_steps)[names(new_steps) == "Group.1"] <- "Date"
    hist2 <- ggplot(data = new_steps, aes(Total)) + 
        geom_histogram(binwidth = 1500, colour = "white") +
        xlab("Total Number of Steps Taken Each Day") +
        ylab("Count") +
        ggtitle("Histogram of the Total Number of Steps Taken Each Day with New Version Dataset")
    print(hist2)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

Now, we compare two plots.

    library(grid)
    library(gridExtra)

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    grid.arrange(hist1, hist2, ncol = 2)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

Compare the mean and median.

    mean(na.omit(steps$Total))

    ## [1] 10766.19

    median(na.omit(steps$Total))

    ## [1] 10765

    mean(new_steps$Total)

    ## [1] 10766.19

    median(new_steps$Total)

    ## [1] 10766.19

########################################################################################### 

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

*1. Create a new factor variable in the dataset with two levels -
"weekday" and "weekend" indicating whether a given date is a weekday or
weekend day.*

    new_dataset$WeekendOrWeekday <- ifelse(weekdays(as.Date(new_dataset$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")
    head(new_dataset)

    ##       steps       date interval WeekendOrWeekday
    ## 1 1.7169811 2012-10-01        0          Weekday
    ## 2 0.3396226 2012-10-01        5          Weekday
    ## 3 0.1320755 2012-10-01       10          Weekday
    ## 4 0.1509434 2012-10-01       15          Weekday
    ## 5 0.0754717 2012-10-01       20          Weekday
    ## 6 2.0943396 2012-10-01       25          Weekday

*2. Make a panel plot containing a time series plot*

    new_dataset <- (new_dataset %>% group_by(interval, WeekendOrWeekday) %>% summarise(Mean = mean(steps)))
    ggplot(new_dataset, mapping = aes(x = interval, y = Mean)) + geom_line() +
        facet_grid(WeekendOrWeekday ~.) + xlab("Interval") + ylab("Mean of Steps") +
        ggtitle("Comparison of Average Number of Steps in Each Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-14-1.png)
