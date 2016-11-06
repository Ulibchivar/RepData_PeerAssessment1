PA1\_template
================

Reading DATA into R
-------------------

Looking at data
---------------

### Want to see just fist few rows and check if we have NA's, and how many if we do.

``` r
echo = TRUE
head(data)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
summary(data)
```

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

### Reminder what the variables are, taken from course site

The variables included in this dataset are:

*steps- Number of steps taking in a 5-minute interval (missing values are coded as NA) *date- The date on which the measurement was taken in YYYY-MM-DD format \*interval- Identifier for the 5-minute interval in which measurement was taken

Calculate steps per day and visualize it
----------------------------------------

``` r
echo = TRUE
ss<- aggregate(data[,c("steps"),drop=FALSE],by=list(Date=data$date),FUN=sum,na.rm=FALSE)
ss$steps<-as.numeric(ss$steps)
hist(ss$steps, xlab="Number of Steps", main="Histogram of steps per day", col="red")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

Calculate mean and median with simple summary fuction
-----------------------------------------------------

``` r
echo = TRUE
summary(ss)
```

    ##          Date        steps      
    ##  2012-10-01: 1   Min.   :   41  
    ##  2012-10-02: 1   1st Qu.: 8841  
    ##  2012-10-03: 1   Median :10765  
    ##  2012-10-04: 1   Mean   :10766  
    ##  2012-10-05: 1   3rd Qu.:13294  
    ##  2012-10-06: 1   Max.   :21194  
    ##  (Other)   :55   NA's   :8

Time series
-----------

``` r
aggint<- aggregate(data[,c("steps"),drop=FALSE],by=list(interval_avg=data$interval), FUN=mean,na.rm=TRUE)
plot(aggint$interval_avg,aggint$steps, xlab="Intervals", ylab="Steps", main="Average steps per interval")
lines(aggint$interval_avg,aggint$steps)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

Maximum number of steps
-----------------------

``` r
echo = TRUE
aggint[which.max(aggint$steps),]
```

    ##     interval_avg    steps
    ## 104          835 206.1698

Create new data table with joining original table to average per interval table
-------------------------------------------------------------------------------

``` r
echo = TRUE
names(aggint) <- c('interval_avg','Steps_avg')
 data_new<- merge(data, aggint, by.x="interval", by.y="interval_avg", all.x=TRUE)  

  for(i in 1:nrow(data_new)) {
    
     if(is.na(data_new$steps[i])) {
        
        data_new$steps[i] <- data_new$Steps_avg[i]
}}
```

Visualize new data with missing values replaced
-----------------------------------------------

``` r
hist(data_new$steps, xlab="Number of Steps", main="Histogram of steps per day with replaced values", col="green")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-7-1.png)

Change formatting in dates and add weekday names
------------------------------------------------

``` r
echo = TRUE
weekend_weekday <- data_new
weekend_weekday$day_of_week2 <- as.Date(weekend_weekday$date)
weekend_weekday$day_of_week <- weekdays(weekend_weekday$day_of_week2)
weekend <- subset(weekend_weekday,day_of_week == c("Saturday","Sunday"))
 weekdays <- subset(weekend_weekday,day_of_week != "Saturday" & day_of_week != "Sunday")
 weekend_interval_steps <- aggregate(steps ~ interval, data = weekend, mean, na.rm=TRUE)
 weekdays_interval_steps <- aggregate(steps ~ interval, data = weekdays, mean, na.rm=TRUE)
 weekend_interval_steps$weekend_flag <- "Weekend"
 weekdays_interval_steps$weekend_flag <- "Weekday"
  week_combined <- rbind(weekend_interval_steps,weekdays_interval_steps)
 week_combined$weekend_flag <- factor(week_combined$weekend_flag)
 
 
 library("lattice")
```

    ## Warning: package 'lattice' was built under R version 3.3.2

``` r
 xyplot(steps~interval | weekend_flag, data = week_combined,
        main = "Comparison of weekend and weekday steps by time of day",
        layout = c(1,2), 
        type = "l",
 )
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-1.png)
