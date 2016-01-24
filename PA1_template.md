
##Loading and preprocessing the data


```r
library(data.table)
activity_dt<-data.table(read.csv("activity.csv"))
activity_dt$date<-as.POSIXct(activity_dt$date,"%Y-%m-%d")
activity_dt$interval<-as.factor(activity_dt$interval)
```

##Total number of steps taken per day


Calculation the total number of steps taken per day

```r
steps_taken_per_day<-tapply(activity_dt$steps,activity_dt$date,sum, na.rm=TRUE)
```

Histogram of the total number of steps taken each day

```r
hist(steps_taken_per_day, main="Total number of steps taken per day", xlab="steps",breaks=10)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Mean and median of the total number of steps taken per day

```r
mean(steps_taken_per_day)
```

```
## [1] 9354.23
```

```r
median(steps_taken_per_day)
```

```
## [1] 10395
```



##The average daily activity pattern
Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avg_steps_per_interval<-tapply(activity_dt$steps,activity_dt$interval,mean, na.rm=TRUE)
plot(names(avg_steps_per_interval),avg_steps_per_interval,xlab="5-minute interval", ylab="average number of steps", main="average daily activity",type = "l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 


5-minute interval, on average across all the days in the dataset, contains the maximum number of steps


```r
names(avg_steps_per_interval)[which.max(avg_steps_per_interval)]
```

```
## [1] "835"
```

##Imputing missing values

The total number of missing values in the dataset 

```r
nrow(activity_dt[is.na(activity_dt$steps),])
```

```
## [1] 2304
```

I use the mean for that 5-minute interval to Imputing missing values.

```r
activity_dt[,interval_mean:=mean(steps,na.rm=TRUE), by=interval]
NA_row<-is.na(activity_dt$steps)
activity_dt$new_steps<-activity_dt$steps
activity_dt$new_steps[NA_row]<-activity_dt$interval_mean[NA_row]
```


Create a new dataset that is equal to the original dataset but with the missing data filled in


Two histograms of the total number of steps taken each day and Calculate.  Befor and after.

```r
activity_dt_new<-activity_dt[,c("date","interval","new_steps"), with=FALSE]
steps_taken_per_day_new<-tapply(activity_dt_new$new_steps,activity_dt_new$date,sum, na.rm=TRUE)
par(mfrow=c(2,1),mar=c(4,4,2,1))
hist(steps_taken_per_day, main="Total number of steps taken per day without NA", xlab="steps",breaks=10)
hist(steps_taken_per_day_new, main="NEW Total number of steps taken per day  
     after missing values were imputed", xlab="new steps",breaks=10)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

mean and median total number of steps taken per day

```r
mean(steps_taken_per_day_new)
```

```
## [1] 10766.19
```

```r
median(steps_taken_per_day_new)
```

```
## [1] 10766.19
```
Values differ from the estimates from the first part of the assignment



## The differences in activity patterns between weekdays and weekends

```r
activity_dt_new[,day_name:=weekdays(date,TRUE)]
day_name=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
week_part=c(rep("weekday",5),rep("weekend",2))
week_table<-data.table(day_name,week_part)
setkey(activity_dt_new,day_name)
setkey(week_table,day_name)
activity_dt_new<-merge(x = activity_dt_new, y = week_table, by = "day_name", all.x = TRUE)
activity_dt_new$week_part<-as.factor(activity_dt_new$week_part)
```


The panel plot containing a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
weekday<-activity_dt_new[activity_dt_new$week_part=="weekday"]
weekend<-activity_dt_new[activity_dt_new$week_part=="weekend"]

avg_steps_per_interval_weekday=tapply(weekday$new_steps,weekday$interval,mean, na.rm=TRUE)
avg_steps_per_interval_weekend=tapply(weekend$new_steps,weekend$interval,mean, na.rm=TRUE)

par(mfrow=c(2,1),mar=c(4,4,2,1))
plot(names(avg_steps_per_interval_weekday),avg_steps_per_interval_weekday,xlab="5-minute interval", ylab="average number of steps", main="Weekday average daily activity",type = "l")
plot(names(avg_steps_per_interval_weekday),avg_steps_per_interval_weekend,xlab="5-minute interval", ylab="average number of steps", main="Weekend average daily activity",type = "l")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 
