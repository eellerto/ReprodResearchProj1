library(dplyr)
library(ggplot2)
library(tidyr)
data_full <- read.csv("/Users/elaine/OneDrive - SAGE Publishing/Laptop/Courses/Data_Science_course_JohnsHopkins/Reproducible/ReprodResearchProj1/activity.csv", header=T)
data<- data[!is.na(data$steps),]
steps_per_day <- aggregate(steps ~ date, data, FUN=sum)

#Histogram of steps per day

g <- ggplot(steps_per_day, aes(x=steps, fill="red" ))
g+ geom_histogram(binwidth=600, col="black") + labs(x="Totals of Steps per Day", 
                                       title="Histogram of Steps per Day") + 
                                        theme(legend.position = "none")
  
median(steps_per_day$steps)
round(mean(steps_per_day$steps))


#What is the average daily activity pattern?
steps_per_interval <- aggregate(steps ~ interval, data, FUN=mean)
g1 <- ggplot(steps_per_interval, aes(interval, steps)) 

g1 + geom_line(col="blue") + labs(x="5 Minute Intervals", y="Average Steps", 
                        title="Average # of Steps Over All Intervals")       
#Which interval has max steps
steps_per_interval[which.max(steps_per_interval$steps), ]

#Imputing missing values
#total number of missing values
total_NA <- sum(is.na(data_full))

#replace NAs with zeros
data_full[is.na(data_full)] = 0

#Make a histogram of the total number of steps taken each day 
data_full_steps<- aggregate(steps ~ date, data_full, FUN=sum)

g2 <- ggplot(data_full_steps, aes(x=steps))
g2 + geom_histogram(binwidth = 800, fill="blue") + labs(x="Totals of Steps per Day", 
                                          title="Histogram of Steps per Day with no NAs") + 
        theme(legend.position = "none")

#and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
mean(data_full_steps$steps)
median(data_full_steps$steps)

#difference from original mean and median

#with NAS
median(steps_per_day$steps)
round(mean(steps_per_day$steps))
#without NAs
mean(data_full_steps$steps)
median(data_full_steps$steps)

#the zeros changed the mean and median
#Are there differences in activity patterns between weekdays and weekends?
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
#indicating whether a given date is a weekday or weekend day.

library(lubridate)
data_full$day<- wday(data_full$date, label=T)
data_full$day_type<- ifelse(data_full$day %in% c("Sat","Sun"), "weekend", "weekday")     
        
#panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 
#5-minute interval (x-axis) and the average number of steps taken, averaged across 
#all weekday days or weekend days (y-axis).      
 
data_week <- subset(data_full, data_full$day_type=="weekday")
data_weekend <- subset(data_full, data_full$day_type=="weekend")

steps_per_interval_week <- aggregate(steps ~ interval, data_week, FUN=mean)
steps_per_interval_weekend <- aggregate(steps ~ interval, data_weekend, FUN=mean)

library(gridExtra)
 
g3 <- ggplot(steps_per_interval_week, aes(x=interval, y=steps))
g3 <-g3 + geom_line(col="blue") + labs(x="5 Minute Intervals", y="Average Steps", 
                                          title="Week Day Average # of Steps Over All Intervals")       
g4 <- ggplot(steps_per_interval_weekend, aes(x=interval, y=steps))
g4 <-g4 + geom_line(col="green") + labs(x="5 Minute Intervals", y="Average Steps", 
                                 title="Weekend Average # of Steps Over All Intervals")       

grid.arrange(g3, g4, nrow=2, ncol=1)


