## Reproducible Research - Case Study (Course Project 1)
## Chris Croupe
## 03/27/2020

library(zoo)

### Here are the data for the project
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

data_file <- "activity.csv"

## Download and unzip the dataset:
if (!file.exists(data_file)){
        download.file(url, "repdata_data_activity.zip")
 }  

if (!file.exists(data_file)) { 
        unzip("repdata_data_activity.zip") 
}

## Read the required data sets into the environment
activity_data <- read.csv(data_file, header = TRUE, sep = ",")


## Get the aggregate number of steps by day 
daily_total <- aggregate(activity_data$steps, by=list(activity_data$date), FUN = sum, na.rm = TRUE)
names(daily_total) <- c("date", "steps")

## make a histogram of the daily totals
hist(as.numeric(daily_total$steps), xlab = "Total Steps per Day", main = "Histogram of Steps Taken in a Day Over the Period")

## I'm not sure if the istructions are actually looking for a histogram or a view of the actual steps taken each day
## so I made this as well
barplot(daily_total$steps, ylab = "Steps", xlab = "Day", col = "blue", 
        main = "Steps per Day")

## Get the mean and median number of steps each day - initially I looked at this as the mean number of step per interval
## but the more I look at it the more it seems like the question is asking for the single number mean and median
## number of steps per day across the period of observation - which is available in the call to summary 
daily_total <- aggregate(activity_data$steps, by=list(activity_data$date), FUN = sum, na.rm = TRUE)
names(daily_total) <- c("date", "average_steps")
summary(daily_total)

daily_mean <- aggregate(activity_data$steps, by=list(activity_data$date), FUN = mean, na.rm = TRUE)
names(daily_mean) <- c("date", "average_steps")

## Plot the mean steps each day
plot(as.Date(daily_mean$date), daily_mean$average_steps, type = "l", lty = 1,   col = "green",
     xlab = "Day", ylab = "Average number of steps per interval", 
     main = "Average Number of Steps Over the Observation Period")

## Find the interval that on average, has the maximum number of steps
interval_mean <- aggregate(activity_data$steps, by=list(activity_data$interval), FUN = mean, na.rm=TRUE)
names(interval_mean) <- c("interval", "mean_steps")
max_interval <- interval_mean[which.max(interval_mean$mean_steps),]
max_interval <- max_interval[,1]
print(max_interval)


## How many NAs are there?
count_NAs <- sum (!complete.cases(activity_data))
percent_NAs = count_NAs/nrow(activity_data)
print(count_NAs)
print(percent_NAs)

## Find a more elegant way to deal with the NAs
## Using the na.locf function (found on Stack Overflow) - this function does not work if the first or last
## observations are 0 - so I hard set those to 0.

print(head(activity_data)) ## look at activity_data pre-data smoothing

activity_data$steps[1] = 0
activity_data$steps[17568] = 0 
activity_data$steps <- (na.locf(activity_data$steps) + rev(na.locf(rev(activity_data$steps))))/2

print(head(activity_data)) ## look at activity_data post-data smoothing

## re-run the histogram and bar plots with the data smoothed

## Get the aggregate number of steps by day 
daily_total <- aggregate(activity_data$steps, by=list(activity_data$date), FUN = sum, na.rm = TRUE)
names(daily_total) <- c("date", "steps")

## make a histogram of the daily totals
hist(as.numeric(daily_total$steps), xlab = "Total Steps per Day", 
     main = "Histogram of Steps Taken in a Day Over the Period (Smoothed)")

## I'm not sure if the istructions are actually looking for a histogram or a view of the actual steps taken each day
## so I made this as well
barplot(daily_total$steps, ylab = "Steps", xlab = "Day", col = "blue", 
        main = "Steps per Day (Smoothed)")

## Compare weekday to weekend steps
## Identify weekdays and weekends
activity_data$Day <- weekdays(as.Date(activity_data$date))
weekend <- c("Saturday", "Sunday")
activity_data$Weekend <- factor(activity_data$Day %in% weekend, levels = c(FALSE, TRUE), labels = c("weekday", "weekend"))

## Get the weekend and weekday interval averages
days_interval_mean <- aggregate(activity_data$steps, by=list(activity_data$Weekend, activity_data$interval), 
                                   FUN = mean, na.rm=TRUE)
names(days_interval_mean) <- c("Weekend", "interval", "average_steps")

weekend_interval_mean <- subset(days_interval_mean, Weekend == "weekend")
weekday_interval_mean <- subset(days_interval_mean, Weekend == "weekday")

summary(weekend_interval_mean)
summary(weekday_interval_mean)

## plot in a couple frames
par(mfrow=c(2,1))

plot(x = strptime(weekday_interval_mean$interval, format("%H%M")), y = weekday_interval_mean$average_steps, type = "l", lty = 1,   col = "blue",
     xlab = "Interval", ylab = "Average number of steps", 
     main = "Average Number of Weekday Steps by Interval", ylim = c(0,150))
    
plot(x = strptime(weekend_interval_mean$interval, format("%H%M")), y = weekend_interval_mean$average_steps, type = "l", lty = 1,   col = "purple",
     xlab = "Interval", ylab = "Average number of steps", 
     main = "Average Number of Weekend Steps by Interval", ylim = c(0,150))
