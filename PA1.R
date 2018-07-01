library(lubridate)
library(dplyr)

fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fileLocal <- "activity.zip"
fileLocalExt <- "activity.csv"

if(!file.exists(fileLocal)){
  download.file(fileURL, fileLocal)
}

activityData <- read.csv(unz(
  fileLocal,
  fileLocalExt
  ),
  header = TRUE)

#activityData <- mutate(activityData, dt = as.Date(ymd(date)))


date_wise_steps <- select(activityData, steps, date, interval) %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))

par("mfrow" = c(1,1))

hist(date_wise_steps$total_steps, 
     breaks = 50,
     xlab = "Total steps taken each day",
     main = "HIstogram of Total steps per day")
meansteps <- mean(date_wise_steps$total_steps)
mediansteps <- median(date_wise_steps$total_steps)

abline(v = meansteps, col = "blue", lwd = 2, lty = 4)
abline(v = mediansteps, col = "pink", lwd = 2, lty = 4)



### section
interval_wise_steps <- select(activityData, steps, date, interval) %>%
  group_by(interval) %>%
  summarise(average_steps = mean(steps, na.rm = TRUE))

plot(interval_wise_steps$interval, interval_wise_steps$average_steps, type = "l")

max_steps <- max(interval_wise_steps$average_steps)
top_interval <- interval_wise_steps[
  interval_wise_steps$average_steps == max_steps,
  ]$interval

abline(h = max_steps, col = "red")
abline(v = top_interval, col = "red")



#### step 1 number of missing data records
na_count <- count(activityData[is.na(activityData$steps),])

set.seed(123)
tmp <- sample(5000,25, replace = FALSE)


#### step 2 and 3 replace NA with mean steps value and create new data
#cat(str(activityData))
UpdatedActivityData <- select(activityData, steps, date, interval) %>%
  left_join(interval_wise_steps, ., by = c("interval")) %>%
  mutate(steps = if_else(is.na(steps), average_steps, as.double(steps))) %>%
  select(steps, date, interval) %>%
  distinct %>%
  arrange(date, interval)
#cat(str(activityData))

#### step 4 create histogram

date_wise_steps <- select(UpdatedActivityData, steps, date, interval) %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))

hist(date_wise_steps$total_steps, 
     breaks = 50,
     xlab = "Total steps taken each day",
     main = "HIstogram of Total steps per day")
meansteps <- mean(date_wise_steps$total_steps)
mediansteps <- median(date_wise_steps$total_steps)

abline(v = meansteps, col = "blue", lwd = 2, lty = 4)
abline(v = mediansteps, col = "pink", lwd = 2, lty = 4)


####
UpdatedActivityData$day <- ifelse(
  wday(UpdatedActivityData$date, label = TRUE) %in% c("Sun","Sat"),"weekend","weekday"
  )

#par("mfrow" = c(2,1))
library(lattice) 

interval_wise_steps <- select(UpdatedActivityData, steps, date, interval, day) %>%
  group_by(interval, day) %>%
  summarise(average_steps = mean(steps, na.rm = TRUE))

#head(interval_wise_steps)

xyplot(average_steps ~ interval | day, data = interval_wise_steps, type = "l", layout = c(1,2))
