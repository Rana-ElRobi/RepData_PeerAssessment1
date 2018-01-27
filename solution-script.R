setwd("/home/rana/Desktop/Ds-Coursera/07-reproducible-research/RepData_PeerAssessment1")

# unzip data and read 
unzip("activity.zip")
step.data <- read.csv("activity.csv", header = TRUE)
head(step.data)

library(magrittr)
library(dplyr)

data.date <- step.data %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
hist(data.date$tsteps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)


# Calculate and report the mean and median of the total number of steps taken per day
mean(data.date$tsteps)
## [1] 10766.19
median(data.date$tsteps)
## [1] 10765

# Time series plot
library(ggplot2)
data.interval <- step.data%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 
ggplot(data.interval, aes(x=interval, y=tsteps))+ geom_line()

# The 5-minute interval that, on average, contains the maximum number of steps
five <- data.interval[which(data.interval$tsteps== max(data.interval$tsteps)),]
five
## # A tibble: 1 x 2
##   interval   tsteps
##      <int>    <dbl>
## 1      835 206.1698

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
# new dataset
step.data.v2 <- step.data
 
# dataset minus NA's for Mean calculation
NA.data <- step.data.v2[is.na(step.data.v2$steps),]
clean.data <- step.data.v2[!is.na(step.data.v2$steps),]

# generate mean by interval
mean.data.interval <- aggregate(clean.data$steps, by=list(clean.data$interval), sum)
names(mean.data.interval)[1] ="interval"
names(mean.data.interval)[2] ="steps"

step.data.v2 <- step.data
missingData <- is.na(step.data.v2$steps)
meanVals <- tapply(clean.data$steps, clean.data$interval, mean, na.rm=TRUE, simplify=TRUE)
step.data.v2$steps[missingData] <- meanVals[as.character(step.data.v2$interval[missingData])]


# original missing data count
sum(missingData)
## [1] 2304
# count of NA values
sum(is.na(step.data.v2$steps))
## [1] 0

#--------------------------------------------
# Make a histogram of the total number of steps taken each day and
# Calculate and report the mean and median total number of steps taken per day.

full.SumData.Day <- aggregate(step.data.v2$steps, by=list(step.data.v2$date), sum)

names(full.SumData.Day)[1] ="date"
names(full.SumData.Day)[2] ="totalsteps"
head(full.SumData.Day,15)

hist(full.SumData.Day$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)

summary(full.SumData.Day)

#----------------------------------------------
# Compare the mean and median of Old and New data
oldmean <- mean(data.date$tsteps, na.rm = TRUE)
newmean <- mean(full.SumData.Day$totalsteps)
# Old mean and New mean
oldmean
## [1] 10766.19
newmean
## [1] 10766.19
oldmedian <- median(data.date$tsteps, na.rm = TRUE)
newmedian <- median(full.SumData.Day$totalsteps)
# Old median and New median
oldmedian
## [1] 10765
newmedian
## [1] 10766.19

#--------------------------------------------------
# Are there differences in activity patterns between weekdays and weekends?
step.data.v2$date <- as.Date(step.data.v2$date)
step.data.v2$weekday <- weekdays(step.data.v2$date)
step.data.v2$weekend <- ifelse(step.data.v2$weekday=="Saturday" | step.data.v2$weekday=="Sunday", "Weekend", "Weekday" )
###
mean.week.end.day <- aggregate(step.data.v2$steps, by=list(step.data.v2$weekend, step.data.v2$interval), mean)
names(mean.week.end.day)[1] ="weekend"
names(mean.week.end.day)[2] ="interval"
names(mean.week.end.day)[3] ="steps"

ggplot(mean.week.end.day, aes(x = interval, y=steps, color=weekend)) +
  geom_line() +
  facet_grid(weekend ~ .) +
  labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")

#---------------------------

# generate html page from RMD file
library(knitr)
library(markdown)

# converts an md file to html
markdownToHTML("/home/rana/Desktop/Ds-Coursera/07-reproducible-research/RepData_PeerAssessment1/PA1_template.Rmd", "/home/rana/Desktop/Ds-Coursera/07-reproducible-research/RepData_PeerAssessment1/PA1_template.html") 
