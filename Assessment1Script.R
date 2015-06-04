#Load data

setwd("/RepResearch")
activity <- read.csv("activity.csv", header = TRUE, colClasses = c("numeric", "POSIXct", "numeric"))

library(dplyr)
TotalStepsPerDay <- 
activity %>% 
    group_by(date) %>% 
    summarize(TotalSteps = sum(steps, na.rm = TRUE))

hist(TotalStepsPerDay$TotalSteps,
     col = "gray",
     xlab = "Total Steps Per Day",
     main = "Histogram of Total Steps Per Day")

TotalStepsPerDay %>% summarize(MeanStepsPerDay = mean(TotalSteps, na.rm = TRUE), 
                               MedianStepsPerDay = median(TotalSteps, na.rm = TRUE))

#Average activity during each 5 min interval across all days

AvgActivity <-
    activity %>% 
    group_by(interval) %>%
    summarize(Average = mean(steps, na.rm=TRUE))    

plot(AvgActivity$interval, AvgActivity$Average, 
     type = "l",
     xlab = "5-min Interval",
     ylab = "Average Steps",
     main = "Average Daily Activity Pattern")

## Report max interval

AvgActivity[which.max(AvgActivity$Average),]

## Total number of missing values
sum(is.na(activity))


## Replacing Missing Values with intervale mean

Complete_activity <- 
    activity %>%
    group_by(interval) %>%
    mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps)) %>%
    ungroup()
    
## total steps of complete data plus histogram

CompleteTotalSteps <- 
    Complete_activity %>% 
    group_by(date) %>% 
    summarize(TotalSteps = sum(steps))

hist(CompleteTotalSteps$TotalSteps,
     col = "darkblue",
     xlab = "Total Steps Per Day",
     main = "Histogram of Total Steps Per Day using Complete Data")

CompleteTotalSteps %>% summarize(MeanComplete = mean(TotalSteps), 
                               MedianComplete = median(TotalSteps))

##Weekend vs weekday 

## Create weekday column factor

Complete_activity[,"dayofweek"] <- weekdays(activity$date)

weekend <- function(x){
    j <- c()
    for(i in x){
        if(i == "Monday"| i == "Tuesday"| i == "Wednesday"| i == "Thursday"| i == "Friday"){
            j <- c(j, "weekday")
        }
        else if(i == "Saturday"| i == "Sunday"){
            j <- c(j, "weekend")
        }
        }
    j
    }

Complete_activity[, "weekdayFactor"] <- weekend(activity$dayofweek)
head(Complete_activity)


CompleteTotalSteps <- 
    Complete_activity %>% 
    group_by(date) %>% 
    summarize(TotalSteps = mean(steps))


Complete_activity <- 
    Complete_activity %>%
    mutate(weekdayFactor = ifelse(dayofweek == "Sunday"| dayofweek == "Saturday", "weekend", "weekday")) %>%
    ungroup()

StepsbyFactor <- 
    Complete_activity %>%
    group_by(weekdayFactor, interval) %>%
    summarize(TotalSteps = mean(steps)) %>%
    ungroup()



library(ggplot2)

qplot(interval, TotalSteps, data=StepsbyFactor, facets = weekdayFactor~., 
      geom = "line")






