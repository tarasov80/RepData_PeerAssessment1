setwd("C:/R/RepData_PeerAssessment1")

##Loading and preprocessing the data

#Show any code that is needed to

if (!file.exists("data\activity.csv")) 
{
    if (file.exists("activity.zip")) 
    {
        unzip("activity.zip", exdir = "data")
    }
    else
    {
        stop("Can't find zip file (activity.zip).")
    }
}
#Load the data (i.e. read.csv())
activity <- read.csv("data/activity.csv", stringsAsFactors = FALSE)

#Process/transform the data (if necessary) into a format suitable for your analysis
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")


##What is mean total number of steps taken per day?
library(dplyr)
#Make a histogram of the total number of steps taken each day
totalStepsByDate <- 
    activity %>%
    filter(is.na(steps)==FALSE) %>%
    group_by(date) %>%
    summarise(sum(steps))
plot(totalStepsByDate, type = "h")

#Calculate and report the mean and median total number of steps taken per day
meanStepsByDate <- 
    activity %>%
    filter(is.na(steps)==FALSE) %>%
    group_by(date) %>%
    summarise(mean(steps))
plot(meanStepsByDate, type = "h")

medianStepsByDate <- 
    activity %>%
    filter(is.na(steps)==FALSE) %>%
    group_by(date) %>%
    summarise(median(steps))
plot(medianStepsByDate, type = "h")

