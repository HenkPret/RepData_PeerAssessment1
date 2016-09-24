---
title: "Reproducible Research: Course Project 1"
author: "Henk Pretorius"
date: "24 September 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, echo=FALSE, include=FALSE}
## Load packages
library(lubridate)
library(dplyr)
library(ggplot2)

## Set working directory
setwd("C:/Users/Henk/Dropbox/Research projects/R language/Coursera R/Working_directory/Reproducible research/Assignment1")
```

## Loading and preprocessing the data

```{r}
# Load data
rawdat <- read.csv("./activity.csv", sep = ",", header = TRUE)

# Process/transform the data (if necessary) into a format suitable for your analysis
rawdat$date <- ymd(rawdat$date) # changed date variable to date type
```

## What is the mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```{r}
# 1. Calculate the total number of steps taken per day
totsteps <- rawdat %>% na.omit() %>%
        group_by(date) %>% 
        summarise(totalsteps = sum(steps))
```

2. Make a histogram of the total number of steps taken each day

```{r}
histplot1 <- ggplot(totsteps, aes(totalsteps))+geom_histogram(binwidth = 2500)
histplot1 <- histplot1+ylab("Frequency")+
        xlab("Total number of steps")+
        ggtitle("Histogram of the total number of steps taken each day")
histplot1
```

3. Calculate and report the mean and median of the total number of steps taken per day.

```{r}
mean(totsteps$totalsteps, na.rm = TRUE)
median(totsteps$totalsteps, na.rm = TRUE)
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
avgstep <- rawdat %>% na.omit() %>%
        group_by(interval) %>% 
        summarise(meansteps = mean(steps)) 

lineplot1 <- ggplot(avgstep, aes(interval, meansteps))+
        geom_line()+
        xlab("5-minute interval")+
        ylab("Mean number of steps")+
        ggtitle("Mean number of steps by 5-minute interval across all days")
lineplot1
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
avgstep[which.max(avgstep$meansteps),1]
```

The 835th interval has the highest number of steps.

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
sum(is.na(rawdat$steps))
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy uses the average (mean) of all 5-min intervals and is shown below.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
rawdat2 <- rawdat %>%
        group_by(interval) %>%
        mutate(steps= ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
totsteps2 <- rawdat2 %>% 
        group_by(date) %>% 
        summarise(totalsteps = sum(steps))

histplot2 <- ggplot(totsteps2, aes(totalsteps))+geom_histogram(binwidth = 2500)
histplot2 <- histplot2+ylab("Frequency")+
        xlab("Total number of steps")+
        ggtitle("Histogram of the total number of steps taken each day")
histplot2

mean(totsteps2$totalsteps, na.rm = TRUE)
median(totsteps2$totalsteps, na.rm = TRUE)
```

Both the mean and the median values are the same after imputing values for the NAs. The estimate for the median differs slightly from the first part of the assignment, while the mean remains the same.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
rawdat <- mutate(rawdat, weekday = ifelse(weekdays(rawdat$date) == "Saturday" |
                                                    weekdays(rawdat$date) == "Sunday", 
                                            "weekend", "weekday"))
rawdat2 <- rawdat %>%
        group_by(interval) %>%
        mutate(steps= ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r, echo=TRUE}
intervsteps <- rawdat2 %>% 
        group_by(interval, weekday) %>% 
        summarise(meansteps = mean(steps))

lineplot2 <- ggplot(intervsteps, aes(interval, meansteps))+
        geom_line()+
        facet_wrap(~weekday, ncol = 1, nrow=2)+
        ggtitle("Average number of steps by interval over weekdays and weekends")+
        ylab("Mean number of steps")
        xlab("5-minute interval")
lineplot2
```

Both weekdays and weekends show high activity between the 750th and 1000th interval, while activity is slightly elevated for the weekends between the 1000th and 2000th interval compared to weekdays.
