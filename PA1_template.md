---
title: "PA1_template"
author: "Andrew Wagner"
date: "4/1/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

## Loading and preprocessing the data

### Load the data

```{r}
library(ggplot2)

data <- read.csv(file = "activity.csv", header = TRUE)

```
## What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day

```{r}
totalsteps <- aggregate(steps ~ date, data, FUN=sum)

```
### If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```{r}
hist(totalsteps$steps, main = "Total Steps per Day", xlab = "Number of Steps", ylab = "# of Days")

```
### Calculate and report the mean and median of the total number of steps taken per day

```{r}
meansteps <- mean(totalsteps$steps, na.rm = TRUE)
mediansteps <- median(totalsteps$steps, na.rm = TRUE)

meansteps
mediansteps
```

## What is the average daily activity pattern?

### Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
meanint <- aggregate(steps ~ interval, data, mean)
ggplot(data = meanint, aes(x = interval, y = steps)) + geom_line() +
  ggtitle("Average Activity Pattern") + xlab("5-minute interval") +
  ylab("Average # of Steps")

```

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
maxint <- meanint[which.max(meanint$steps),]

maxint
```

## Imputing missing values

### Calculate and report the total number of missing values in the dataset

```{r}
missvalues <- is.na(data$steps)
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r}
imdata <- transform(data, steps = ifelse(is.na(data$steps), meanint$steps[match(data$interval, meanint$interval)],data$steps))
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
imstepsint <- aggregate(steps ~ date, imdata, FUN = sum)
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
hist(imstepsint$steps, main = "Inputed Steps per Day", xlab = "Number of Steps")

immeansteps <- mean(imstepsint$steps, na.rm = TRUE)
immediansteps <- median(imstepsint$steps, na.rm = TRUE)
meandif <- immeansteps - meansteps
mediandif <- immediansteps - mediansteps
totaldif = sum(imstepsint$steps) - sum(totalsteps$steps)

immeansteps
immediansteps
meandif
mediandif
totaldif
```

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
dt <- function(date) {
  day <- weekdays(date)
  if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
    return ("weekeday")
  else if (day %in% c('Saturday', 'Sunday'))
    return ("weekend")
  else
    stop ("Invalid Date Format.")
}

imdata$date <- as.Date(imdata$date)
imdata$day <- sapply(imdata$date, FUN = dt)

```

### Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}
meanspd <- aggregate(steps ~ interval + day, imdata, mean)
ggplot(data = meanspd, aes(x = interval, y = steps)) + geom_line() +
  facet_grid(day ~ .) + ggtitle("Average Activity Pattern") +
  xlab("5-minute Interval") + ylab("Average # of Steps")
```