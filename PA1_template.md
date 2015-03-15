# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
setwd("D:\\Git\\RepData_PeerAssessment1")
file<-"activity.csv"
data<-read.csv(
                file,
                skip=1, # skip header and specify column attributes below for readability.
                col.names=c("steps","date","interval"),
                colClasses=c("numeric","Date","numeric"))
```


## What is mean total number of steps taken per day?

The total number of steps recorded per day and mean total number of steps per day is calculated below.  Code to plot a histogram of step counts per day is also included. Following the code are the results - a histogram and the calculated average number of steps.


```r
# Libraries
        library(plyr)
        library(date)
        library(ggplot2)

# Get step totals for each day, only consider non-NA records
        dataNoNA<- data[!is.na(data$steps),]
        dataNA  <- data[is.na(data$steps),]
        dataDailyNoNA<-ddply(dataNoNA,.(date),summarize,sum=sum(steps))
        stepAverage<- sum(dataDailyNoNA$sum)/length(dataDailyNoNA$sum)
        stepAverage<- mean(dataDailyNoNA$sum)
        stepAverageThousands<- stepAverage/1000.00

# Set histogram labels and render histogram
        title<- c("Total Steps per Day\n(missing data filtered out)")
        xlabel<- c("Date"); ylabel<- c("Steps")
        ggplot(dataDailyNoNA,aes(x=dataDailyNoNA$date))+
                geom_histogram(aes(weight=dataDailyNoNA$sum),binwidth=1)+
                ggtitle(title)+xlab(xlabel)+ylab(ylabel)
```

![](PA1_template_files/figure-html/MeanStepsPerDay-1.png) 

The mean total number of steps taken per day is 10.7661887 thousand when ignoring all days having missing data.

## What is the average daily activity pattern?

The next plot shows the average number of steps by time of day averaged over all days.  The plot is annotated with the time of day where the most activity occurs, on average. Missing values are filtered out of the analysis.


```r
# Average step counts over each interval for all days,
# filtering out NA values
dataIntervalNoNA<-ddply(dataNoNA,.(interval),summarize,
                ave=sum(steps)/length(dataNoNA$steps))

# Interval with the maximum number of steps
maxSteps<-max(dataIntervalNoNA$ave)
maxInterval<-dataIntervalNoNA[dataIntervalNoNA$ave==maxSteps,]$interval

# Set plot labels and render line plot
title<- c("Average Steps per 5 Minute Interval Averaged over all Days\n(missing data filtered out)")
xlabel<- c("Time of Day (24 hr)"); ylabel<- c("Steps per interval averaged over all days")
ggplot(dataIntervalNoNA,aes(x=dataIntervalNoNA$interval,y=dataIntervalNoNA$ave))+
        geom_line()+
        ggtitle(title)+xlab(xlabel)+ylab(ylabel)+
        annotate("text",x=(maxInterval+1*12*maxInterval/24),y=maxSteps,
                 label=paste("Max activity at",maxInterval))
```

![](PA1_template_files/figure-html/AverageOverInterval-1.png) 


## Imputing missing values

The total number of rows in the data set with missing values is: 2303.  We will now imput data for the missing values by imputing the avarage number of steps recorded for the interval corresponding with each missing value


```r
# create function to replace NA with average value for the interval
addMissing<-function(dataRaw){
        rvalue<-dataRaw[[1]]
        if(is.na(rvalue)){
                rvalue<-dataIntervalNoNA[dataIntervalNoNA$interval==dataRaw[[2]],]$ave
        }
        rvalue
}

# create a dataframe with only steps and interval columns and use to fill missing values
stepsIntervalWNA<- data.frame(steps=data$steps,interval=data$interval)
imputed<-data
imputed$steps<-apply(stepsIntervalWNA,1,addMissing)
imputedDaily<-ddply(imputed,.(date),summarize,sum=sum(steps,na.rm=FALSE))

# plot result
mlabel<- c("Total Steps taken per Day")
xlabel<- c("Day")
ylabel<- c("Step Count")
ggplot(imputedDaily,aes(x=imputedDaily$date))+
        geom_histogram(aes(weight=imputedDaily$sum),binwidth=1)+
        ggtitle("Total Steps per Day\n(missing data imputed)")+
        xlab("Date")+
        ylab("Steps")
```

![](PA1_template_files/figure-html/ImputedData-1.png) 

```r
imputedMean<-mean(imputedDaily$sum)
imputedMedian<- median(imputedDaily$sum)
meanDaily<-mean(dataDailyNoNA$sum)
medianDaily<-median(dataDailyNoNA$sum)

textResult1<- paste("The imputed (",imputedMean,") and nonimputed ",meanDaily,"means are ",
if(imputedMean==meanDaily){"the same."}else{"different."})
textResult2<- paste("The imputed (",imputedMedian,") and nonimputed ",medianDaily,"medians are ",
if(imputedMedian==medianDaily){"the same."}else{"different."})
```

The imputed ( 9359.13204647386 ) and nonimputed  10766.1886792453 means are  different. The imputed ( 10395 ) and nonimputed  10765 medians are  different.

## Are there differences in activity patterns between weekdays and weekends?
