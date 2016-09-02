#Setting the working directory
setwd("~/Documents/R Files/Reproducible research/2nd week assignment")
#Loading the data file
stepData<-read.csv("activity.csv")
#Prepare data for mean calculation of steps
stepsperDay<-tapply(stepData$steps,stepData$date,sum,na.rm=TRUE)
hist(stepsperDay,col="blue",xlab="Steps per day",main="Histogram of steps per day")
stepsMean<-mean(stepsperDay,na.rm = TRUE)
stepsMedian<-median(stepsperDay,na.rm = TRUE)
#Average daily pattern
stepsperInterval<-tapply(stepData$steps,stepData$interval,mean,na.rm=TRUE)
plot(names(stepsperInterval),stepsperInterval,type="l",xlab="Interval",ylab="Average number of steps",main="Average number of steps per interval")
maxStepsInterval<-names(stepsperInterval[which.max(stepsperInterval)])
maxSteps<-max(stepsperInterval)
#Number of incomplete cases
x<-complete.cases(stepData)
sum(!x)
stepDataComplete<-stepData
stepDataComplete[!x,1]<-stepsperInterval[as.character(stepDataComplete[!x,3])]
#Ploting the data with filled NAs
stepsperDay2<-tapply(stepDataComplete$steps,stepDataComplete$date,sum,na.rm=TRUE)
par(mfrow=c(1,2),mar=c(4,4,2,1),oma=c(0,0,2,0))
hist(stepsperDay,col="blue",xlab="Steps per day",main="Steps per day",ylim=c(0,40))
hist(stepsperDay2,col="green",xlab="Steps per day",main="Steps per day (adjusted)",ylim = c(0,40))
#calculating mean and median for adjusted values
stepsMean2<-mean(stepsperDay2,na.rm = TRUE)
stepsMedian2<-median(stepsperDay2,na.rm = TRUE)
#weekdays and weekends
stepDataDate<-stepDataComplete
stepDataDate$date<-as.Date(stepData$date)
weekends=c("Saturday","Sunday")
stepDataDate$typeofDay<-factor(weekdays(stepDataDate$date)%in%weekends,c(TRUE,FALSE),c("weekend","weekday"))
#Plotting weekday vrs weekend data
library(ggplot2)
library(dplyr)
groupedData<-group_by(stepDataDate,interval,typeofDay)
summarizedData<-summarize(groupedData,avgSteps=mean(steps,na.rm = TRUE))
qplot(interval,avgSteps,data=summarizedData,facets = typeofDay~.,geom = c("line"),main="Average steps per interval",ylab = "Average steps",xlab = "Interval")
