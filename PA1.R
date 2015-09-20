## ----setup, include=TRUE-------------------------------------------------
library("knitr")
knit_hooks$set(purl = hook_purl)

## ----download------------------------------------------------------------
if(!file.exists("activity.zip")){
fileUrl<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile='activity.zip', method='curl')
dateDownloaded<-date()
}
unzip('activity.zip', files="activity.csv", exdir=".",unzip="internal")

## ----loaddata------------------------------------------------------------
df<-read.csv("activity.csv",header=TRUE,sep=",")

## ----df_class------------------------------------------------------------
 sapply(df,class)

## ----classchange---------------------------------------------------------
df$date<-as.Date(df$date, format="%Y-%m-%d")

## ----noNA----------------------------------------------------------------
dfcomplete<-na.omit(df)

## ----numberdays----------------------------------------------------------
length(unique(df$date))
length(unique(dfcomplete$date))

## ----aggregatesteps------------------------------------------------------
outdf <- aggregate(steps ~ date, dfcomplete,FUN=sum)
str(outdf)

## ----histogram1----------------------------------------------------------
library("ggplot2")
qplot(outdf$steps, geom="histogram",xlab="number of steps",alpha=I(0.8),col=I("blue"),fill=I("orange"),binwidth=1000,main="Histogram of the total number of steps per day")

## ----stats---------------------------------------------------------------
mean(outdf$steps)
median(outdf$steps)

## ----dailyactivity-------------------------------------------------------
 daily.activity<- aggregate(steps ~ interval,dfcomplete,FUN=mean)
 ggplot(daily.activity, aes(x =daily.activity$interval, y = daily.activity$steps))+geom_line(color="blue")+xlab("5-minute interval")+ylab("average number of steps taken")+ggtitle("Averaged number of steps by interval")

## ----maxinterv-----------------------------------------------------------
 maxtab<-daily.activity[which.max(daily.activity$steps),]
 as.numeric(as.character(maxtab[[1]]))

## ----missingvalues-------------------------------------------------------
 sum(is.na(df))

## ----impute--------------------------------------------------------------
library("dplyr")
df$weekdays<-weekdays(df$date) 
dfcomplete$weekdays<-weekdays(dfcomplete$date) 
meanperday<-as.data.frame(with(dfcomplete, tapply(steps, list(interval, weekdays), mean)))
dfNA<-df[which(is.na(df)),]
for (i in 1:length(unique(dfNA$weekdays))){
    dfNA$steps<-replace(dfNA$steps, dfNA$weekdays==unique(dfNA$weekdays)[i],meanperday[unique(dfNA$weekdays)][[i]])
}
newdf<-rbind(dfcomplete, dfNA)%>%arrange(-desc(date))

## ----histogram2+stats----------------------------------------------------
newdf2 <- aggregate(steps ~ date, newdf,FUN=sum)
qplot(newdf2$steps, geom="histogram",xlab="number of steps",alpha=I(0.8),col=I("blue"),fill=I("orange"),binwidth=1000,main="Histogram of the total number of steps per day (missing values imputated)")
mean(newdf2$steps)
median(newdf2$steps)

## ----weekend|weekday-----------------------------------------------------
weekend<-c("Saturday","Sunday")
newdf$dayfactor<-as.factor(ifelse(newdf$weekdays %in% weekend,"weekend","weekday"))

## ----dayfactoractivity---------------------------------------------------
daily.activity2<-aggregate(steps~interval+dayfactor,newdf,FUN=mean)
ggplot(daily.activity2, aes(x =interval, y = steps))+geom_line(color="blue")+xlab("5-minute interval")+ylab("average number of steps taken")+ggtitle("Averaged number of steps by interval")+facet_grid(dayfactor~.)

