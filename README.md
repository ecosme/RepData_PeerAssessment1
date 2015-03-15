For this assignment data is loaded in fullActivity variable from the activity.csv file.
I use an additional variable rawOriginalData to store original data, because  fullActivity variable is modified some steps after
Then the aggregate function is used to get the average of steps by date and stored on activityPerDay variable.

fullActivity<-read.csv("activity.csv");
rawOriginalData<-fullActivity;
activityPerDay<-aggregate(fullActivity$steps, by=list(fullActivity$date), mean)
colnames(activityPerDay)<-c("Date","AvgStepsDay");
-------------------------------------------------------------------------------
To get the mean totl number of steps taken per day without excluding NA values activityPerDay is used  
barplot(activityPerDay$AvgStepsDay, names.arg=activityPerDay$Date, ylab="Steps", xlab="Dates", main="Average Steps Per Day (Oct-Nov 2012)")
But we remove those NA values in order to get the mean and median values, otherwise these two values would be affected...
mean(activityPerDay$AvgStepsDay, na.rm=TRUE)
median(activityPerDay$AvgStepsDay, na.rm=TRUE)
-------------------------------------------------------------------------------
To get the average daily activity pattern we need to convert original character character column to a Date data type format then we convert this to a timestamp value and we stored this formated data in a data frame named df, it's important to mention that we have to use POSIX function to interpret dates properly.
df<-data.frame(date=strptime(as.character(paste(rawOriginalData$date,paste(substr(formatC(rawOriginalData$interval, width=4, flag="0"),1,2), ":", substr(formatC(rawOriginalData$interval, width=4, flag="0"),3,4),sep = ""))), format="%Y-%m-%d %H:%M"),steps=rawOriginalData$steps);
d.range<-range(df$date)
d.list <- seq(d.range[1], d.range[2], by='day')
par(mar = c(0.5, 4, 0, 1), oma = c(3, 0, 4, 0), mfcol = c(2,1))
plot(steps ~ date, data=df, type="l", ylab="Steps", xlab="Date" , xaxt='n', las=1, cex.axis=0.75, col="blue") 
axis.POSIXct(at=d.list, side=1, format="%b-%d", cex.axis=0.75)
mtext('Activity Monitoring Experiment', outer=TRUE, line=2, font=2)
-------------------------------------------------------------------------------
We count number of NA values separate them with the subset function...
missingValues<-subset(rawOriginalData, is.na(rawOriginalData$steps))
nrow(missingValues)

We replace these NA values with the average value of steps...
fullActivity$steps[is.na(fullActivity$steps)]<-mean(fullActivity$steps, na.rm=TRUE)
We calculate the average with the "new" data frame and set these values to the activityPerDay variable...
activityPerDay<-aggregate(fullActivity$steps, by=list(fullActivity$date), mean)
colnames(activityPerDay)<-c("Date","AvgStepsDay")
barplot(activityPerDay$AvgStepsDay, names.arg=activityPerDay$Date, ylab="Steps", xlab="Dates", main="Average Steps Per Day (Oct-Nov 2012)")
mean(activityPerDay$AvgStepsDay)
median(activityPerDay$AvgStepsDay)
-------------------------------------------------------------------------------
We identify dates into our data frame activityPerDay variable, We also need to create a group with the days of the week and we store them into the weekDayNames variable and the weekends into the weekEndDayNames variable so we could separate them using the ifelse function...
activityPerDay$Date<-as.Date(activityPerDay$Date);
day <- weekdays(activityPerDay$Date);
weekD <- cbind(activityPerDay,day);
weekDayNames<-c("Monday","Tuesday","Wednesday","Thursday","Friday" );
weekEndDayNames<-c( "Saturday","Sunday");
weekNames <- ifelse (weekD$day %in% weekDayNames, "weekday", "weekend");
weekD <- cbind(weekD, weekNames);
weekday <- weekD[weekD$weekNames=="weekday",];
weekend <- weekD[weekD$weekNames=="weekend",];
par(mfrow=c(2,1));
In this way we get the differences in activity patterns between weekdays and weekends....
barplot(weekday$AvgStepsDay, names.arg=weekday$Date, ylab="Avg Steps", xlab="Dates", main="Average Steps Weekdays", xlim=c(0,45), ylim = c(0,80));
barplot(weekend$AvgStepsDay, names.arg=weekend$Date, ylab="Avg Steps", xlab="Dates", main="Average Steps Weekend", xlim=c(0,16), ylim = c(0,60));





