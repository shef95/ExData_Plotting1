## Import the dataset into R and set classes for each variable.
household<-read.table("household_power_consumption.txt", header=TRUE,sep = ";",na.strings = "?",colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

##Convert the Date column to formate dd-mm-yy
household$Date<-as.Date(household$Date, "%d/%m/%Y")

##Subsetting the date from 2007-02-01 to 2007-02-02
household<-subset(household,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

## Remove incomplete values
household <- household[complete.cases(household),]

## Combine Date and Time column
DateTime<-paste(household$Date, household$Time)

##Change the name of the newly merged column to "DateTime"
DateTime <- setNames(DateTime, "DateTime")

## Remove the original Date and Time columns
data <- household[ ,!(names(household) %in% c("Date","Time"))]

##Add the new DateTime column into data set
data <- cbind(DateTime, data)

## Change the class of DateTime column to Datetime
data$DateTime <- as.POSIXct(dateTime)

## Create the histogram (plot 1)
hist(data$Global_active_power,main = "Global Active Power",xlab = "Global Active Power (kilowatts)",ylab = "Frequency",xlim = c(0,6),col = "red")

##Save file and close
dev.copy(png,"plot1.png",width=480,height=480)
dev.off()

##Create plot2 
plot(data$DateTime,data$Global_active_power,ylab="Global Active Power (kilowatts)",xlab="",type="l")

##Save the plot and close
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()

## Create plot 3
with(data, {
        plot(Sub_metering_1~DateTime, type="l",
             ylab="Global Active Power (kilowatts)", xlab="")
        lines(Sub_metering_2~DateTime,col='Red')
        lines(Sub_metering_3~DateTime,col='Blue')
})

## Save the plot and close
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()

## Create plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(data, {
        plot(Global_active_power~DateTime, type="l", 
             ylab="Global Active Power (kilowatts)", xlab="")
        plot(Voltage~DateTime, type="l", 
             ylab="Voltage (volt)", xlab="")
        plot(Sub_metering_1~DateTime, type="l", 
             ylab="Global Active Power (kilowatts)", xlab="")
        lines(Sub_metering_2~DateTime,col='Red')
        lines(Sub_metering_3~DateTime,col='Blue')
        legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        plot(Global_reactive_power~DateTime, type="l", 
             ylab="Global Rective Power (kilowatts)",xlab="")
})

## Save file and close
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()
