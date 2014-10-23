## libraries
require(ggplot2)

## set work dir
setwd("C:/Users/mdragt/SkyDrive/Coursera/Exploring Data/")

## -----------------------------------------------------------------------------
## get data
## -----------------------------------------------------------------------------
file = "household_power_consumption.txt"
df_hpc <- read.csv(file, 
                   sep=";", 
                   header=T, 
                   colClasses=c("character","character","numeric","numeric",
                                "numeric","numeric","numeric", "numeric","numeric"), 
                   na.strings="?")

## -----------------------------------------------------------------------------
## elaborate data
## -----------------------------------------------------------------------------

## create date format
df_hpc$Date <- as.Date(df_hpc$Date,format="%d/%m/%Y")

## select days
df_day1 <- subset(df_hpc, df_hpc$Date == "2007-02-01")
df_day2 <- subset(df_hpc, df_hpc$Date == "2007-02-02")

## combine datasets
df_all <- rbind(df_day1,df_day2)

## create time stamp
df_all$datetime <- as.POSIXct(paste(as.Date(df_all$Date), df_all$Time))

## -----------------------------------------------------------------------------
## functions
## -----------------------------------------------------------------------------

## create histogram Global Active Power
plot1 <- function() {
    png(file='plot1.png', width=480, height=480)
    hist(df_all$Global_active_power, 
        main="Global Active Power", 
        xlab="Global Active Power (kilowatts)", 
        ylab="Frequency", 
        col="Red")
    dev.off()
}

## create line graph Global Active Power
plot2 <- function() {
    png(file='plot2.png', width=480, height=480)
    plot(df_all$datetime,  df_all$Global_active_power, 
        ylab="Global Active Power (kilowatts)", 
        type="l", 
        lwd=1, 
        xlab="")
    dev.off()
}    

## plot Energy Sub Metering 
plot3 <- function() {
    png(file='plot3.png', width=480, height=480)   
    plot(df_all$datetime, df_all$Sub_metering_1,
         ylab="Energy sub metering",
         type="l",
         xlab="")
    lines(df_all$datetime, df_all$Sub_metering_2,
          col="red")
    lines(df_all$datetime, df_all$Sub_metering_3,
          col="blue")
    legend("topright", c(colnames(df_all)[7:9]), col=c("black","red","blue"),
                         bty="y", lty=1)
    dev.off()
}

## 4 plots
plot4 <- function() {
    png(file='plot4.png', width=480, height=480)
    par(mfrow=c(2,2), mar=c(2,4,2,1), oma=c(0,0,1,0))
    ## Global Active Power
    plot(df_all$datetime,  df_all$Global_active_power, 
         ylab="Global Active Power", 
         type="l", 
         lwd=1, 
         xlab="")
    # Voltage
    plot(df_all$datetime,  df_all$Voltage, 
         ylab="Voltage", 
         type="l", 
         lwd=1, 
         xlab="datetime")
    # Energy sub metering
    plot(df_all$datetime, df_all$Sub_metering_1,
         ylab="Energy sub metering",
         type="l",
         xlab="")
    lines(df_all$datetime, df_all$Sub_metering_2,
          col="red")
    lines(df_all$datetime, df_all$Sub_metering_3,
          col="blue")
    legend("topright", c(colnames(df_all)[7:9]), col=c("black","red","blue"),
           bty="n", lty=1, cex=.75)
    # Global reactive power
    plot(df_all$datetime,  df_all$Global_reactive_power, 
         ylab="Global_reactive_power", 
         type="l", 
         lwd=1, 
         xlab="datetime")
    dev.off()
}
