require(graphics)
#Load the data 
# 1. Download data file and unzip it
# 2. Read data 
# 3. Filter data with Date value equals 2/1/2007 or 2/2/2007, 
# 4. Eliminate the data with missing value "?".
# 5. Transform each column into desired format, e.g. Date, numeric
loadData <- function() {
  
  #### Change to your own working directory
  wd <- "/Users/yuazhuan/R"
  setwd(wd)
  wd <- getwd()
  
  # Download data
  message("Download and unzip data file.")
  destFile <- "household_power_consumption.txt"
  if (!file.exists(destFile)) {
    tmp <- "power.zip"
    dlink <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    download.file(dlink, tmp, "curl")
    unzip(tmp)
  }
  
  tmp <- read.table(destFile, header=TRUE, sep=";")
  
  wantedRows <-  grep("^[:blank:]*[0]?[12]{1}/[0]?2/2007", tmp$Date)
  dt <- tmp[wantedRows, ]
  rm(tmp)
  
  #Remove original data set to save memory
  dt <- dt[(dt$Date == "1/2/2007" | dt$Date == "2/2/2007") 
           & dt$Global_active_power != "?"
           & dt$Global_reactive_power != "?" 
           & dt$Voltage != "?"
           & dt$Global_intensity != "?"
           & dt$Sub_metering_1 != "?"
           & dt$Sub_metering_2 != "?"
           & dt$Sub_metering_3 != "?",]
  
  tmp <- paste(dt$Date, dt$Time)
  dt$Time <- strptime(tmp, "%d/%m/%Y %H:%M:%S")
  dt <- dt[, 2:ncol(dt)]
  
  dt$Global_active_power <- as.numeric(levels(dt$Global_active_power))[dt$Global_active_power]
  dt$Global_reactive_power <- as.numeric(levels(dt$Global_reactive_power))[dt$Global_reactive_power]
  dt$Voltage <- as.numeric(levels(dt$Voltage))[dt$Voltage]
  dt$Global_intensity <- as.numeric(levels(dt$Global_intensity))[dt$Global_intensity]
  dt$Sub_metering_1 <- as.numeric(levels(dt$Sub_metering_1))[dt$Sub_metering_1]
  dt$Sub_metering_2 <- as.numeric(levels(dt$Sub_metering_2))[dt$Sub_metering_2]
  return(dt)
  
}


x<-loadData()
targetFile <- "plot4.png"
png(targetFile, width = 480, height = 480 )


# plot 4
par(mfrow=c(2,2))

#plot 4.1
plot(x$Time,x$Global_active_power, type="l", ylab = "Global Active Power (kilowatts)", xlab="")

#plot 4.2
plot(x$Time,x$Voltage, type="l", ylab = "Voltage", xlab="datetime")

#plot 4.3
plot(x$Time, x$Sub_metering_1, type="l", col = "black", ylab = "Energy sub metering", xlab = "")
lines(x$Time, x$Sub_metering_2, type="l", col = "red")
lines(x$Time, x$Sub_metering_3, type="l", col = "blue")
legend("topright", lty=1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

#plot 4.4
plot(x$Time,x$Global_reactive_power, type="l", ylab = "Global_reactive_power", xlab="datetime")

dev.off()