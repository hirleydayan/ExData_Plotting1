# Load libraries 
library(data.table)
library(lubridate)
library(dplyr)

#' This function reads the Household Power Consumption input file
#' and extract data based on the desired period
#'
#' @param file The file name and path
#' @param period the desired period vector
#' @return The \code{data.table} with the desired period
#' @example
#' readDataForPeriod("household_power_consumption.txt", 
#'                      c(ymd("2007-02-01"), ymd("2007-02-02")))  
readDataForPeriod <- function(file, period){
        
        # Read data from file
        data <- fread(file,
                      colClasses ="character",
                      header = TRUE, 
                      na.strings = '?')
        
        # Remove lines with NA entries
        data <- data[complete.cases(data)]
        
        # Filter lines based on desired date period
        data <- data[dmy(data$Date) %in% period]
        data <- mutate(data,
                       Date_time=as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S"),
                       Global_active_power = as.numeric(Global_active_power),
                       Global_reactive_power = as.numeric(Global_reactive_power),
                       Voltage = as.numeric(Voltage),
                       Global_intensity = as.numeric(Global_intensity),
                       Sub_metering_1=as.numeric(Sub_metering_1),
                       Sub_metering_2=as.numeric(Sub_metering_2),
                       Sub_metering_3=as.numeric(Sub_metering_3) 
        )
        select(data,Date_time,Global_active_power:Sub_metering_3) 
}

# File name
file <- "household_power_consumption.txt"

# Period
period <- c(ymd("2007-02-01"), ymd("2007-02-02"))

# Read data from file and extract period
data <- readDataForPeriod(file,period)

# Find max Y value 
max <- do.call('pmax',select(data,Sub_metering_1:Sub_metering_3))

## Save plot on file
png("plot4.png", width = 480, height = 480)

# Plog graph
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0)) 
with(data, {
        plot(Date_time,Global_active_power, 
             xlab = "",
             ylab = "Global Active Power",
             type = "l")
        plot(Date_time,Voltage, 
             xlab = "datetime",
             ylab = "Voltage",
             type = "l") 
        plot(Date_time, max, type = "n",
             xlab = "",
             ylab = "Energy sub metering")
        lines(Date_time,Sub_metering_1,col = "black")
        lines(Date_time,Sub_metering_2,col = "red")
        lines(Date_time,Sub_metering_3,col = "blue")
        legend("topright", lty=1, col = c("black","blue", "red"), bty = "n",
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")) 
        plot(Date_time,Global_reactive_power,
             xlab = "datetime",
             ylab = "Global reactive power",
             type = "l")
        }
     )

## Shutdown devive (for saving on file reasons)
dev.off()