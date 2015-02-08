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

## Save plot on file
png("plot1.png", width = 480, height = 480)

# Plot histogram 
hist(data$Global_active_power,  
     main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)",
     col = "red")

## Shutdown devive (for saving on file reasons)
dev.off()
