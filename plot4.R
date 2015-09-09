Create_Household_Power_DataFrame <- function(HP_file = "household_power_consumption.txt") {
##  This read the household_power_comsumption file and converts the columns to the appropriate data types.
##  The date and time values are converted to a time variable and the file is filtered to those data
##  between 2007-02-01 and 2007-02-02 as required.
##
##  Use data.table for faster reading of large file
##  Use dplyr for filter function
        library(data.table)       
        library(dplyr)
        HP_df <- fread(HP_file)
        Date_Time_vec <- paste(HP_df$Date, HP_df$Time, sep = " ")
        Date_Time = strptime(Date_Time_vec, format = "%d/%m/%Y %H:%M:%S")
        HP_df <- cbind(HP_df, Date_Time)
##  Set time limits
        time1 <- strptime("1/2/2007 00:00:00", format = "%d/%m/%Y %H:%M:%S")
        time2 <- strptime("3/2/2007 00:00:00", format = "%d/%m/%Y %H:%M:%S")
##  Filter file
        HP_df <- filter(HP_df, Date_Time >= time1)
        HP_df <- filter(HP_df, Date_Time <= time2)
##  Convert other columns to numeric
        HP_df$Global_active_power <- as.numeric(HP_df$Global_active_power)
        HP_df$Global_reactive_power <- as.numeric(HP_df$Global_reactive_power)
        HP_df$Voltage <- as.numeric(HP_df$Voltage)
        HP_df$Global_intensity <- as.numeric(HP_df$Global_intensity)
        HP_df$Sub_metering_1 <- as.numeric(HP_df$Sub_metering_1)
        HP_df$Sub_metering_2 <- as.numeric(HP_df$Sub_metering_2)
        HP_df$Sub_metering_3 <- as.numeric(HP_df$Sub_metering_3)
##  Create column with time differences in fractional days
        Time_Change <- (as.numeric((difftime(HP_df$Date_Time, time1)))/(60*60*24))
        HP_df <- cbind(HP_df, Time_Change)        
}

Make_plot4 <- function(HP_file = "household_power_consumption.txt") {
##  This creates a plot with four panels with appropriate content and margins.
##
##  The Create_Household_Power_Dataframe function reads in the data and formats the variables
##  appropriately for use
##  It can be commented out if the HP_df file has already been read in.
##                
        HP_df <- Create_Household_Power_DataFrame()
##  Set overall graphics parameters
        par(mfrow = c(2,2), mar = c(4.1, 4.1, 2.1, 2.1))
##
##  Plot 1
##
        plot(HP_df$Time_Change, HP_df$Global_active_power, type = "l",
             xlab = "", ylab = "Global Active Power", xaxt = "n", yaxt = "n",
             cex.lab = 0.75, main = "")
        axis(side = 1, at = c(0, 1, 2), labels = c("Thu", "Fri", "Sat"), cex.axis = 0.75)
        axis(side = 2, at = c(0, 2, 4, 6), cex.axis = 0.75)
##
##  Plot 2
##
        plot(HP_df$Time_Change, HP_df$Voltage, type = "l",
                xlab = "datetime", ylab = "Voltage", xaxt = "n", yaxt = "n",
                ylim = c(234, 246), cex.lab = 0.75, main = "")
                axis(side = 1, at = c(0, 1, 2), labels = c("Thu", "Fri", "Sat"), cex.axis = 0.75)
                axis(side = 2, at = c(234, 236, 238, 240, 242, 244, 246), cex.axis = 0.75)
##
##  Plot 3
##
        plot(HP_df$Time_Change, HP_df$Sub_metering_1, type = "l",
                xlab = "", ylab = "Energy sub metering", cex.lab = 0.75, xaxt = "n", yaxt = "n")
        axis(side = 1, at = c(0, 1, 2), labels = c("Thu", "Fri", "Sat"), cex.axis = 0.75)
        axis(side = 2, at = c(0, 10, 20, 30), cex.axis = 0.75)
        lines(HP_df$Time_Change, HP_df$Sub_metering_2, col = "red")
        lines(HP_df$Time_Change, HP_df$Sub_metering_3, col = "blue")
        legend(x = "topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex = 0.6,
                lty=c(1, 1, 1), col = c("black", "red", "blue"), bty = "n")
##
##  Plot 4
##
        plot(HP_df$Time_Change, HP_df$Global_reactive_power, type = "l",
                xlab = "datetime", ylab = "Global_reactive_power", xaxt = "n", yaxt = "n",
                cex.lab = 0.75, main = "")
        axis(side = 1, at = c(0, 1, 2), labels = c("Thu", "Fri", "Sat"), cex.axis = 0.75)
        axis(side = 2, at = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), cex.axis = 0.75)
}

Make_plot4_png <- function() {
        ##  This makes a 480 X 480 png version of plot1
        png("plot4.png")
        Make_plot4()
        dev.off()
}