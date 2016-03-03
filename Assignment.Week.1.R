# Exploratory Data Analysis, Week 1 Assignment

# Not required if you have previously download the data
# fileurl <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
setwd('C:\\Users\\kwnstantinos\\Desktop\\Exploratory Data Analysis')
# download.file(fileurl, destfile = './Electric Power Consuption.zip')
# unzip('./Electric Power Consuption.zip',exdir = './data')


# Loading the data #
# When loading the dataset into R, please consider the following:
#       
# The dataset has 2,075,259 rows and 9 columns. First calculate a rough estimate of how much memory the dataset will require in memory before reading into R. Make sure your computer has enough memory (most modern computers should be fine).
# We will only be using data from the dates 2007-02-01 and 2007-02-02. One alternative is to read the data from just those dates rather than reading in the entire dataset and subsetting to those dates.
# You may find it useful to convert the Date and Time variables to Date/Time classes in R using the strptime()  and as.Date() functions.
# Note that in this dataset missing values are coded as ?.
datInd <- grep(pattern = '.*power.*',list.files('./data')) #finding the index of our data
datName <- list.files('./data')[datInd] #getting the name of the file
dat <- read.table(paste('./data/',datName,sep = ''),header = T, 
                  sep = ';', na.strings = '?',colClasses = c('character', 'character',rep('numeric',7)))

library(data.table)
dat <- as.data.table(dat) #transforming it to data.table for faster operations


library(lubridate) #loading lubridate to handle dates
if( Sys.timezone()== 'unknown'){Sys.setenv(TZ='GMT')} #setting the timezone
#dat$Date <- as.Date(as.character(dat$Date),format="%d:%m:%Y") #not working due to size
# dat$Date <- strptime(dat$Date, format = 'dd/mm/yyyy' ,tz='GMT') # unfortunately not working due to the size of the data

dat$Date <- dmy(dat$Date,tz = 'GMT') #converting the character to POSIXct
class(dat$Date) #checking the class of the Date column
View(dat) #view the data table to make sure everything is fine and tidy

#subsetting the data table on only 2007-02-01 and 2007-02-02

#changing the date format so it could be matched with the data
rdates <- strftime(c('2007-02-01','2007-02-02'),"%d/%m/%Y",tz='GMT')
rdates <- dmy(rdates, tz='GMT') #transform to posixct
# "2007-02-01 GMT" "2007-02-02 GMT"

library(dplyr)
dat %>% filter(Date %in% rdates) -> dat2days #filtered only those 2 days
View(dat2days) #making sure that the data set was filtered correctly



# Making Plots # 
# Our overall goal here is simply to examine how household energy usage varies over a 2-day period in February, 2007. Your task is to reconstruct the following plots below, all of which were constructed using the base plotting system.
# 
# First you will need to fork and clone the following GitHub repository: https://github.com/rdpeng/ExData_Plotting1
# 
# For each plot you should
# 
# Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
# Name each of the plot files as plot1.png, plot2.png, etc.
# Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the corresponding plot, i.e. code in plot1.R constructs the plot1.png plot. Your code file should include code for reading the data so that the plot can be fully reproduced. You must also include the code that creates the PNG file.
# Add the PNG file and R code file to the top-level folder of your git repository (no need for separate sub-folders)
# When you are finished with the assignment, push your git repository to GitHub so that the GitHub version of your repository is up to date. There should be four PNG files and four R code files, a total of eight files in the top-level folder of the repo.
# 
# The four plots that you will need to construct are shown below.



#plot 1
png(filename = 'plot1.png',width = 480, height = 480)
hist( dat2days$Global_active_power, 
      main = 'Global Active Power' ,
      col='red', xlab = 'Global Active Power (kilowatts)')
dev.off()

#plot 2
# creating a new column that combines Data and Time into single column
dat2days$DateTime <- as.POSIXct(paste(dat2days$Date, dat2days$Time), format="%Y-%m-%d %H:%M:%S")

png(filename = 'plot2.png',width = 480, height = 480)
with(dat2days, plot(y = Global_active_power,
                    x= DateTime, type = 'l',
                    xlab = '', ylab = 'Global Active Power (kilowatts)'))
dev.off()


#plot 3
dat2days$DateTime <- as.POSIXct(paste(dat2days$Date, dat2days$Time), format="%Y-%m-%d %H:%M:%S")

png('plot3.png')
with(dat2days, plot(DateTime, Sub_metering_1, type = "n", ylab= 'Energy sub metering',xlab = ''))
with(dat2days, points(DateTime, Sub_metering_1,  type = "l", ylab= 'Energy sub metering',xlab = ''))
with(dat2days,  points(DateTime, Sub_metering_2, col = "red", type = 'l', ylab= 'Energy sub metering',xlab = ''))
with( dat2days, points(DateTime, Sub_metering_3, col = "blue", 
                       type = 'l', ylab= 'Energy sub metering',xlab = ''))

legend("topright", lty = c(1,1,1), col = c("black", "red", 'blue'), 
       legend = c("Sub_metering_1", 'Sub_metering_2','Sub_metering_3'),
       cex = .5, y.intersp= 1)
dev.off()

#plot 4
png('plot4.png')

par(mfrow=c(2,2))
#1st
with(dat2days, plot(y = Global_active_power,
                    x= DateTime, type = 'l'
                    , ylab = 'Global Active Power (kilowatts)'))

#2nd
with(dat2days, plot(y= Voltage,x= DateTime , type = 'l'))


#3rd
with(dat2days, plot(DateTime, Sub_metering_1, type = "n", ylab= 'Energy sub metering',xlab = ''))
with(dat2days, points(DateTime, Sub_metering_1,  type = "l", ylab= 'Energy sub metering',xlab = ''))
with(dat2days,  points(DateTime, Sub_metering_2, col = "red", type = 'l', ylab= 'Energy sub metering',xlab = ''))
with( dat2days, points(DateTime, Sub_metering_3, col = "blue", 
                       type = 'l', ylab= 'Energy sub metering',xlab = ''))

legend("topright", lty = c(1,1,1), col = c("black", "red", 'blue'), 
       legend = c("Sub_metering_1", 'Sub_metering_2','Sub_metering_3'),
       cex = .5, y.intersp= 1, bty = 'n')

#4rd
with(dat2days, plot(x=DateTime,y=Global_reactive_power, type = 'l'))

dev.off()