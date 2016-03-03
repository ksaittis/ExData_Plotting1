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
