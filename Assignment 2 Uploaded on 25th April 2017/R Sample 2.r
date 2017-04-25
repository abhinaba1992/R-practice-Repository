#Note that if you are working from R Desktop, you may need to set the working directory first
#The below code does it
setwd("D:\\Users\\abhincha\\Downloads")


#Creating the data frame from the files
ATandTStocks <- read.csv(file="AT&TSmarket.csv", header=TRUE, sep=",")


#SUBSETTING THE REQUIRED YEAR WISE DETAILS
year2001 <- ATandTStocks[ATandTStocks$Year == 2001,c("Day","Year","Lag1","Volume","Direction")]
year2002 <- ATandTStocks[ATandTStocks$Year == 2002,c("Day","Year","Lag1","Volume","Direction")]
year2003 <- ATandTStocks[ATandTStocks$Year == 2003,c("Day","Year","Lag1","Volume","Direction")]
year2004 <- ATandTStocks[ATandTStocks$Year == 2004,c("Day","Year","Lag1","Volume","Direction")]
year2005 <- ATandTStocks[ATandTStocks$Year == 2005,c("Day","Year","Lag1","Volume","Direction")]


# Setting the default property for all Graphs
par(bg="Black",fg="White")

#SETTING THE GRID LAYOUT
grid <- matrix(c(1,1,2,2,3,3,4,4), nrow=2,ncol=4,byrow=TRUE) # Creating the grid of the layout
layout(grid) #Passing the grid to the layout


#THE HIGHEST VOLUME IN A YEAR WITH THE YEAR AND DAY NUMBER FOR THE YEARS 2001-2005.


#THE DAYS HAVING THE MAXIMUM VALUE FOR EACH YEAR ARE EXTRACTED
Day01 <- year2001[year2001$Volume==max(as.double(year2001$Volume)),c("Day")]
Day02 <- year2002[year2002$Volume==max(as.double(year2002$Volume)),c("Day")]
Day03 <- year2003[year2003$Volume==max(as.double(year2003$Volume)),c("Day")]
Day04 <- year2004[year2004$Volume==max(as.double(year2004$Volume)),c("Day")]
Day05 <- year2005[year2005$Volume==max(as.double(year2005$Volume)),c("Day")]


#A VECTOR IS FORMED OUT OF THE EXTRACTED DAYS
Days <- c(Day01,Day02,Day03,Day04,Day05)

#UNIQUE YEAR VALUES ARE CONCATINATED WITH THE THE DAYS VECTOR CALCULATED ABOVE
YearandDay <- paste(unique(ATandTStocks$Year),Days,sep=" - ")


#THE MAX VOLUMES FOR ALL THE YEARS ARE CALCULATED AND PLACED IN A VECTOR
allvolumes <-c(
ATandTStocksVolumeMax01 <- as.double(year2001[year2001$Volume==max(as.double(year2001$Volume)),c("Volume")]),
ATandTStocksVolumeMax02 <- as.double(year2002[year2002$Volume==max(as.double(year2002$Volume)),c("Volume")]),
ATandTStocksVolumeMax03 <- as.double(year2003[year2003$Volume==max(as.double(year2003$Volume)),c("Volume")]),
ATandTStocksVolumeMax04 <- as.double(year2004[year2004$Volume==max(as.double(year2004$Volume)),c("Volume")]),
ATandTStocksVolumeMax05 <- as.double(year2005[year2005$Volume==max(as.double(year2005$Volume)),c("Volume")])
)


#THE BAR IS PLOTTED WITH THE REQUIRED PARAMETERS AND ATTRIBUTES
barplot(allvolumes,names.arg = YearandDay,xlab = "Year and Day",ylab = "Highest Volume Count",
main = "Highest vol cnt with years and day No",col.main="White",col.lab="White",col.axis="White",
col="cornsilk3",cex.names=0.6)




#PIE OF SUMMATION OF VOLUMES CATEGORIZED BY YEAR.

#THE SUM VOLUME OF EACH YEAR IS CALCULATED AND ASSIGNED TO A VECTOR
Sum_Vol_Vector <- c(
sum(year2001$Volume),
sum(year2002$Volume),
sum(year2003$Volume),
sum(year2004$Volume),
sum(year2005$Volume)
)


#PLOTTING THE REQUIRED PIE
pie(Sum_Vol_Vector, main="Total Volume Per Year",col.main="White",
labels=(
paste
(
unique(ATandTStocks$Year),Sum_Vol_Vector,sep="\n"
)
)
,
col=c("darkslategray1","darkcyan","deepskyblue","cornsilk3","burlywood3")
)




#COMPARISON BAR OR GROUP BAR BETWEEN THE DIRECTION UP AND DOWN FOR INDIVIDUAL YEARS

#UP COUNTS PER YEAR IS CALCULATED
UpCounts <- c(
length(which((year2001[,c("Direction")]) == "Up")),
length(which((year2002[,c("Direction")]) == "Up")),
length(which((year2003[,c("Direction")]) == "Up")),
length(which((year2004[,c("Direction")]) == "Up")),
length(which((year2005[,c("Direction")]) == "Up"))
)

#DOWN COUNTS PER YEAR IS CALCULATED
Downcounts <- c(
length(which((year2001[,c("Direction")]) == "Down")),
length(which((year2002[,c("Direction")]) == "Down")),
length(which((year2003[,c("Direction")]) == "Down")),
length(which((year2004[,c("Direction")]) == "Down")),
length(which((year2005[,c("Direction")]) == "Down"))
)

#THE ABOVE TWO VECTORS ARE COMBINED (ROW BINDED) IN ORDER TO BE REPRESENTED AS A GROUP BAR
Count_Table <- rbind(UpCounts,Downcounts)


#BAR IS PLOTTED WITH THE REQUIRED DETAILS
barplot(Count_Table,names.arg = unique(ATandTStocks$Year),xlab = "Years",ylab = "Up and Down Count",
main = "Up Vs Down Count by Years",col.main="White",col.lab="White",col.axis="White",
col=c("deepskyblue","darkcyan"),beside=T)


#LEGEND IS PLOTTED FOR THE ABOVE GRAPHS
legend("topleft", c("Up","Down"), pch=15, 
       col=c("deepskyblue","darkcyan"), 
       bty="n")



#THE VALUE OF BEST LAG1 FOR EACH QUARTER OF 2001 WITH DAY NUMBER

#DAYS AND LAG1S FOR EACH QUARTER IS FETCHED AND FOLLOWINGLY, THE MAXIMUM FOR EACH LAG IS CALCULATED
Q1Lag1Vals <- year2001[year2001$Day >= min(year2001$Day) & year2001$Day <= floor((max(year2001$Day)/4)),c("Day","Lag1")]
Q1Lag1MaxVals <- Q1Lag1Vals[Q1Lag1Vals$Lag1 == max(Q1Lag1Vals$Lag1),c("Day","Lag1")]
Q2Lag1Vals <- year2001[year2001$Day > floor((max(year2001$Day)/4)) & year2001$Day <= floor((max(year2001$Day)/2)),c("Day","Lag1")]
Q2Lag1MaxVals <- Q2Lag1Vals[Q2Lag1Vals$Lag1 == max(Q2Lag1Vals$Lag1),c("Day","Lag1")]
Q3Lag1Vals <- year2001[year2001$Day > floor((max(year2001$Day)/2)) & year2001$Day <= (max(year2001$Day)-floor((max(year2001$Day)/3))),c("Day","Lag1")]
Q3Lag1MaxVals <- Q3Lag1Vals[Q3Lag1Vals$Lag1 == max(Q3Lag1Vals$Lag1),c("Day","Lag1")]
Q4Lag1Vals <- year2001[year2001$Day > (max(year2001$Day)-floor((max(year2001$Day)/3))) & year2001$Day <= floor((max(year2001$Day))),c("Day","Lag1")]
Q4Lag1MaxVals <- Q4Lag1Vals[Q4Lag1Vals$Lag1 == max(Q4Lag1Vals$Lag1),c("Day","Lag1")]


#DAYS HAVING THE MAXIMUM LAG IN A QUARTER IS FETCHED FROM ABOVE VARIABLES 
DaysWithMaxLags <- c(
as.character(Q1Lag1MaxVals$Day),
as.character(Q2Lag1MaxVals$Day),
as.character(Q3Lag1MaxVals$Day),
as.character(Q4Lag1MaxVals$Day)
)

#ALL THE QUARTERS IS HARDCODED IN A VECTOR
Quartrs <- c("Q1","Q2","Q3","Q4")


#THE QUARTER NAMES IS CONCATINATED WITH THE DAY NUMBERS HAVING MAXIMUM LAG1 VALUE FOR THAT QUARTER 
QuartersWihDays <- (paste
(
Quartrs,
DaysWithMaxLags,
sep=" - "
))

#VECTOR WITH MAXIMUM LAG VALUES EACH QUARTER IS FORMED
LagMaxVals <- c(
as.numeric(Q1Lag1MaxVals$Lag1),
as.numeric(Q2Lag1MaxVals$Lag1),
as.numeric(Q3Lag1MaxVals$Lag1),
as.numeric(Q4Lag1MaxVals$Lag1)
)

#THE REQUIRED BAR IS PLOTTED WITH THE ABOVE DETAILS
barplot(LagMaxVals,names.arg = QuartersWihDays,xlab = "Quarters and Day",ylab = "Lag 1 Value",
main = "Highest Lag1 for Q1,Q2,Q3 & Q4 of 2001",col.main="White",col.lab="White",col.axis="White",
col="darkcyan")