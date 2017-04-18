# The below sample code is to generate Sample details for the Population of Australian states.

SrNo <- c(1,2,3,4,5,6,7,8,9)
year <- c(1917,1927,1937,1947,1957,1967,1977,1987,1997)
NSW <- c(1904,2402,2693,2985,3625,4295,5002,5617,6274)
Vic <- c(1409,1727,1853,2055,2656,3274,3837,4210,4605)
Qld <- c(683,873,993,1106,1413,1700,2130,2675,3401)
SA <- c(440,565,589,646,873,1110,1286,1393,1480)
WA <- c(306,392,457,502,688,879,1204,1496,1798)
Tas <- c(193,211,233,257,326,375,415,449,474)
NT <- c(5,4,6,11,21,62,104,158,187)
ACT <- c(3,8,11,17,38,103,214,265,310)
Aust <- c(4943,6182,6835,7579,9640,11798,14192,16263,18529)

#Creating the data frame from the vectors coz I am running the scripts on r-fiddle.org
AustralianStates <- data.frame(SrNo,year,NSW,Vic,Qld,SA ,WA ,Tas,NT ,ACT,Aust)

#Alternately, if you have using R desktop or any other portal that supports uploading your data file you can use the following code
#AustralianStates <- read.csv(file="austpop.csv", header=TRUE, sep=",")

#Note that if you are working from R Desktop, you may need to set the working directory before executing the above step
#The below code does the following
#setwd("D:\\Users\\abhincha\\Downloads")


#PLOT THE STATE WISE POPULATION FOR ALL THE STATES FOR THE YEAR 1947

# Setting the default property for all Graphs
par(bg="Black",fg="White")


#AustralianStates
StateDetailsForSelectedYear <- AustralianStates[AustralianStates$year == 1947,c(3:10)]
#StateDetailsForSelectedYear
PoPcount <- as.numeric(StateDetailsForSelectedYear[1,])
#PoPcount
StateNames <- names(StateDetailsForSelectedYear)
#StateNames


#Specifying the layout of the charts using the layout function
grid <- matrix(c(1,1,2,3,3,4), nrow=2,ncol=3,byrow=TRUE) # Creating the grid of the layout
layout(grid) #Passing the grid to the layout

#Plotting a bar chart to depict State wise population for all the states for the year 1947
barplot(PoPcount,names.arg = StateNames,xlab = "States",ylab = "Population Count",
main = "Australian State Vs. Population Count 1947",col.main="White",col.lab="White",col.axis="White",
col="darkcyan")




#PLOT A PIE CHART FOR DEPICTING POPULATIONS FOR QUEENS LAND VS TASMANIA IN THE YEAR 1977
#EXTRACTING REQUIRED DATA
statedetailsforyear <- AustralianStates[AustralianStates$year == 1977,c("Qld","Tas")]
statedetailsforYEAR <- as.numeric(statedetailsforyear)

#PLOTTING THE REQUIRED PIE
pie(statedetailsforYEAR, main="Qld Vs.Tas (1977)",col.main="White",
labels=(
paste
(
names(statedetailsforyear),statedetailsforYEAR,sep="\n"
)
)
,
col=c("darkslategray1","darkcyan")
)





#PLOT A COMPARISON BAR PLOT OR GROUP BAR BETWEEN NEW SOUTH WHALES, VICTORIA AND SOUTHERN AUSTRALIA 
#FOR THE YEARS BEFORE 1950

#Extract the data set relevant to your time period
statedetailsbeforegivendate <- AustralianStates[AustralianStates$year < 1950,c("year","NSW","Vic","SA")]

#Bind the rows for NSW and Vic in order to form a matrix that can be represented as a group bar
Count_Table <- rbind(statedetailsbeforegivendate$NSW,statedetailsbeforegivendate$Vic,statedetailsbeforegivendate$SA)

#Plotting the required bar
barplot(Count_Table,names.arg = statedetailsbeforegivendate$year,xlab = "Years",ylab = "Population Count",
main = "NSW Vs. Vic Vs. SA before 1950s",col.main="White",col.lab="White",col.axis="White",
col=c("deepskyblue","darkslategray1","darkcyan"),beside=T)

legend("topleft", c("NSW","Vic","SA"), pch=15, 
       col=c("deepskyblue","darkslategray1","darkcyan"), 
       bty="n")
       




#CALCULATE THE TOTAL POPULATION FOR AUSTRALIA EXCLUSIDNG WESTERN AUSTRALIA FOR THE YEAR BETWEEN 1950 and 1980
#AND PLOT IT IN A BAR

#Extracting the required data from the data frame
statedetailsbeforegivendaterange <- AustralianStates[AustralianStates$year > 1950 & AustralianStates$year < 1980,c("year","WA","Aust")]

populationdiff <- statedetailsbeforegivendaterange$Aust - statedetailsbeforegivendaterange$WA
years <- statedetailsbeforegivendaterange$year

barplot(populationdiff,names.arg = years,xlab = "Years",ylab = "Population Count",
main = "Aus Pop 1950-80 excl WA",col.main="White",col.lab="White",col.axis="White",
col="darkcyan")
 









