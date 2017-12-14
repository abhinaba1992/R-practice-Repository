#This portion covers the concepts of R basics, data visualisation in R, data preparation in R and  univariate statistics
#Author: Abhinaba Chakraborty
#last Updated: 22.11.2017

#dplyr: used for joins, also functions like mutate, select, filter, arrange, summarise and other data preparation functions.
#psych : contains the describe function
#vcd: contains the data set arthritis
#ggplot2:Grammer of graphics, for seeing UI rich the graphical functions
#lubridate: for functions to manipulate date fields

-----------------------------------------------------------------------------------------------------------------------
  
  #Part 1 [R Fundamentals] (27th aug2017)
  
  x=(1:5)
  #OR
  x=1:6
  x
  
  y="a"
  #OR
  y='a'
  y
  
  rm(x) #to remove an object
  
  ls()    #Displays all the functuion in the memory
  
  rm(list=ls()) #Removes all the object in the memory,2 step process, 1st assign all the objects inthe memoryinto a list, 2nd remove the list
  
  class(x)  #Help usto determinethe dat type of a variable
  
  typeof(x) #Sams as class; however, unlike class function, the representation may be different for some data types/structures
  #---------------------------------------------------------------------------------------------------
    #Explicit casting for integer 
    
  a=1   
  class(a) # here a would be numeric
  a=1L      #if we append the character L after a numeric value,than we can cast it into an integer
  class(a) # here a would be integer as its already casted
  #---------------------------------------------------------------------------------------------------
    #Explicit casting for integer along with other data types
    #If we try and cast any uncompatible datatypes, we would get a NA
    
  #integer
  a="1"
  class(a)
  a=as.integer(a)
  class(a)
  
  #character
  a=1
  class(a)
  a=as.character(a)
  class(a)
  
  #numeric
  a=1L
  class(a)
  a=as.numeric(a)
  class(a)
  
  #Logical
  a="TRUE"
  class(a)
  a=as.logical(a)
  class(a)
  
  
  #Logical 2
  a="T"
  class(a)
  a=as.logical(a)
  class(a)
  #---------------------------------------------------------------------------------------------------
  #If we dont know the use or meaning of a function and we want to see it's definition and 
  #We know the entire function name (pass the function nameafter question mark)
  ?ls()
  #If we just know the part name (pass the partly known name after two question marks)
  ??as.lo
  #---------------------------------------------------------------------------------------------------
  #Concatinating 2 strings
  a="Good"
  b="boy"
  #func 1 first part
  paste(a,b) #concatinating normally 
  
  #func 1 second part
  paste(a,b,sep=":") #concatinating with a seperator
  
  #func 1 third part
  a=1:26
  b=LETTERS
  paste(a,b,sep=":",collapse = "-") #concatinating with a separator and a collapse param that would be added 
  #in between each instance of the Concatinated string 
  
  #func 2
  a="Good"
  b="boy"
  paste0(a,b) # this is particularly useful when we do not want any seperator during concatination
  
  #---------------------------------------------------------------------------------------------------
  #Inorder to replace/substitute a value we use the following function
  #This will only substitute the first instance of the character that is subjected to be replaced
  a="CG/Airoli/Navi Mumbai"
  sub("/","-",a)
  
  #in order to substitute every instance of a particular character, we use gsub()
  a="CG/Airoli/Navi Mumbai"
  gsub("/","-",a)
  
  
  #string subsetting
  a="CG/Airoli/Navi Mumbai"
  substr(a,5,10)  # subsetting the string a from 5th to 10th position
  #---------------------------------------------------------------------------------------------------
  a="CG/Airoli/Navi Mumbai"
  nchar(a) # returns the number of character in an object (both character and string), if its passed for a vector, 
  # we would get the length for each element In the vector (that is we would get a vector of lengths)
  
  a=c("ab","abc","abcd")
  nchar(a)
  
  b=123
  nchar(b)
  
  #---------------------------------------------------------------------------------------------------
    
  #Part 2 [R Fundamentals continued...] (2nd Sep 2017)
    
  #defining vectors in R
  a=c("abc","def","ghi")
  a
  
  # its possible to create an object with a null value and pass actual values to it at later stage
  a=NULL
  a=c("abc","def","ghi")
  a 
  #---------------------------------------------------------------------------------------------------
  #The following function helps us to know if data structure is a vector, for primitive data types, it will always return true
  a=c("abc","def","ghi")
  is.vector(a)
  #---------------------------------------------------------------------------------------------------
  #Accessing elements in a vector
  a=c("India","China","USA")
  a[1] # by index
  
  a[-1] # this will select all the values except the one for whose index you have applied anegative value
  
  a[c(1,3)] # Accessing multiple values by passing a vector of indexes to get the specific values
  
  a[-c(1,2)]  # this helps to reject the values provided within the index vector
  
  a[c(1:2)]   # this will hep in getting all the elements from 1st to 2nd position
  
  a[c(1,2,-3)]  # THIS IS HOWEVER NOT POSSIBLE AS WE ARE CONFUSING THE R COMPILER about the selection
  
  a[c(1,1)] # We can also pass the same index multiple times,in that case,we would get the value in that index the number
  # of times wehave choosen to pass the index
  
  #---------------------------------------------------------------------------------------------------
  a=c(1,2,4,5,6)
  # applying any binary operator on a vector would apply on all the elements of the same
  a>4
  a+1
  a-1
  a*4
  a/4
  
  a[a>4] # This will help us access the elements that only satisfy agiven condition
  
  a[!a>4]  # This is just the opposite of the above statement
  #---------------------------------------------------------------------------------------------------
  #Sequence function
  seq(1,5)
  #Increasing the values in a sequence by a specific interval
  seq(1,5,by=0.5)
  
  
  #We can play around with the location of params of seq,but in that case, we need to specify some labels as 
  #it may turn out to not work properly
  #,e.g.
  seq(by=1,from=100,to=120)
  #OR
  seq(from=100,by=1,to=120)
  #OR
  seq(to=120,by=1,from=100)
  #---------------------------------------------------------------------------------------------------
  #Using the sort function
  a=c(1,255,16,78,90,43,21)
  sort(a)  #This isused to sort the results in vector
  #---------------------------------------------------------------------------------------------------
  #Repeat function
  rep("WCG",4) #This would repeat WCG four times
  
  rep(c("abc","def"),4) #This would repeat the entire vector 4 times
  
  rep(c("abc","def"),each=4) #This would repeat each element of the vector four time, following by the 2nd element
  #---------------------------------------------------------------------------------------------------
  #Round function
  a=1.245
  #Way 1
  round(a)
  #Way 2
  round(a,2)
  #---------------------------------------------------------------------------------------------------
  #Applying any binary operations on two vectors wouyld work like following
  #if two vectors are of equal length, then we wont have any issues,but in case they are of different length,
  #The longer vector size should ideally be a multiple of the shorter one, else we would get a warning
  a=c(1,2,3,4,5,6,7)
  b=c(1,2,3)
  a+b
  a-b
  a*b
  a/b
  #---------------------------------------------------------------------------------------------------
  #match function isused to comapre two vectors and find possible matches, eg
  a=c(1,91,98,3,5,99,7)
  c=98
  b=c(91,98,7)
  match(b,a)  # This will try and find a match for values of b in a and return the position of the matched vars
  match(c,a) # This will try and find a match for values of c in a, here, and return the position of the matched vars
  
  #the below operator is like using the "in" clause in SQL (This will return a logical vector)
  b %in% a
  #---------------------------------------------------------------------------------------------------
  #The following functions helps us to find the unique values in a vector
  a=c(1,1,2,2,3,4,5)
  unique(a)
  #---------------------------------------------------------------------------------------------------
  #The following function helps to find the length of a vector
  a=c(1,1,2,2,3,4,5)
  length(a)
  #---------------------------------------------------------------------------------------------------
  #The which function will help us to subset a vector based on some condition 
  a=c(1,2,3,4,5)
  a[which(a>2)]
  #---------------------------------------------------------------------------------------------------
  #The following function is used to reverse the contents of a vector
  a=c(1,2,3,4,5)
  rev(a)
  #---------------------------------------------------------------------------------------------------
  #The sample function and it's variations
  a=c(1:100)
  #Taking sample
  sample(a)
  #Taking sample for a specific no. of counts
  sample(a,20)
  #Taking sample for a specific no. of counts,and if wer are ok to repeat values in the sample
  sample(a,20,replace=T)
  #Setting a fixed permutation of sampling
  #First set the seed and run the sampling code, then set the same seed and run the sampling
  #Code again to get the same samples from the vector
  set.seed(20)
  sample(a,20,replace=T)
  #Set the seed again with same values and run the sampling again to get the same result
  set.seed(20)
  sample(a,20,replace=T)
  #If we want the sampling to be done around a specific percentage, we can do the following
  #We have a vector denoting the 2 sides of coins, we would need hundred samples of the same
  #and also expect "H" to come around 30%, while "T" to come around 70%,hence the formulae
  a=c("H","T")
  sample(a,100,replace=T,prob=c(0.3,0.7))
  #---------------------------------------------------------------------------------------------------
  #The table function would return the count of various variables for a categorical vector
  table(mtcars$am)
  #---------------------------------------------------------------------------------------------------
    
  #Dealing with data frame
  #Defining data frames
  #way 1
  df1=mtcars
  #way 2
  df2=data.frame(mtcars)
  df2
  
  #viewing data frames
  View(df2)
  #The following function would return the column names of all the cols in the data frame
  names(df2)
  names(df2)[1]# to get specific colnames
  names(df2)[c(1,2)] #to get specifc colnames (way 2)
  names(df2)[c(1:3)] #to get specifc colnames (way 3)
  #another method for getting the column names
  colnames(df2)
  #methodto get the rownames of thev ector
  rownames(df2)
  #Method to get both row and column names
  dimnames(df2)
  #The following function retruns the count of rows and cols
  dim(df2)
  #To get the no.of cols in a data frame, weuse
  ncol(df2)
  #To get theno. of rows in a data frame,weuse
  nrow(df2)
  #To get an idea of the overall structure of the data frame and its various data types, we use the following function
  str(df2)
  #---------------------------------------------------------------------------------------------------
  #Accessinga column in a df
  df2$mpg
  
  #accessing specific rows or columns (We can also use a series like c(1:10) instead of c(1,2) if we wish to)
  df2[3,]  # Accessing 3rd row and all cols
  df2[,3]  # Accessing all rows the 3rd column
  
  #Accessing multiple rows or cols
  df2[c(1,2,3),]   # specific rows
  df2[,c(1,2,3)]   # specific cols
  
  #Accessing specific rows and columns
  df2[c(1,2,3),c(1,2,3)]   # specific rows
  
  #Accessing series of cols
  df2[,c(1:3)]
  
  #Accessing series of rows
  df2[c(1:3),]
  
  #Accessing series of rows and cols
  df2[c(1:3),c(1:3)]
  
  #Accessing based on colnames and rownames
  df2[,c("mpg","cyl")]  #Based on column names
  df2[c("Fiat 128","Toyota Corolla"),]  #Based on row names
  
  #Based on both row names and column names
  df2[c("Fiat 128","Toyota Corolla"),c("mpg","cyl")]
  
  #Excluding rows and columns
  df2[-3,]  #Specific rows
  df2[,-3]  #Specific cols
  
  
  df2[-c(1,2,3),]   # specific set of rows
  df2[,-c(1,2,3)]   # specific set of cols
  
  
  df2[-c(1,2,3),-c(1,2,3)]    #Rows and cols 
  
  
  #We cannot exclude rows or columns based on column names or row names
  #for example the followings won't work
  df2[,-c("mpg")]
  #OR
  df2[-c("Fiat 128"),]
  
  #As a work around,we would need to use something like the following
  df2[,!names(df2) %in% c("mpg")]
  #OR
  df2[!rownames(df2) %in% c("Fiat 128"),]
  
  #---------------------------------------------------------------------------------------------------
    
  #Part 3 [R Fundamentals continued...] (3rd Sep 2017)
    
  #Matrix
  #It's a 2 dimensional data structure that stores same type of variables, e.g.
  matrix(c(1,2,3,4,5,6),nrow=2,ncol=3)
  #by default a matrix is filled column wise, if we want it to be filled row wise, we may the following
  matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow = T)
  
  #Assigning the names of rows and columns to a matrix
  a=matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow = T)
  dimnames(a)=list(c("1st","2nd"),c("col1","col2","col3"))
  a
  
  #Accessing elements in a matrix
  a=matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow = T)
  a[1,2] # rows and cols
  a[,2] # all rows but1 col
  a[1,] # 1 row but all cols
  a[,c(1:2)] #All rows but first to second column
  a[,c(1,3)] #All rows but 1st and 3rd column
  
  #Excluding elements in a matrix while selection
  a[,-1] #Excluding the 1st column of a matrix
  a[-1,] #Excluding the 1st row of a matrix
  a[,-c(2,3)] #Including all rows but Excluding  2nd and third column
  a[,-c(1:2)] #Excluding the first two columns
  
  #Accessing byrow names or colnames
  a[c("1st"),]
  a[,c("col1","col2")]
  
  
  #---------------------------------------------------------------------------------------------------
  #Lists in R
    
  x=c(1,2,3)
  y=c("a","b")
  z=1.25
  
  list1=list(x,y,z)
  list1
  
  #Accessing itemws in a list
  #1st level access
  list1[[1]]
  #2nd level access
  list1[[1]][2]
  
  #Renaming the branches of a list
  list2=list(a=x,b=y,c=z)  #x,y andzare the 3 variables decalred before
  list2
  
  
  #Accessing list variables by branch names
  #1st level access
  list2$a
  #2nd level access
  list2$a[2]
  #---------------------------------------------------------------------------------------------------
    
  #cbind(), this is used for appending columns to a data frame
  #rbind(), this isused fro appending rows to a datafram
    
  #---------------------------------------------------------------------------------------------------
  #Joins
  #Creating the tables
  empid=c(1,2,3,4,5)
  empname=c("john","Kevinf","Rob","Mike","Stan")
  deptid=c(1,2,2,3,4)
  
  emptable=data.frame(empid,empname,deptid)
  emptable
  
  deptid=c(1,2,3,4,5)
  deptname=c("HR","IT","FIN","SALES","ADM")
  
  depttable=data.frame(deptid,deptname)
  depttable
  
  library(dplyr)
  
  #following joins will return all the cols from both the data frames
  
  #inner join
  inner_join(emptable,depttable,by="deptid") 
  
  #left join
  left_join(emptable,depttable,by="deptid")
  
  #right join
  right_join(emptable,depttable,by="deptid")
  
  #full join
  full_join(emptable,depttable,by="deptid")
  
  
  #following joins will only return the cols from the first data frame
  
  #same as inner join, the only diff. is that it would only return the cols from the 1st data frame
  semi_join(emptable,depttable,by="deptid")
  
  #Returns the mismatched rows  and only the cols of the 1st data  frame
  anti_join(emptable,depttable,by="deptid")
  
  #---------------------------------------------------------------------------------------------------
  #read.csv(<file path> or file.choose(),row.names=T or F)
  #write.csv(<data frame>,<file path> or file.choose(),header=T or F)
    
  #note that paths in R are not denoted as "\", you may either choose "\\" or "/"
  #---------------------------------------------------------------------------------------------------
  #The follwoing functions help us to choose a sample set of records from above or below a data frame,
  #by defaultit is 6
  head(df1)
  tail(df1)
  #if you wish to choose the number of recordsto be selected, you maydo thefollowing
  head(df1,20)
  tail(df1,20)
  #---------------------------------------------------------------------------------------------------
  #Filtering samples
  #1
  data(iris)  
  df1=iris
  
  #Choosing all the columns but only selected rows that satisfy the given condition
  df1[df1$Sepal.Length>2,]
  #2
  df1[df1$Sepal.Length>2 & df1$Sepal.Width <3,] #and Clause
  #3
  df1[df1$Sepal.Length>2 | df1$Sepal.Width <3,] #or Clause
  #4
  #Choosing a specific column and only selected rows that satisfy the given condition
  df1[df1$Sepal.Length>2,"Sepal.Width"]
  
  #Choosing a specific column first and then applying the required condition as stated in [] brackets
  #5
  df1$Sepal.Length[df1$Sepal.Width<4]
  
  #---------------------------------------------------------------------------------------------------
    
  #Part 4 [Univariate statistics] (9th Sep 2017)
    
  #Univariate statistical fuctions
  data(mtcars)
  df1=mtcars
  df1
  
  #The following functions helps us to understand the measure of central tendency, that is where do the major chunk of 
  #our data lies
  #mean
  mean(df1$mpg)
  
  #meadian
  median(df1$mpg)
  
  #mode
  mode(df1$mpg)
  
  #The following functions are used for understanding variannce or data distribution
  #Variance
  var(df1$mpg)
  
  #Standard Deviation
  sd(df1$mpg)
  
  #inter quartile range
  IQR(df1$mpg)
  
  #Quantile
  quantile(df1$mpg)
  
  #range
  range(df1$mpg)
  
  --------------------------------------------------------------
    
    #Following functions are used for showing multiple univariate values at a time
    #Summary, We can choose to pass a data frame or a single column to get the result
    #Describe function is also similar to summary, however a lot more matrics are shown for describe
    
    summary(df1)
  #OR
  summary(df1$mpg)
  
  library(psych)
  
  #describe
  describe(df1)
  #OR
  describe(df1$mpg)
  
  #Histogram to see distribution of data for a continuous variable
  hist(df1$mpg)
  #OR
  hist(df1$mpg,breaks=10)
  
  #boxplot to see continous data Vs Categorical data
  #Solo continous data
  boxplot(df1$mpg)
  #OR
  #Continous data Vs Categorical data
  boxplot(df1$mpg~df1$am)
  
  #Barplot function is used to vizualise the categorical variables
  barplot(table(df1$am))
  ------------------------------------------------------
    
    #Getting the percentage of numerical data in our categorical col
    prop.table(df1$am)
  #OR
  round(prop.table(df1$am),2)
  #Prop. table gives us percentages across all values of our table, if we want to get percentage across rows or column of a 
  #data frame, we use the following 
  #Row wise % age
  prop.table(table(df1$am))
  
  -----------------------------------------------------
    
    #Xtabs function helps us to get the count instead of % age
    xtabs(~mpg,data = df1)
  
  #E.g.1
  xtabs(~mpg,data = df1)
  
  #E.g.2
  xtabs(~disp,data = df1)
  
  
  #Multiple column in xtabs
  xtabs(~mpg+disp,data=df1)
  
  -----------------------------------------------------
    
    #Margin.table function helps us to see the sum
    #either row wise or column wise
    #Row wise
    margin.table(df1$mpg,1)
  
  #Column wise
  margin.table(df1$mpg,2)
  
  ----------------------------------------------------
    #Add margins help us to append the summation result along with the sum values
    addmargins(table(df1$am),1)
  
  addmargins(table(df1),2)
  
  
  ftable(df1$mpg)
  
  -----------------------------------------------------------------------------------------------------------------------
    
    #Part 5 [data visualisations with ggplot] (10th Sep 2017)
    
    data(mtcars)
  df1=mtcars
  
  
  #Bivariate charts for analysis
  #Plotting a scattred plot for two continuous variables
  plot(df1$mpg,df1$hp)
  #OR
  plot(df1$mpg,df1$hp,col="red")
  
  #Plotting a boxplot for multiple variables (One continuous while the other Categorical)
  boxplot(df1$mpg~df1$am,col="blue",xlab="transmission",ylab="mileage",main="Mlg VS Transmission")
  
  #Plotting a mosaic plot for two categorical variables
  mosaicplot(~df1$am+df1$vs)
  
  --------------------------------------------------
    #Exploring the capabilities of ggplot 2
    library(ggplot2)
  
  #There are three layers for any gg plot graphs,
  #1.) Data Layer and Aesthetics, 2.) Geometric Layer, 3.) shape, size and color layer
  
  
  #Plotting two continuous variables using ggplot 2
  #P1: defining the data and aesthetics
  ggplot(df1,aes(x=mpg,y=wt))
  
  #Defining the geometric layer
  ggplot(df1,aes(x=mpg,y=wt))+geom_point()
  
  #If we want any numerical value to be treated as a categorical var, then we must cast it to factor for the ggplot
  #function to work upon it
  #Continuous variable Vs categorical variable
  df1$am=as.factor(df1$am)
  
  ggplot(df1,aes(x=am,y=mpg))+geom_boxplot()
  
  # Three-Demension plots
  df1$vs=as.factor(df1$vs)
  #Herw we would have 2 continuos variables and a single categorical variable
  ggplot(df1,aes(x=wt,y=mpg,color=vs)) + geom_point()
  
  #If we simply want a 2-D plot with a color red, we can simply do
  ggplot(df1,aes(x=wt,y=mpg,color="red")) + geom_point()
  
  
  #Adding other dimensions to our chart (5-d Chart)
  #Converting the required categorical varibales to factor
  df1$cyl=as.factor(df1$cyl)
  
  
  #Plotting the 5-D chart
  ggplot(df1,aes(x=wt,y=mpg,color=vs,shape=am,size=cyl)) + geom_point()
  
  
  #Another way to have a 3rd chart is to pass a categorical value directly to the geom_point() function, e.g.
  #Way 1
  ggplot(df1,aes(x=wt,y=mpg,color=vs)) + geom_point() + geom_smooth()
  #Now since, the above plot is broken down by the categorical variable vs, we will see an inconsistant chart 
  #for the same, so in order to rectify it, we need to pass the 3rd categorical variable directly to the geom
  #point function as shown below
  ggplot(df1,aes(x=wt,y=mpg)) + geom_point(aes(color=vs)) + geom_smooth() + geom_line()
  
  --------------------------------------------------
    #bar charts
    library(vcd)
  
  data(Arthritis)
  df3=Arthritis
  df3
  
  #Shwoing the bar for a categorical variable
  ggplot(df3,aes(x=Improved))+geom_bar()
  
  #Colors of graphs
  ggplot(df3,aes(x=Improved))+geom_bar(aes(color=Improved))
  ggplot(df3,aes(x=Improved))+geom_bar(aes(fill=Improved))
  
  
  #Color of graphs with different fills(two categorical variables)
  ggplot(df3,aes(x=Improved))+geom_bar(aes(color=Treatment))
  ggplot(df3,aes(x=Improved))+geom_bar(aes(fill=Treatment))
  
  
  #bar graph in polar coordinate system
  ggplot(df3,aes(x=Improved))+geom_bar()+coord_polar(theta="y")
  #Drawing a pie chart from a polar coordinate
  ggplot(df3,aes(x="",fill=Improved))+geom_bar()+coord_polar(theta="y")
  
  #In base package, it would be something like this
  pie(table(df3$Improved),col=c("red","green","blue"))
  
  
  #Variations of ggplot
  data(diamonds)
  df4=diamonds
  df4
  
  #Pie chart with the clarity field of the dat aset diamonds
  ggplot(diamonds,aes(x="",fill=clarity))+geom_bar()+coord_polar(theta="y")
  
  #The same Pie cahrt above with a color palette
  ggplot(diamonds,aes(x="",fill=clarity))+geom_bar()+coord_polar(theta="y")+scale_fill_brewer("Accent")
  
  #Coord polar chart with various bars
  ggplot(diamonds,aes(x=clarity,fill=clarity))+geom_bar()+coord_polar(theta="y")+scale_fill_brewer("Accent")
  
  #ween chart
  ggplot(diamonds,aes(x=clarity,fill=cut))+geom_bar()+coord_polar(theta="x")+scale_fill_brewer("Accent")
  
  #Specifying graph width
  ggplot(diamonds,aes(x=clarity,fill=clarity))+geom_bar(width=0.9)+coord_polar(theta="y")+scale_fill_brewer("Accent")
  
  #Histogram for continuous numerical data
  #Defining the data set
  mydata=data.frame(v1=c(rnorm(2000),runif(1000)),dist=c(rep("normal",2000),rep("uniform",1000)))
  mydata
  
  #Defining the histogram
  ggplot(mydata,aes(x=v1))+geom_histogram(bins = 100)
  
  #passing multiple values to function geom_histogram()
  ggplot(mydata,aes(x=v1,fill=dist))+geom_histogram(bins = 100)
  
  #To get a density plot
  ggplot(mydata,aes(x=v1))+geom_density()
  
  #Passing a scaled param to geom density
  ggplot(mydata,aes(x=v1,fill=dist))+geom_density(aes(y=..scaled..))
  
  -----------------------------------------------------------------------------------------------------------------------
    
    #Part 6 [Data Preparation in R] (16th Sep 2017)
    
    #The following function is used to get/set the working directory
    #example
    #Getting
    getwd()
  #Setting (Way 1)
  setwd("C:\\Users\\Abhinaba\\Downloads")
  #or 
  #Setting (Way 2)
  setwd("C:/Users/Abhinaba/Downloads")
  
  #Reading the files from a directory
  read.csv("C:\Users\Abhinaba\Downloads",sep=",")
  #OR
  file=read.csv(file.choose(),sep=",")
  
  #If we want our text related fields to be treated as strings and not factors, then we do the following
  file=read.csv(file.choose(),sep=",",stringsAsFactors = F)
  str(file)
  
  #if we have an unknow or unmarked variable in our file, we can make them NA by following the below method
  #In the below example, all the "Unknown" would be converted to "NA"
  file=read.csv(file.choose(),sep=",",stringsAsFactors = F,na.strings = "Unknown")
  
  #Counting the number of NAs in a column
  sum(is.na(file$statecode))
  #Counting the number of non NAs in a column
  sum(!is.na(file$statecode))
  
  
  #Using the lapply function to calculate the mean of all the columns in a data frame concurrently
  #We will get the mean of all numeric or integer cols, we can also use other functions here instead of mean
  #The return type of lapply would always be a list
  lapply(file,mean)
  
  
  file=read.csv(file.choose(),sep=",",stringsAsFactors = F)
  #Using lapply on a data set that has NA
  lapply(file,mean,na.rm=T) # We have a copy of the original file and introduced the word NA 
  #in one of the fields for eq_site_limit column
  
  
  #Getting a list of file names in a directory
  filenames=list.files(getwd(),pattern = "*txt")
  filenames
  
  #Getting files values from a location using lapply
  files=lapply(filenames,read.csv,header=F,stringsAsFactors=F)
  
  
  #do.call() function is used to combine multiple data sets into one
  #e.g.
  file=do.call(rbind,files)
  
  #While the return type of lapply would always be a list, the return type of sapply is intelligently calculated
  #e.g.
  data(mtcars)
  df1=mtcars
  df1
  
  sapply(df1,mean)
  #The above piece of code may return a vector based upon the data type
  
  
  #apply function helps us in getting row wise or col wise operations
  apply(df1,1,mean)  #Row wise mean
  apply(df1,2,mean)  #Col wise mean
  
  
  #We can also use laaply, sapply etc inside other functions and also lapply, sapply etc can be used inside other function
  #e.g.
  #Getting a column wise mean for all the columns for both the categories, i.e; automatic and manual
  fn=function(x){tapply(x,mtcars$am,mean)}
  apply(mtcars[,-9],2,fn)
  
  
  #tapply (This function is generally used for  for doing calculations on aggregation purposes)
  #Hence in the below example we get the mean of miles per galon, once for each type of category in am
  #we have two types of transmission (automatic and manual), hence two means
  #e.g. 1
  tapply(df1$mpg,df1$am,mean)
  #e.g. 2
  tapply(df1$mpg,df1$am,median)
  
  
  #At times, appending the rows to our existing data frame may give improper row names, in that case , we do the following
  #rownames(<data frame>)=NULL
  #Filtering made easy
  #old filtering
  mtcars[df1$mpg==21 & df1$qsec==16.46,]
  
  library(dplyr)
  
  #Filtering now
  filter(df1,mpg==21,qsec==16.46)
  
  
  #We can also filter using ifelse function like the following
  ifelse(df1$mpg>40,1,0)
  
  
  #Select statement based on column names
  select(df1,mpg,cyl)
  
  #Selecting based on conditions (The following select will select the cols from mpg to disp and any other two columns
  #that contain the keyword "qs" or "gea" as column names)
  select(df1,mpg:disp,contains("qs"),contains("gea"))
  
  #Excluding column names while selection
  select(df1,-disp,-hp,-vs)
  
  
  #Combining filter with select
  filter(select(df1,mpg,drat),mpg<15)
  
  -----------------------------------------------------------------------------------------------------------------------
    
    #Part 7 [Data Prepration in R Continued...] (17th Sep 2017)
    
    #Chaining of methods
    mtcars %>%  select(mpg,cyl,disp)  %>% filter(mpg>20)
  #Traditional way
  mtcars[mtcars$mpg>20,c("mpg","cyl","disp")]
  
  #arrange() function is used for sorting, its just like the order by clause in SQL
  #e.g.
  mtcars %>%  select(mpg,cyl,disp) %>% filter(mpg>20) %>% arrange(desc(mpg))
  #if we want it in ascending order we may write just mpg or we may also choose to write -mpg instead of desc(mpg), if we 
  #want it in descending order
  
  
  #mutate function is used to create new columns
  #e.g.
  mtcars %>% select(mpg,cyl,disp) %>% mutate(abc=mpg*2,def=cyl/mpg)
  
  
  #Group by clause in R, note that it is mandatory to use a summarize function if we using group by
  #e.g.
  mtcars %>% group_by(mpg) %>% summarise(avg_mpg=mean(mpg,na.rm=T))
  
  #Getting the count grouped by type of cylinder engine of records in our data set
  mtcars %>% group_by(cyl) %>% summarise(count=n())
  
  
  #Using group by for multiple columns
  mtcars %>% group_by(cyl, disp) %>% summarise(mpg_count=n()) %>% arrange(-mpg_count)
  
  #n_distinct() function helps us to get the count of unique values
  #eg.
  mtcars %>% group_by(cyl) %>% summarise(total_count=n(),distinct_count=n_distinct(am))
  
  #Working with dates in R
  library(lubridate)
  
  #The following function can convert a string to a date in the format year-month-day / month-day-year / day-month-year
  #NOTE:  when we use any of the below functions such as ymd, mdy or dmy, the function actually assumes that the string 
  # date is in that format which states in the function name and it's not responsible to make the string date look as 
  #the format states in the function 
  
  
  #The following function can convert a string to a date in the format year-month-day
  x="09-01-01"
  y=ymd(x)
  class(y)
  y
  #It can also convert a vector of date strings
  xx=c("09-01-01", "09-01-02", "09-01-03")
  yy=ymd(xx)
  yy
  -------------------------------------------------------
    
    #The following function can convert a string to a date in the format month-day-year
    x="09-01-01"
  y=mdy(x)
  class(y)
  y
  #It can also convert a vector of date strings
  xx=c("09-01-01", "09-01-02", "09-01-03")
  yy=mdy(xx)
  yy
  -------------------------------------------------------
    
    #The following function can convert a string to a date in the format day-month-year
    x="09-01-01"
  y=dmy(x)
  class(y)
  y
  #It can also convert a vector of date strings
  xx=c("09-01-01", "09-01-02", "09-01-03")
  yy=dmy(xx)
  yy
  -------------------------------------------------------
    
    #ymd_hms(): this helps us to get a timestamp, it allows us to param 
    
    
    
    
    
    
    # #How to connect to SQL from R
    # #First we need to create a DSN for ODBC driver in order to connect to SQL.
    # #We need to first download the driver for ODBC connection to SQL
  # #Here we have created an ODBC driver connection for our SQL data source and named it DemoData
  #
  #
  # #Load the library
  library(RODBC)
  #
  #Create the connection
  con = odbcConnect("DemoData")
  #We can optionally also do like the following in case we wanna pass uid and pwd
  #con = odbcConnect("DemoData",uid="<username>",pwd="<password>")
  
  #Running a basic query
  Student_Data= sqlQuery(con,"select * from [Abhinaba].[studentsDetails]")
  
  #Running a SP with a hard coded parameter
  Student_Data_SP <-sqlQuery(con, "set nocount on\nEXEC getdetails_basedOnID @ID=7")
  
  
  #With a dynamic parameter value
  #Single value
  Field1=1
  data_std = sqlQuery(con,paste0("exec getdetails_basedOnID @ID= ", Field1),errors=FALSE)
  #Multiple values
  Field1=1
  Field2=10
  data_std = sqlQuery(con,paste0("exec getdetails_basedOnID_class @ID= ", Field1 ,",@class=",Field2),errors=FALSE)
  
  
  library(ggplot2)
  #Normal GGPlot2 bar graph with absolute values and not grouped values
  ggplot(Student_Data,aes(x=name,y=Age))+ 
    geom_bar(stat = "identity",aes(fill=name)) +
    scale_fill_brewer(palette="Pastel1") +
    theme_dark()
  
  
  #Grouped Bar graphs in GGPlot2 with absolute values
  
  #Reshape 2 package contains the function melt that helps in converting the data from wide format to long format
  library(reshape2)
  
  dataset2=Student_Data[-1]
  
  #Converting the wide data into long format grouped by name
  DataSetGrouped = melt(dataset2, id.vars = "name")
  #Group bar
  ggplot(DataSetGrouped, aes(name, value)) +   
    geom_bar(aes(fill = variable), position="dodge",stat="identity") +
    scale_fill_brewer(palette="Pastel2")  +
    theme_dark()
  
  #Stacked bar
  ggplot(DataSetGrouped, aes(name, value)) +   
    geom_bar(aes(fill = variable), stat="identity") +
    scale_fill_brewer(palette="Pastel1") +
    theme_dark()
