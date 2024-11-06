youtube
-nested loop

###########################################
#introduction to function
###########################################

#a function that add two numbers

Nasir1<-function(num1,num2){
  #operations to perfom
  sum<-num1+num2
  
  #print the output
  print(sum)
}

Nasir1(5,2)

#function that return mean and standard deviation

Nasir2<-function(data1){
  mean1<-mean(data1)
  sd1<-sd(data1)
  
  print(paste("the mean is ", mean1))
  print(paste("the standard deviation is ", sd1))
}

Nasir2(mtcars$mpg)

#function that return median and IQR

Nasir2<-function(data1){
  median1<-median(data1)
  IQR1<-IQR(data1)
  
  print(paste("the median is ", median1))
  print(paste("the IQR is ", IQR1))
}

Nasir2(mtcars$mpg)


#function that return mean,sd,median, IQR
#for all nunmerical variables in the dataset


Nasir2<-function(data1){
  numeric_data<-sapply(data1, is numeric)
  
  means<-sapply(numeric_data,mean,na.rm=TRUE)
  sds<-sapply(numeric_data,sd,na.rm=TRUE)
  medians<-sapply(numeric_data,median,na.rm=TRUE)
  IQRs<-sapply(numeric_data,IQR,na.rm=TRUE)
  
  results<-data.frame{
    mean=means,
    Std_Dev=sds,
    median=medians,
    IQR=IQRs,
    
  }
  print(results)
}

Nasir2(mtcars)


######################################3
#to call the package
library(dplyr)

ls(getNamespace("dplyr"))

#to use the package 
#first need to see the documentation 
?sumaries


mtcars%>%
filter(mpg>20)%>%
  

  
  
  
  
  
  
library(devtools)
install_github("harraz/nasirds")





#1st method read.csv

data1<-read.csv("demo.csv")
?read.csv
str(data1)

#2nd method read.csv
library(readr)
?read.csv
data2<-read.csv("demo.csv")
str(data2)

#to import excel file 

library(openxlsx)
?read.xlsx


#export to csv file 
write.scv(data7)


#nested loop tgk youtube sir flow control
#introduction to function

#a func that add 2 no

Nasir1 <- function(num1, num2){
  #operations to perform
  sum <- num1+num2
  
  #print the output
  print(sum)
}

Nasir1(5, 2)

#func that return mean and sd

Nasir2 <- function(data1){
  mean1 <- mean(data1)
  sd1 <- sd(data1)
  
  print(paste("The mean is: ", mean1))
  print(paste("The standard deviation is: ", sd1))
}

Nasir2(mtcars$mpg)

#func that return median and interquarterrange

Nasir3 <- function(num){
  median1 <- median(num)
  IQR1 <- IQR(num)
  
  print(paste("The median is: ", median1))
  print(paste("The IQR is: ", IQR1))
}

stats_results_df <- stats_mtcars

Nasir4 <- function(data1){
  numeric_data <- sapply(data1, is.numeric)
  
  means <- sapply(numeric_data, mean, na.rm = T)
  sds <- sapply(numeric_data, sd, na.rm = T)
  medians <- sapply(numeric_data, median, na.rm = T)
  IQRs <- sapply(numeric_data, IQR, na.rm = T)
  
  results <- data.frame(
    Mean = means,
    Std_Dev = sds,
    Median = medians,
    IQR = IQRs
  )
  
  print(results)
}

Nasir4(mtcars)

#1. a func that detect missing values in a dataset
#2. calculate median for each variable 
#3. replace missing value with its median value
#4. print new dataset without missing value
#5. conclude(pack) the function into a package + documentation + help + description
#6. upload into Github
#7. give the link for your package
