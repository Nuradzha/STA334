# 1) Create a function that detect missing values in a datasets

detect_missing_values <- function(data){
  missing_counts <- numeric(ncol(data))
  names(missing_counts) <- colnames(data)
  
  for(i in 1:ncol(data)){
    missing_counts[i] <-sum(is.na(data[[i]]))
  }
  
  #filter out collumn with no missing values
  missing_counts <- missing_counts[missing_counts > 0]
  return(missing_counts)
}

#test with a sample dataswt
sample_data <- data.frame(
  A = c(1, 2, NA, 4, 5),
  B = c(NA, 2, 3, 4, 5),
  c = c(1, 2, 3, 4, 5)
  
)

detect_missing_values(sample_data)

#check airquality dataset

detect_missing_values(airquality)

#lets experiment
data1 <- mtcars
data1[c(1,2,3,4),c(5,4,3,2,1)] <- NA
detect_missing_values(data1)

#2) Calculate median for each variables.
calculate_median<-function(data){
  medians<-numeric(0)
  
  for(col_name in colnames(data)){
    if(is.numeric(data[[col_name]])){
      medians[col_name]<-median(data[[col_name]],na.rm=TRUE)
    } else{
      cat(paste("Skipping non-numeric column:",col_name,"\n"))
    }
  }
  
  return(medians)
}

#Test with a simple dataset
calculate_median(sample_data)

#Check with airquality dataset
calculate_median(airquality)

#Experiment with modified mtcars dataset
calculate_median(data1)

#3) Replace the missing values with its median values.
replace_with_median<-function(data){
  for(col_name in colnames(data)){
    if(is.numeric(data[[col_name]])){
      col_median<-median(data[[col_name]], na.rm=TRUE)
      data[[col_name]][is.na(data[[col_name]])]<-col_median
    } else{
      cat(paste("Skipping non_numeric column:", col_name, "\n"))
    }
  }
  return(data)
}

#Test with sample_data
sample_data_filled<-replace_with_median(sample_data)
print(sample_data_filled)

#Check with airquality dataset
airquality_filled<-replace_with_median(airquality)
print(airquality_filled)

#Experiment with modified mtcars dataset
data1_filled<-replace_with_median(data1)
print(data1_filled)

#4) Update the dataset without missing values.

#Replace missing values in sample_data and update it
sample_data<-replace_with_median(sample_data)
print("Updated sample_data:")
print(sample_data)

#Replace missing values in airquality dataset and update it
airquality<-replace_with_median(airquality)
print("Updated airquality dataset:")
print(airquality)

#Replace missing values in modified mtcars dataset and update it
data1<-replace_with_median(data1)
print("Updated data1(modified mtcars dataset:")
print(data1)
