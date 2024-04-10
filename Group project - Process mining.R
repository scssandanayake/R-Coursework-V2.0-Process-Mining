#loading the required libraries
library(ggplot2)
library(dplyr)
library(tidyverse)

#loading the data set
data <- read.csv("D:\\My Projects 1\\R-Coursework-V2.0\\Student_Enrollment_Event_Log.csv")

View(data)  #view the data set

str(data) #get the structure of the data set & it's variables/features
glimpse(data) #view full detail about data set
names(data)  #know the columns of data set

class(data$Case_ID)  #check the class of a particular variable
unique(data$Case_ID)  #what can we found inside a particular variable
levels(data$Case_ID)  #check the levels of any variable


