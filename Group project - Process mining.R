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

#installing the required packages for process mining
install.packages("bupaR")
install.packages("processmapR")
install.packages("eventdataR")

#loading the required libraries for process mining
library(bupaR)
library(eventdataR)
library(processmapR)
library(edeaR)
library(dplyr)
library(tidyverse)

data <- data %>%
  arrange(Case_ID,Opened_At)%>%
  group_by(Case_ID)%>%
  mutate(activity_instance_id = paste(Case_ID,Enrollment_Status, row_number(), sep="_"))%>%
  ungroup()

dim(data)

head(data)

class(data)

mapping(data) #there is an error after this code line

summary(data)

eventlog <- read_csv("D:\\My Projects 1\\R-Coursework-V2.0\\Student_Enrollment_Event_Log.csv")

# Example preprocessing steps
eventlog <- filter_trace_starts(eventlog, condition = "activity %in% c('Start', 'TaskA', 'TaskB')")
eventlog <- adjust_time_zone(eventlog, new_zone = "GMT")

process_map <- process_map(eventlog)

plot(process_map)


# Convert dataframe to event log object
eventlog <- as_eventlog(eventlog_df, case_id = "CaseID", activity_id = "Activity", timestamp = "Timestamp")

# Create process map
process_map <- process_map(eventlog)

# Visualize process map
plot(process_map)

