#Install the required libraries for process mining
install.packages("bupaR")
install.packages("eventdataR")
install.packages("edeaR")
install.packages("processmapR")
install.packages("processmonitR")

#load the required library
library(bupaR)
library(dplyr)
library(eventdataR)
library(processmapR)
library(edeaR)
library(tidyverse)

#loading the data set
log <- read.csv("D:\\My Projects 1\\R-Coursework-V2.0\\Student_Enrollment_Event_Log.csv")

#get an summery of the data set
log

#view full data set
View(log)

#checking NA / null values
is.na(log)
sum(is.na(log))
colSums(is.na(log))

log[log == '?'] <- NA
log <- na.omit(log)

# Convert timestamp into POSIXct format
log$Opened_At<-as.POSIXct(log$Opened_At,format="%d/%m/%Y %H:%M")
log$Closed_At <- as.POSIXct(log$Closed_At, format="%d/%m/%Y %H:%M")
log$Resolved_At <- as.POSIXct(log$Resolved_At, format="%d/%m/%Y %H:%M")
log$Application_Created_At <- as.POSIXct(log$Application_Created_At, format="%d/%m/%Y %H:%M")
log$Last_Updated_At <- as.POSIXct(log$Last_Updated_At, format="%d/%m/%Y %H:%M")

# Creating event log: arrange, group, mutate, then ungroup
log <- log%>%
  arrange(Case_ID,Last_Updated_At)%>%
  group_by(Case_ID,Last_Updated_At)%>%
  mutate(activity_instance_id = paste(Case_ID,Enrollment_Status, row_number(), sep ="_"))%>%
  ungroup()

#get an summery of the data set
log

# Adding a column for resource ID (filled with NA for compatibility)
log$resource_id <- "NA"

View(log)

# Create the event log object
event_log <- eventlog(log,
                      case_id = "Case_ID",
                      activity_id = "Enrollment_Status",
                      timestamp = "Last_Updated_At",
                      resource_id = "resource_id",
                      lifecycle_id = "Student_ID",
                      activity_instance_id = "activity_instance_id")

# Before drawing the process map, ensure there are no missing or infinite values
if(any(is.na(event_log$timestamp)) || any(is.infinite(event_log$timestamp))) {
  stop("The event log contains NA or infinite timestamps, which cannot be processed.")
}


# Save event_log as a CSV file to the user's home directory
#write.csv(event_log, "D:\\My Projects 1\\R-Coursework-V2.0\\event_log_cleaned.csv", row.names = FALSE)

#loading the new cleaned data set
#log1 <- read.csv("D:\\My Projects 1\\R-Coursework-V2.0\\event_log_cleaned.csv")

#get an summery of the data set
#log1

#view full data set
#View(log1)

#draw the full process map (Complete Details)
processmapR::process_map(event_log)

#draw the full process map (Complete Details)
#Median Value
process_map(event_log, performance(median))

#draw the full process map (Complete Details)
#Median Value
process_map(event_log, performance(mean))

#draw the normal process map (Main Details)
event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .80) %>%    
  process_map(render = T)

#process map details
event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .80) %>%    
  process_map(render = F)

#Generate process map with performance measures
#Mean Value
event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .80) %>%    
  process_map(performance(mean, "mins"),
              render = T)

#Generate process map with performance measures
#Median Value
event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .80) %>%    
  process_map(performance(median, "mins"),
              render = T)

################################################################################

#Generate a matrix with activity follower frequency overview
process_matrix <- event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .80) %>%    
  process_matrix() %>% 
  plot(render=T)

#Generate a matrix with activity follower frequency overview
process_matrix <- event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .80) %>%    
  process_matrix()

# Filter activity frequency and trace frequency
filtered_log <- event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = 0.80) %>%
  process_matrix <- process_map(filtered_log) %>%
  plot(process_matrix, render = TRUE)

#Generate a matrix with activity follower frequency overview
process_matrix <- event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .80) %>%    
  process_matrix() %>% 
  plot()

#changes need to be done

###############################################################################

# Assuming event_log is already loaded and contains your event log data

# Filter activity frequency and trace frequency
filtered_log <- event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = 0.80)
  process_matrix <- process_matrix(filtered_log) 
  plot(process_matrix, render = TRUE)

#Generate variant overview
trace_explorer <- filtered_log %>%
  trace_explorer(coverage = 0.5)
  plot(trace_explorer, render = TRUE)

#Show throughput time; In hours by Application Type
event_log %>%
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  group_by(`(case)_ApplicationType`) %>% 
  throughput_time('log', units = 'hours')


