###### SECTION 1 ######

#Install the required libraries for process mining
#install.packages("bupaR")
#install.packages("eventdataR")
#install.packages("edeaR")
#install.packages("processmapR")
#install.packages("processmonitR")
#install.packages("psmineR")

#load the required library for process mining
library(bupaR)
library(eventdataR)
library(processmapR)
library(processmonitR)
library(processanimateR)
library(edeaR)
library(psmineR)
library(tidyverse)
library(dplyr)

#loading the data set
log <- read.csv("D:\\My Projects 1\\R-Coursework-V2.0\\Student_Enrollment_Event_Log.csv")

#get an summery of the data set
log

#view full data set
#View(log)

###### DETAILS ABOUT THE DATASET #######
str(log) #get the structure of the data set & it's variables/features
names(log) #get column names
summary(log) #summery of the dataset
head(log) #first 6 rows
tail(log) #last 6 rows

# assign NA (not available) for ? in records
log[log == '?'] <- NA

#checking NA / null values
is.na(log)
sum(is.na(log))
colSums(is.na(log))




###### SECTION 2 #######
###### DATA INSPECTION BEFORE DATA CLEANING ########

##this part removed

#log[log == '?'] <- NA
#log <- na.omit(log)       ### when omit NA values rows count drops to 53K ##bad step


acb_emp_ids <- unique(log$Application_Created_By)
# For loop to slice the last word of the string
for (i in 1:length(acb_emp_ids)) {
  acb_emp_ids[i] <- strsplit(acb_emp_ids[i], " ")[[1]][length(strsplit(acb_emp_ids[i], " ")[[1]])]
}
length(acb_emp_ids)


ob_emp_ids <- unique(log$Opened_By)
# For loop to slice the last word of the string
for (i in 1:length(ob_emp_ids)) {
  ob_emp_ids[i] <- strsplit(ob_emp_ids[i], " ")[[1]][length(strsplit(ob_emp_ids[i], " ")[[1]])]
}
length(ob_emp_ids)


lub_emp_ids <- unique(log$Last_Updated_By)
# For loop to slice the last word of the string
for (i in 1:length(lub_emp_ids)) {
  lub_emp_ids[i] <- strsplit(lub_emp_ids[i], " ")[[1]][length(strsplit(lub_emp_ids[i], " ")[[1]])]
}
length(lub_emp_ids)

cat_ids <- unique(log$Enrollment_Category)
# For loop to slice the last word of the string
for (i in 1:length(cat_ids)) {
  cat_ids[i] <- strsplit(cat_ids[i], " ")[[1]][length(strsplit(cat_ids[i], " ")[[1]])]
}
length(cat_ids)
cat_ids

sub_cat_ids <- unique(log$Enrollment_Subcategory)
# For loop to slice the last word of the string
for (i in 1:length(sub_cat_ids)) {
  sub_cat_ids[i] <- strsplit(sub_cat_ids[i], " ")[[1]][length(strsplit(sub_cat_ids[i], " ")[[1]])]
}
length(sub_cat_ids)
sub_cat_ids

ag_ids <- unique(log$Assignment_Group)
# For loop to slice the last word of the string
for (i in 1:length(ag_ids)) {
  ag_ids[i] <- strsplit(ag_ids[i], " ")[[1]][length(strsplit(ag_ids[i], " ")[[1]])]
}
length(ag_ids)
ag_ids

ag_resolver_ids <- unique(log$Assigned_To)
# For loop to slice the last word of the string
for (i in 1:length(ag_resolver_ids)) {
  ag_resolver_ids[i] <- strsplit(ag_resolver_ids[i], " ")[[1]][length(strsplit(ag_resolver_ids[i], " ")[[1]])]
}
length(ag_resolver_ids)
ag_resolver_ids

cc_ids <- unique(log$Closed_Code)
# For loop to slice the last word of the string
for (i in 1:length(cc_ids)) {
  cc_ids[i] <- strsplit(cc_ids[i], " ")[[1]][length(strsplit(cc_ids[i], " ")[[1]])]
}
length(cc_ids)
cc_ids

rb_emp_ids <- unique(log$Resolved_By)
# For loop to slice the last word of the string
for (i in 1:length(rb_emp_ids)) {
  rb_emp_ids[i] <- strsplit(rb_emp_ids[i], " ")[[1]][length(strsplit(rb_emp_ids[i], " ")[[1]])]
}
length(rb_emp_ids)

issue_ids <- unique(log$Issue_Description)
# For loop to slice the last word of the string
for (i in 1:length(issue_ids)) {
  issue_ids[i] <- strsplit(issue_ids[i], " ")[[1]][length(strsplit(issue_ids[i], " ")[[1]])]
}
length(issue_ids)
issue_ids

dep_ids <- unique(log$Department)
# For loop to slice the last word of the string
for (i in 1:length(dep_ids)) {
  dep_ids[i] <- strsplit(dep_ids[i], " ")[[1]][length(strsplit(dep_ids[i], " ")[[1]])]
}
length(dep_ids)
dep_ids

student_ids <- unique(log$Student_ID)
# For loop to slice the last word of the string
for (i in 1:length(student_ids)) {
  student_ids[i] <- strsplit(student_ids[i], " ")[[1]][length(strsplit(student_ids[i], " ")[[1]])]
}
length(student_ids)
student_ids

case_ids <- unique(log$Case_ID)
# For loop to slice the last word of the string
for (i in 1:length(case_ids)) {
  case_ids[i] <- strsplit(case_ids[i], " ")[[1]][length(strsplit(case_ids[i], " ")[[1]])]
}
length(case_ids)
case_ids





####### SECTION 3 #######
###### DATA PREPROCESSING TO CREATE AN EVENT LOG ######

# Convert time-stamps into POSIXct format
log$Opened_At<-as.POSIXct(log$Opened_At,format="%d/%m/%Y %H:%M")
log$Closed_At <- as.POSIXct(log$Closed_At, format="%d/%m/%Y %H:%M")
log$Resolved_At <- as.POSIXct(log$Resolved_At, format="%d/%m/%Y %H:%M")
log$Application_Created_At <- as.POSIXct(log$Application_Created_At, format="%d/%m/%Y %H:%M")
log$Last_Updated_At <- as.POSIXct(log$Last_Updated_At, format="%d/%m/%Y %H:%M")

# Creating event log: arrange, group, mutate, then un-group
log <- log%>%
  arrange(Case_ID,Last_Updated_At)%>%
  group_by(Case_ID,Last_Updated_At)%>%
  mutate(activity_instance_id = paste(Case_ID,Enrollment_Status, row_number(), sep ="_"))%>%
  ungroup()

#get an summery of the data set
log

# Adding a column for resource ID (filled with NA for compatibility)
log$resource_id <- "NA"

#View(log)

# Create the event log object
event_log <- eventlog(log,
                      case_id = "Case_ID",
                      activity_id = "Enrollment_Status",
                      timestamp = "Last_Updated_At",
                      resource_id = "resource_id",
                      lifecycle_id = "Student_ID",
                      activity_instance_id = "activity_instance_id")

# Before drawing the process map, ensure there are no missing or infinite values
if(any(is.na(event_log$Last_Updated_At)) || any(is.infinite(event_log$Last_Updated_At))) {
  stop("The event log contains NA or infinite timestamps, which cannot be processed.")
} else {
   print("The event log is good to go.")
}


# Save event_log as a CSV file to the user's home directory
#write.csv(event_log, "D:\\My Projects 1\\R-Coursework-V2.0\\event_log_cleaned.csv", row.names = FALSE)

#loading the new cleaned data set
#log2 <- read.csv("D:\\My Projects 1\\R-Coursework-V2.0\\event_log_cleaned.csv")

#get an summery of the data set
#log2

#view full data set
#View(log2)





###### SECTION 4 #######
###### DATA CLEANING PART ########

#### NEW CLEANING #####
# Remove the cases which does not have consecutive modification counts
has_consecutive <- function(x) {
  any(diff(x) == 1)
}

non_consecutive_cases <- event_log %>%
  group_by(Case_ID) %>%
  summarise(consecutive = has_consecutive(Modification_Count)) %>%
  filter(!consecutive)

print(non_consecutive_cases)

indices_to_remove <- non_consecutive_cases$Case_ID

filtered_event_log <- event_log %>% filter(!(Case_ID %in% indices_to_remove))

non_consecutive_cases <- filtered_event_log %>%
  group_by(Case_ID) %>%
  summarise(consecutive = has_consecutive(Modification_Count)) %>%
  filter(!consecutive)

print(non_consecutive_cases)

event_log <- filtered_event_log

# Take the `log` data-frame, remove the log events with same `Case_ID` and `Enrollment_Status`, keep only the first log of those dUplicates. 
# Don't reassign to the same data-frame, create a new one instead. 
event_log <- event_log %>%
  distinct(Case_ID, Enrollment_Status, .keep_all = TRUE)


# Remove cases with very rare activity statuses
removable_cases <- event_log[event_log$Enrollment_Status == -100, ]$Case_ID
event_log <- event_log[!(event_log$Case_ID %in% removable_cases), ]

#save cleaned dataset
#write.csv(event_log, "D:\\My Projects 1\\R-Coursework-V2.0\\student_log_cleaned (distinct).csv", row.names = FALSE)

######## loading the newly cleaned data set
#log1 <- read.csv("D:\\My Projects 1\\R-Coursework-V2.0\\student_log_cleaned (distinct).csv")
#log1
#View(log1)





### DEPTH ANALYSIS ABOUT THE EVENT LOG ###
## details of EVENT_LOG ##
bupaR::cases(event_log)
bupaR::activities(event_log)
bupaR::n_traces(event_log)
bupaR::trace_list(event_log) %>% print(n=85)
bupaR::mapping(event_log)

edeaR::activity_frequency(event_log)
edeaR::activity_presence(event_log)
edeaR::end_activities(event_log)
edeaR::idle_time(event_log)
edeaR::number_of_repetitions(event_log)
edeaR::number_of_selfloops(event_log)
edeaR::number_of_traces(event_log)
edeaR::processing_time(event_log)
edeaR::resource_frequency(event_log)
edeaR::resource_involvement(event_log)
edeaR::resource_specialisation(event_log)
edeaR::size_of_repetitions(event_log)
edeaR::size_of_selfloops(event_log)
edeaR::start_activities(event_log)
edeaR::throughput_time(event_log)
edeaR::trace_coverage(event_log)
edeaR::trace_length(event_log)



######## SECTION 4 ########

#### DRAWING PROCESS MAPS 1#####
###### process maps for the full event log ############


##normal data details
event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = 1.0)

#draw the full process map (Complete Details)
process_map(event_log)
#animated map
#animate_process(event_log)


#draw the full process map (Complete Details)
#Median Values
process_map(event_log, performance(median, "minutes"))

process_map(event_log, performance(median,"days"))

process_map(event_log, performance(median,"hours"))

#draw the full process map (Complete Details)
#Mean Values
process_map(event_log, performance(mean))

process_map(event_log, performance(mean, "days"))

process_map(event_log, performance(mean, "hours"))



#### DRAWING PROCESS MAPS 2 #####
########## process maps for the filtered log_data ###################

##filtering the data for the next maps
filtered_data <- event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = 0.95)
##filter data details
event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = 0.95)

bupaR::n_traces(filtered_data)
bupaR::trace_list(filtered_data) %>% print(n=85)

#draw the filtered process map (Main Details)
filtered_data %>% process_map(render = T)
#animated map
#animate_process(filtered_data)

#process map details
filtered_data %>% process_map(render = F)

#Generate process map with performance measures ( Mean Value )
filtered_data %>%process_map(performance(mean, "mins"),render = T)

filtered_data %>%process_map(performance(mean, "days"),render = T)

filtered_data %>%process_map(performance(mean, "hours"),render = T)

#Generate process map with performance measures ( Median Value )
filtered_data %>%process_map(performance(median, "mins"),render = T)

filtered_data %>%process_map(performance(median, "days"),render = T)

filtered_data %>%process_map(performance(median, "hours"),render = T)

# Generate process matrix for original data
process_matrix <- process_matrix(event_log)
plot(process_matrix, render = TRUE)
#perfomnace matrix
event_log %>%
  process_matrix(performance(FUN = mean, units = "days"))  %>%
  plot()

# Generate process matrix for filterd data
process_matrix <- process_matrix(filtered_data)
plot(process_matrix, render = TRUE)
#perfomnace matrix
filtered_data %>%
  process_matrix(performance(FUN = mean, units = "days"))  %>%
  plot()

#Generate variant overview
trace_explorer <- event_log %>%
  trace_explorer(coverage = 0.5)
  plot(trace_explorer, render = TRUE)
  
trace_explorer <- filtered_data %>%
  trace_explorer(coverage = 0.5)
  plot(trace_explorer, render = TRUE)

event_log %>%
  trace_explorer(n_traces = 10, label_size = 4)

event_log %>%
  trace_explorer(n_traces = 10,
                 coverage_labels = c("cumulative", "relative"))

event_log %>%
  trace_explorer(n_traces = 10, label_size = 4,
                 scale_fill = ggplot2::scale_fill_discrete)
  
######## SECTION 5 ########
##### OTHER PROCESS VISUALIZATIONS ######
# precedence_matrix = process_matrix
processmapR::precedence_matrix(event_log)
data <- precedence_matrix(event_log)
data %>% plot()

#process_matrix
processmapR::process_matrix(event_log)
data1 <- process_matrix(event_log)
data1 %>% plot()
plot(process_matrix(event_log))


#other maps
processmapR::trace_explorer(event_log)
processmapR::idotted_chart(event_log)
processmapR::resource_map(event_log)

#resource_matrix
processmapR::resource_matrix(event_log)
data2 <- resource_matrix(event_log)
data2 %>% plot()

#activity frequency
event_log %>% activity_frequency(level = "activity") %>% plot()

##### PROCESSS DASHBOARDS #######
processmonitR::activity_dashboard(event_log)
processmonitR::resource_dashboard(event_log)
processmonitR::rework_dashboard(event_log)
processmonitR::performance_dashboard(event_log)
  
#Performance Spectrum
event_log %>%
  ps_detailed(n_segments = 10)

#dotted graph
event_log %>%
  dotted_chart(x = "absolute")

filtered_data %>%
  dotted_chart(x = "absolute")

event_log %>%
  dotted_chart(x = "relative")

filtered_data %>%
  dotted_chart(x = "relative")

######### SECTION 6 #########
######## conditional process analysis #########

#Show throughput time; In hours by Active Type
filtered_data %>%
  group_by(`Active_Status`) %>% 
  throughput_time('log', units = 'hours') %>% 
  plot(render = T)
  
event_log %>%
  group_by(`Active_Status`) %>% 
  throughput_time('log', units = 'hours') %>% 
  plot(render = T)

#Show throughput time; In hours by Notification_Status
filtered_data %>%
  group_by(`Notification_Status`) %>% 
  throughput_time('log', units = 'hours') %>% 
  plot(render = T)

event_log %>%
  group_by(`Notification_Status`) %>% 
  throughput_time('log', units = 'hours') %>% 
  plot(render = T)

#Show throughput time; In hours by Notification_Status and active status
filtered_data %>%
  group_by(`Notification_Status`,`Active_Status`) %>% 
  throughput_time('log', units = 'hours') %>% 
  plot(render = T)

event_log %>%
  group_by(`Notification_Status`,`Active_Status`) %>% 
  throughput_time('log', units = 'hours') %>% 
  plot(render = T)

filtered_data %>%
  group_by(`Notification_Status`,`Active_Status`) %>% 
  throughput_time('log', units = 'hours')

glimpse(event_log)
glimpse(filtered_data)

event_log %>%
  group_by(`Closed_At`) %>% 
  throughput_time('log', units = 'hours') %>% 
  plot(render = T)

event_log %>%
  group_by(`Last_Updated_At`) %>% 
  throughput_time('log', units = 'hours') %>% 
  plot(render = T)




###### SECTION 7 #######
######## CALCULATE THE OPTIMAL PATH ###########
##filtering the data for the next maps
filtered_data1 <- event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = 0.978)

#get the traces and trace list
bupaR::n_traces(filtered_data1)
bupaR::trace_list(filtered_data1) %>% print(n=85)

#draw the filtered process map (Main Details)
filtered_data1 %>% process_map(render = T)
#animated map
#animate_process(filtered_data1)

#process map details
filtered_data1 %>% process_map(render = F)

#Generate process map with performance measures ( Mean Value )
filtered_data1 %>%process_map(performance(mean, "mins"),render = T)

filtered_data1 %>%process_map(performance(mean, "days"),render = T)

#animated map
#animate_process(filtered_data1)

filtered_data1 %>%process_map(performance(mean, "hours"),render = T)
