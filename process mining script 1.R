###### SECTION 1 ######

#Install the required libraries for process mining
#install.packages("bupaR")
#install.packages("eventdataR")
#install.packages("edeaR")
#install.packages("processmapR")
#install.packages("processmonitR")

#load the required library for process mining
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
###### DATA CLEANING PART ########

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

# Take the `log` data-frame, remove the log events with same `Case_ID` and `Enrollment_Status`, keep only the first log of those dUplicates. 
# Don't reassign to the same data-frame, create a new one instead. 
distinct_df <- distinct(log, Case_ID, Enrollment_Status, .keep_all = TRUE)

#save cleaned dataset
#write.csv(distinct_df, "D:\\My Projects 1\\R-Coursework-V2.0\\student_log_cleaned (distinct).csv", row.names = FALSE)

######## loading the newly cleaned data set

#log1 <- read.csv("D:\\My Projects 1\\R-Coursework-V2.0\\student_log_cleaned (distinct).csv")
#log1
#View(log1)





####### SECTION 3 #######
###### DATA PREPROCESSING TO CREATE AN EVENT LOG ######

# Convert time-stamps into POSIXct format
distinct_df$Opened_At<-as.POSIXct(distinct_df$Opened_At,format="%d/%m/%Y %H:%M")
distinct_df$Closed_At <- as.POSIXct(distinct_df$Closed_At, format="%d/%m/%Y %H:%M")
distinct_df$Resolved_At <- as.POSIXct(distinct_df$Resolved_At, format="%d/%m/%Y %H:%M")
distinct_df$Application_Created_At <- as.POSIXct(distinct_df$Application_Created_At, format="%d/%m/%Y %H:%M")
distinct_df$Last_Updated_At <- as.POSIXct(distinct_df$Last_Updated_At, format="%d/%m/%Y %H:%M")

# Creating event log: arrange, group, mutate, then un-group
distinct_df <- distinct_df%>%
  arrange(Case_ID,Last_Updated_At)%>%
  group_by(Case_ID,Last_Updated_At)%>%
  mutate(activity_instance_id = paste(Case_ID,Enrollment_Status, row_number(), sep ="_"))%>%
  ungroup()

#get an summery of the data set
distinct_df

# Adding a column for resource ID (filled with NA for compatibility)
distinct_df$resource_id <- "NA"

#View(distinct_df)

# Create the event log object
event_log <- eventlog(distinct_df,
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
#log2 <- read.csv("D:\\My Projects 1\\R-Coursework-V2.0\\event_log_cleaned.csv")

#get an summery of the data set
#log2

#view full data set
#View(log2)




######## SECTION 4 ########
#### drawing process maps #####

###### process maps for the full event log ############

#draw the full process map (Complete Details)
processmapR::process_map(event_log)

#draw the full process map (Complete Details)
#Median Value
process_map(event_log, performance(median))

#draw the full process map (Complete Details)
#Mean Value
process_map(event_log, performance(mean))

##### Process Visualizations ######
processmapR::precedence_matrix(event_log) #there is a problem
processmapR::process_matrix(event_log)
processmapR::trace_explorer(event_log)
processmapR::idotted_chart(event_log)
processmapR::resource_map(event_log)
processmapR::resource_matrix(event_log)  #there is a problem

##### process dashboards #######
processmonitR::activity_dashboard(event_log)
processmonitR::resource_dashboard(event_log)
processmonitR::rework_dashboard(event_log)
processmonitR::performance_dashboard(event_log)


########## preoces maps for the filtered log ###################


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

#Generate process map with performance measures ( Mean Value )
event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .80) %>%    
  process_map(performance(mean, "mins"),
              render = T)

#Generate process map with performance measures ( Median Value )
event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .80) %>%    
  process_map(performance(median, "mins"),
              render = T)

# Filter activity frequency and trace frequency
event_log %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = 0.80)
  process_matrix <- process_matrix(event_log) 
  plot(process_matrix, render = TRUE)

#Generate variant overview
trace_explorer <- event_log %>%
  trace_explorer(coverage = 0.5)
  plot(trace_explorer, render = TRUE)

#Show throughput time; In hours by Application Type
event_log %>%
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  group_by(`(Case_ID)Enrollment_Status`) %>% 
  throughput_time('log', units = 'hours')
#there is an error in the above code  ####ERROR

#Show throughput time; In hours by Loan Goal
event_log %>%
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  group_by(`(Closed_At)Department`) %>% 
  throughput_time('log', units = 'hours')
#there is an error in the above code       ####ERROR


######### conditional process analysis #########
event_log %>%
  group_by(Case_ID, Enrollment_Status) %>%
  throughput_time("log") %>%
  plot(render= T)

event_log %>%
  group_by(Case_ID, Enrollment_Status) %>%
  throughput_time('log', units = 'hours') %>%
  plot(render= T)

