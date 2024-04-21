remotes::install_github("bupaverse/pm4py@dev")
pm4py::install_pm4py()
install.packages("pm4py")
library(pm4py)
library(processmapR)
library(bupaR)
library(dplyr)
library(eventdataR)
library(edeaR)
library(tidyverse)
library(heuristicsmineR)
library(pm4py)

log <- read.csv("D:\\My Projects 1\\R-Coursework-V2.0\\Student_Enrollment_Event_Log.csv")

log$Opened_At<-as.POSIXct(log$Opened_At,format="%d/%m/%Y %H:%M")
log$Closed_At <- as.POSIXct(log$Closed_At, format="%d/%m/%Y %H:%M")
log$Resolved_At <- as.POSIXct(log$Resolved_At, format="%d/%m/%Y %H:%M")
log$Application_Created_At <- as.POSIXct(log$Application_Created_At, format="%d/%m/%Y %H:%M")
log$Last_Updated_At <- as.POSIXct(log$Last_Updated_At, format="%d/%m/%Y %H:%M")

log <- log%>%
  arrange(Case_ID,Last_Updated_At)%>%
  group_by(Case_ID,Last_Updated_At)%>%
  mutate(activity_instance_id = paste(Case_ID,Enrollment_Status, row_number(), sep ="_"))%>%
  ungroup()

log$resource_id <- "NA"

distinct_df <- distinct(log, Case_ID, Enrollment_Status, .keep_all = TRUE)

event_log <- eventlog(distinct_df,
                      case_id = "Case_ID",
                      activity_id = "Enrollment_Status",
                      timestamp = "Last_Updated_At",
                      resource_id = "resource_id",
                      lifecycle_id = "Student_ID",
                      activity_instance_id = "activity_instance_id")

alpha_model <- discovery_alpha(event_log)