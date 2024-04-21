install.packages("renv")

library(renv)
renv::init()

remotes::install_github("bupaverse/pm4py@dev")
pm4py::install_pm4py()
install.packages("reticulate")
library(reticulate)
py_install("pm4py==2.7.11.7")
pm4py <- import("pm4py")
library(pm4py)
library(pm4py)
library(processmapR)
library(bupaR)
library(dplyr)
library(eventdataR)
library(edeaR)
library(tidyverse)
library(heuristicsmineR)
library(pm4py)


help(package="pm4py")
names(pm4py)

log <- read.csv("C:/Users/kasrs/Downloads/ev.csv")

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

head(event_log)
alpha_model <- pm4py$discover_heuristics_net(event_log)

  
  if (!requireNamespace("heuristicsmineR", quietly = TRUE)) {
    install.packages("heuristicsmineR")
  }

# Load the heuristicsmineR package

library(heuristicsmineR)

# Dependency graph / matrix
dependency_matrix(patients)
# Causal graph / Heuristics net
causal_net(event_log)
help()

  