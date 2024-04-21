library(bupaR)
# library(processmapR)
library(edeaR)
library(eventdataR)
library(dplyr)

raw_df <- read.csv('./dataset/Student_Enrollment_Event_Log.csv') 

tail(raw_df)

acb_emp_ids <- unique(raw_df$Application_Created_By)
# For loop to slice the last word of the string
for (i in 1:length(acb_emp_ids)) {
  acb_emp_ids[i] <- strsplit(acb_emp_ids[i], " ")[[1]][length(strsplit(acb_emp_ids[i], " ")[[1]])]
}
length(acb_emp_ids)


ob_emp_ids <- unique(raw_df$Opened_By)
# For loop to slice the last word of the string
for (i in 1:length(ob_emp_ids)) {
  ob_emp_ids[i] <- strsplit(ob_emp_ids[i], " ")[[1]][length(strsplit(ob_emp_ids[i], " ")[[1]])]
}
length(ob_emp_ids)


lub_emp_ids <- unique(raw_df$Last_Updated_By)
# For loop to slice the last word of the string
for (i in 1:length(lub_emp_ids)) {
  lub_emp_ids[i] <- strsplit(lub_emp_ids[i], " ")[[1]][length(strsplit(lub_emp_ids[i], " ")[[1]])]
}
length(lub_emp_ids)

cat_ids <- unique(raw_df$Enrollment_Category)
# For loop to slice the last word of the string
for (i in 1:length(cat_ids)) {
  cat_ids[i] <- strsplit(cat_ids[i], " ")[[1]][length(strsplit(cat_ids[i], " ")[[1]])]
}
length(cat_ids)
cat_ids

sub_cat_ids <- unique(raw_df$Enrollment_Subcategory)
# For loop to slice the last word of the string
for (i in 1:length(sub_cat_ids)) {
  sub_cat_ids[i] <- strsplit(sub_cat_ids[i], " ")[[1]][length(strsplit(sub_cat_ids[i], " ")[[1]])]
}
length(sub_cat_ids)
sub_cat_ids

ag_ids <- unique(raw_df$Assignment_Group)
# For loop to slice the last word of the string
for (i in 1:length(ag_ids)) {
  ag_ids[i] <- strsplit(ag_ids[i], " ")[[1]][length(strsplit(ag_ids[i], " ")[[1]])]
}
length(ag_ids)
ag_ids

ag_resolver_ids <- unique(raw_df$Assigned_To)
# For loop to slice the last word of the string
for (i in 1:length(ag_resolver_ids)) {
  ag_resolver_ids[i] <- strsplit(ag_resolver_ids[i], " ")[[1]][length(strsplit(ag_resolver_ids[i], " ")[[1]])]
}
length(ag_resolver_ids)
ag_resolver_ids

cc_ids <- unique(raw_df$Closed_Code)
# For loop to slice the last word of the string
for (i in 1:length(cc_ids)) {
  cc_ids[i] <- strsplit(cc_ids[i], " ")[[1]][length(strsplit(cc_ids[i], " ")[[1]])]
}
length(cc_ids)
cc_ids

rb_emp_ids <- unique(raw_df$Resolved_By)
# For loop to slice the last word of the string
for (i in 1:length(rb_emp_ids)) {
  rb_emp_ids[i] <- strsplit(rb_emp_ids[i], " ")[[1]][length(strsplit(rb_emp_ids[i], " ")[[1]])]
}
length(rb_emp_ids)

issue_ids <- unique(raw_df$Issue_Description)
# For loop to slice the last word of the string
for (i in 1:length(issue_ids)) {
  issue_ids[i] <- strsplit(issue_ids[i], " ")[[1]][length(strsplit(issue_ids[i], " ")[[1]])]
}
length(issue_ids)
issue_ids

dep_ids <- unique(raw_df$Department)
# For loop to slice the last word of the string
for (i in 1:length(dep_ids)) {
  dep_ids[i] <- strsplit(dep_ids[i], " ")[[1]][length(strsplit(dep_ids[i], " ")[[1]])]
}
length(dep_ids)
dep_ids

student_ids <- unique(raw_df$Student_ID)
# For loop to slice the last word of the string
for (i in 1:length(student_ids)) {
  student_ids[i] <- strsplit(student_ids[i], " ")[[1]][length(strsplit(student_ids[i], " ")[[1]])]
}
length(student_ids)
student_ids

case_ids <- unique(raw_df$Case_ID)
# For loop to slice the last word of the string
for (i in 1:length(case_ids)) {
  case_ids[i] <- strsplit(case_ids[i], " ")[[1]][length(strsplit(case_ids[i], " ")[[1]])]
}
length(case_ids)
# case_ids

# Take the `raw_df` dataframe, remove the log events with same `Case_ID` and `Enrollment_Status`, keep only the first log of those dplicates. Don't reassign to the same dataframe, create a new one instead. 

distinct_df <- distinct(raw_df, Case_ID, Enrollment_Status, .keep_all = TRUE)
write.csv(distinct_df, './dataset/Student_Enrollment_Event_Log_Distinct.csv', row.names = FALSE)


