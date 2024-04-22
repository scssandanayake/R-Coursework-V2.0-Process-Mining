#install.packages("renv")

library(renv)
renv::init()

NN#NNremotes::install_github("bupaverse/pm4py@dev")
#pm4py::install_pm4py()
#install.packages("reticulate")
#ibrary(reticulate)
#py_install("pm4py==2.7.11.7")
#pm4py <- import("pm4py")

#library(pm4py)
library(processmapR)
library(bupaR)
library(dplyr)
library(eventdataR)
library(edeaR)
library(tidyverse)
library(heuristicsmineR)
#library(pm4py)


#help(package="pm4py")
#names(pm4py)

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

head(event_log)

#alpha_model <- pm4py$discover_heuristics_net(event_log)

  
#if (!requireNamespace("heuristicsmineR", quietly = TRUE)) {
  install.packages("heuristicsmineR")
}

# Load the neccesary package

#install.packages("reticulate")
#install.packages("DiagrammeRsvg")
#install.packages("svgPanZoom")
#install.packages("igraph")

library(heuristicsmineR)
library(DiagrammeRsvg)
library(magrittr)
library(igraph)

#get bupar packages
help(package = "bupaR")

# Causal graph / Heuristics net
cn <- causal_net(event_log,threshold = 0.95)
pn <- as.petrinet(cn)
#drawing the petrinet
petrinetR::render_PN(pn)






######## FIND POTIMAL PATH #########
places <- pn$places$id
transitions <- pn$transitions$id

# Create igraph vertices for places and transitions
vertices <- c(places, transitions)

# Create igraph edges from preset and postset of transitions
edges <- c()
for (t in transitions) {
  pre <- pn$transitions$preset[t]
  post <- pn$transitions$postset[t]
  edges <- c(edges, paste0(pre, "-", t), paste0(t, "-", post))
}

# Extract unique vertices from edges
unique_vertices <- unique(unlist(strsplit(edges, "-")))

# Check if all vertices are present in the vertex list
missing_vertices <- setdiff(unique_vertices, vertices)
if (length(missing_vertices) > 0) {
  stop("Some vertices in the edge list are not listed in the vertex data.")
}

# Create igraph graph
graph <- graph_from_data_frame(data.frame(from = substr(edges, 1, 1), to = substr(edges, 3, 3)), vertices = vertices, directed = TRUE)

# Get all possible paths in the Petri net
all_paths <- all_simple_paths(graph, from = V(graph)[degree(graph, mode = "in") == 0], to = V(graph)[degree(graph, mode = "out") == 0])

# Print all possible paths
print(all_paths)

# Get the optimal path based on a certain criterion (e.g., shortest path)
optimal_path <- shortest_paths(graph, from = V(graph)[degree(graph, mode = "in") == 0], to = V(graph)[degree(graph, mode = "out") == 0], mode = "out")$vpath

# Print the optimal path
print(optimal_path)

#######################
# Get the optimal path based on a certain criterion (e.g., shortest path)
shortest_paths_result <- shortest_paths(graph, from = V(graph)[degree(graph, mode = "in") == 0], to = V(graph)[degree(graph, mode = "out") == 0], mode = "out")

# Extract and format the optimal path vertices
optimal_path_vertices <- lapply(shortest_paths_result$vpath, function(path) {
  if (length(path) > 0) {
    path_names <- names(path)
    vertex_names <- V(graph)$name[path_names]
    paste(vertex_names, collapse = " -> ")
  } else {
    "No valid path found"
  }
})

# Filter out paths where no valid path was found
valid_optimal_paths <- optimal_path_vertices[sapply(optimal_path_vertices, function(path) path != "No valid path found")]

# Print the valid optimal paths
print(valid_optimal_paths)








#generate causal net
causal_net(event_log,threshold = 0.95) %>%
  render_causal_net(render = TRUE) %>%
  DiagrammeRsvg::export_svg() %>%
  svgPanZoom::svgPanZoom()

#get the parallel_matrix_lifecycle
martix <-parallel_matrix_lifecycle(event_log)
print(matrix)

# matrix_lifecycle
matrix_lifecycle <- parallel_matrix_lifecycle(event_log)
print(matrix_lifecycle)
plot(matrix_lifecycle)

# Example from Process mining book
dependency_matrix(event_log, threshold =0.95)

#get the precedence_matrix_absolute
m <- precedence_matrix_absolute(event_log)
as.matrix(m)






