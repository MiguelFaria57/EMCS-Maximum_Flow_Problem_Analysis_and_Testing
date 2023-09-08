setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
results_path <- "../results/results_miguel"


algorithms <- list.files(path=results_path, pattern=NULL, all.files=FALSE, full.names=FALSE)
for(a in algorithms){
  #print(a)
  files_path <- paste(results_path, a, sep="/")
  #print(files_path)
  aux <- paste(files_path, paste(a, "meanTimes.csv", sep="_"), sep="/")
  #print(aux)
  
  data <- read.csv(aux, header=TRUE)
  dataTable <- xtabs(mean_time~num_vertices+probability_arc, data)/xtabs(~num_vertices+probability_arc, data)
  data_new <- as.data.frame(dataTable)
  data_new <- data_new[order(data_new$num_vertices),]
  colnames(data_new)[3] <- "mean_time"
  
  aux <- paste(files_path, paste(a, "meanTimesWithoutMaxCapacity.csv", sep="_"), sep="/")
  write.csv(data_new, aux, row.names=FALSE)
}