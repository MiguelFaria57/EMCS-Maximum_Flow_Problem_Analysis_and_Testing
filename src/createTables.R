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
  
  temp_fileName <- paste(a, "table.csv", sep="_")
  allTimes_filePath <- paste(files_path, temp_fileName, sep="/")
  #print(allTimes_filePath)
  dataTable <- xtabs(mean_time~num_vertices+probability_arc, data)/xtabs(~num_vertices+probability_arc, data)
  #print(dataTable)
  write.csv(dataTable, allTimes_filePath, row.names=TRUE)
}