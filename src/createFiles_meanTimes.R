setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
results_path <- "../results/results_miguel"

COLNAMES <- c("num_vertices", "probability_arc", "max_capacity", "max_flow", "time")


algorithms <- list.files(path=results_path, pattern=NULL, all.files=FALSE, full.names=FALSE)
for(a in algorithms){
  #print(a)
  files_path <- paste(results_path, a, sep="/")
  data <- 0
  aux <- paste(a, "output", sep="_")
  #print(aux)
  runs <- list.files(path=files_path, pattern=aux, all.files=FALSE, full.names=TRUE)
  aux <- 0
  for (r in runs) {
    #print(r)
    if (aux == 0) {
      data <- read.csv(r, header = FALSE)
      colnames(data) <- COLNAMES
    }
    else {
      temp <- read.csv(r, header = FALSE)
      colnames(temp) <- COLNAMES
      data <- rbind(data, temp)
    }
    
    aux <- aux + 1
  }

  temp_fileName <- paste(a, "all.csv", sep="_")
  allTimes_filePath <- paste(files_path, temp_fileName, sep="/")
  #print(meanTimes_filePath)
  data <- data[order(data$num_vertices),]
  write.csv(data, allTimes_filePath, row.names=FALSE)
  
  data_mean <- aggregate(data$time, by = list(num_vertices=data$num_vertices, probability_arc=data$probability_arc, max_capacity=data$max_capacity), FUN = mean)
  names(data_mean)[4]<-paste("mean_time")
  temp_fileName <- paste(a, "meanTimes.csv", sep="_")
  meanTimes_filePath <- paste(files_path, temp_fileName, sep="/")
  #print(meanTimes_filePath)
  data_mean <- data_mean[order(data_mean$num_vertices),]
  write.csv(data_mean, meanTimes_filePath, row.names=FALSE)
  
  png(paste(paste("../graphs/scatterPlot_meanTimes_All", a, sep="_"), "png", sep="."), width=1024, height=1024)
  plot(data_mean)
  dev.off()
}
