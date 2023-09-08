setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
results_path <- "../results/results_miguel"


data <- 0
algorithms <- list.files(path=results_path, pattern=NULL, all.files=FALSE, full.names=FALSE)
aux <- 0
for(a in algorithms){
  #print(a)
  files_path <- paste(results_path, a, sep="/")
  meanTimes_file <- paste(files_path, paste(a, "meanTimes.csv", sep="_"), sep="/")
  #print(meanTimes_file)
  
  if (aux == 0) {
    data <- read.csv(meanTimes_file, header = TRUE)
    data$algorithm <- paste(a)
  }
  else {
    temp <- read.csv(meanTimes_file, header = TRUE)
    temp$algorithm <- paste(a)
    data <- rbind(data, temp)
  }
  
  aux <- aux + 1
}

graph_paths = c("../graphs/meanTimes/interactionPlot_meanTimes_NumVertices.png", 
                "../graphs/meanTimes/interactionPlot_meanTimes_ProbabilityArc.png", 
                "../graphs/meanTimes/interactionPlot_meanTimes_MaxCapacity.png")
aux <- 0
for (gp in graph_paths) {
  variable <- 0
  x_label <- ""
  if (aux == 0) {
    variable <- data$num_vertices
    x_label <- "Número de Vértices"
  }
  else if (aux == 1) {
    variable <- data$probability_arc
    x_label <- "Probabilidade de Gerar Arco"
  }
  else if (aux == 2) {
    variable <- data$max_capacity
    x_label <- "Capacidade Máxima de um Arco"
  }
  
  png(gp, width=512, height=512)  
  interaction.plot(x.factor = variable,              #x-axis variable
                   trace.factor = data$algorithm,    #variable for lines
                   response = data$mean_time,        #y-axis variable
                   fun = mean,                       #metric to plot
                   ylab = "Tempo Médio de Execução",
                   xlab = x_label,
                   col = c("#3483eb", "#ebcc34", "#eb4034"),
                   lty = 1, #line type
                   lwd = 2, #line width
                   trace.label = "Algoritmo",
  )
  dev.off()
  
  aux <- aux + 1
}


