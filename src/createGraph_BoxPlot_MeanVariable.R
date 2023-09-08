setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
results_path <- "../results/results_miguel"


data_dinic <- 0
data_ek <- 0
data_mpm <- 0

algorithms <- list.files(path=results_path, pattern=NULL, all.files=FALSE, full.names=FALSE)
for(a in algorithms){
  #print(a)
  files_path <- paste(results_path, a, sep="/")
  meanTimes_file <- paste(files_path, paste(a, "meanTimes.csv", sep="_"), sep="/")
  #print(meanTimes_file)

  if (a == "dinic") {
    data_dinic <- read.csv(meanTimes_file, header = TRUE)
  }
  else if (a == "ek") {
    data_ek <- read.csv(meanTimes_file, header = TRUE)
  }
  else if (a == "mpm") {
    data_mpm <- read.csv(meanTimes_file, header = TRUE)
  }
}

graph_paths = c("../graphs/meanTimes/boxPlot_meanTimes_Dinic.png",
                "../graphs/meanTimes/boxPlot_meanTimes_EK.png",
                "../graphs/meanTimes/boxPlot_meanTimes_MPM.png")

dataA <- data.frame(Dinic = data_dinic$mean_time,
                    EK = data_ek$mean_time,
                    MPM = data_mpm$mean_time)

aux <- 0
for (gp in graph_paths) {
  # x_label <- ""
  # dataA <- 0

  # if (aux == 0) {
  #   x_label <- "Número de Vértices"
  #   dataA <- data.frame(#Variable = data_dinic$num_vertices, 
  #                      Dinic = data_dinic$mean_time, 
  #                      EK = data_ek$mean_time, 
  #                      MPM = data_mpm$mean_time)
  # }
  # else if (aux == 1) {
  #   x_label <- "Probabilidade de Gerar Arco"
  #   dataA <- data.frame(#Variable = data_dinic$probability_arc,
  #                      Dinic = data_dinic$mean_time, 
  #                      EK = data_ek$mean_time, 
  #                      MPM = data_mpm$mean_time)
  # }
  # else if (aux == 2) {
  #   x_label <- "Capacidade Máxima de um Arco"
  #   dataA <- data.frame(#Variable = data_dinic$max_capacity, 
  #                      Dinic = data_dinic$mean_time, 
  #                      EK = data_ek$mean_time, 
  #                      MPM = data_mpm$mean_time)
  # }
  
  #print(data)
  
  png(gp, width=256, height=512)
  if (aux == 0) {
    boxplot(dataA$Dinic, ylab = "Tempos de Execução")
    title("Dinic", cex.main = 1, line = 0.5)
  }
  else if (aux == 1) {
    boxplot(dataA$EK, ylab = "Tempos de Execução")
    title("EK", cex.main = 1, line = 0.5)
  }
  else if (aux == 2) {
    boxplot(dataA$MPM, ylab = "Tempos de Execução")
    title("MPM", cex.main = 1, line = 0.5)
  }
  dev.off()

  aux <- aux + 1
}