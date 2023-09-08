setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
results_path <- "../results/results_miguel"

algorithms_names = list("Dinic", "EK", "MPM")
data_algorithms <- list()
data_fileSuf <- "meanTimes"
lr_types = list("normal", "quadratic", "cubic")


algorithms <- list.files(path=results_path, pattern=NULL, all.files=FALSE, full.names=FALSE)
for(a in algorithms){
  #print(a)
  files_path <- paste(results_path, a, sep="/") 
  #print(files_path)
  aux_path <- paste(files_path, paste(a, paste(data_fileSuf, "csv", sep="."), sep="_"), sep="/")
  #print(aux_path)
  data_algorithms[[a]] <- read.csv(aux_path, header=TRUE)
}
#print(data_algorithms)

values <- 0
for(var in 1:3) { # for 3 algorithms
  da <- data_algorithms[[var]]
  #print(da)
  
  if (var == 1) {
    values <- c(0.6)
  }
  if (var == 2) {
    values <- c(0.2, 0.6, 1)
  }
  if (var == 3) {
    values <- c(0.2, 0.8)
  }
  
  for(t in lr_types) { # for 3 types of linear regression
    graph_path <- ""
    if (t == "normal") {
      graph_path <- paste(paste("../graphs/regressions/normalLinearReg_ProbabilityArc", algorithms_names[var], sep="_"), ".png")
      info_path <- paste(paste("../graphs/regressions/normalLinearReg_ProbabilityArc_Info", algorithms_names[var], sep="_"), ".csv")
    }
    else if (t == "quadratic") {
      graph_path <- paste(paste("../graphs/regressions/quadraticLinearReg_ProbabilityArc", algorithms_names[var], sep="_"), ".png")
      info_path <- paste(paste("../graphs/regressions/quadraticLinearReg_ProbabilityArc_Info", algorithms_names[var], sep="_"), ".csv")
    }
    else if (t == "cubic") {
      graph_path <- paste(paste("../graphs/regressions/cubicLinearReg_ProbabilityArc", algorithms_names[var], sep="_"), ".png")
      info_path <- paste(paste("../graphs/regressions/cubicLinearReg_ProbabilityArc_Info", algorithms_names[var], sep="_"), ".csv")
    }
    
    if (var == 1) {
        png(graph_path, width=512, height=512)
    }
    if (var == 2) {
        png(graph_path, width=1536, height=512)
    }
    if (var == 3) {
        png(graph_path, width=1024, height=512)
    }
    
    info <- data.frame(matrix(0, ncol = 4, nrow = length(values)))
    colnames(info) <- c("Probablidade Arco", "Equacao", "R^2", "R")
    
    #print(values)
    aux <- 1
    par(mfcol=c(1, length(values)))
    for (p in values) { # for the values of probability chosen
      #print(p)
      da_p <- da[da$probability_arc == round(p, 2),]
      #print(da_p)
      
      regression <- 0
      coefs <- 0
      func <- ""
      if (t == "normal") {
        regression <- lm(da_p$mean_time ~ da_p$num_vertices)
        coefs <- coef(regression)
        func <- paste("y =", format(coefs[1], scientific = TRUE, digits = 5), "+", format(coefs[2], scientific = TRUE, digits = 5), "x")
      }
      else if (t == "quadratic") {
        da_p$num_vertices_2 <- da_p$num_vertices^2
        regression <- lm(da_p$mean_time ~ da_p$num_vertices + da_p$num_vertices_2)
        coefs <- coef(regression)
        func <- paste("y =", format(coefs[1], scientific = TRUE, digits = 5), "+", format(coefs[2], scientific = TRUE, digits = 5), "x +", format(coefs[3], scientific = TRUE, digits = 5), "x^2")
      }
      else if (t == "cubic") {
        da_p$num_vertices_2 <- da_p$num_vertices^2
        da_p$num_vertices_3 <- da_p$num_vertices^3
        regression <- lm(da_p$mean_time ~ da_p$num_vertices + da_p$num_vertices_2 + da_p$num_vertices_3)
        coefs <- coef(regression)
        func <- paste("y =", format(coefs[1], scientific = TRUE, digits = 5), "+", format(coefs[2], scientific = TRUE, digits = 5), "x +", format(coefs[3], scientific = TRUE, digits = 5), "x^2 +", format(coefs[4], scientific = TRUE, digits = 5), "x^3")
      }
      
      plot(
          da_p$mean_time ~ da_p$num_vertices,
          col="#32ad6a",
          xlab="Nº Vértices",
          ylab="Tempo de Execução",
          pch = 20,
          cex=2
      )
      title(paste("Probabilidade Arco: ", p), cex.main = 1, line = 0.5)
      
      #print(summary(regression))
      #print(coefs)
      #print(func)
      predictions <- predict(regression, newdata = data.frame(x = da_p$num_vertices))
      #print(predictions)
      lines(da_p$num_vertices, predictions, col = "red", lty = 1)
      legend("topleft", legend = c("Tempos", paste("Regressão (", t, ")")), col = c("#32ad6a", "red"), pch = c(20, NA), lty = c(NA, 1))

      info[aux,] <- c(p, func, round(summary(regression)$r.squared, digits=8), round(sqrt(summary(regression)$r.squared), digits=8))
      aux <- aux + 1
    }
    mtext(algorithms_names[var], outer = TRUE, cex = 2, line = -2)
    dev.off()
    
    #print(info)
    write.csv(info, info_path, row.names = FALSE)
  }
}