#Segunda Hipótese
library(graphics)
library(ggplot2)
COLNAMES <- c("vertices","probabilidade","capacidade","tempo","algoritmo")
data1 <- read.csv("C:/Users/filip/Desktop/Meta3_Mei/dados.csv", sep = ";")

#Plot Residual
model <- lm(tempo ~ algoritmo, data = data1)
fitted_values <- predict(model,data = data1)
residuals <- data1$tempo - fitted_values
dados_residuais <- data.frame(fitted = fitted_values, residuals = residuals)
ggplot(data = dados_residuais, aes(x = fitted, y = residuals)) + geom_point() + geom_smooth(method = "lm")
#kruskal.test(tempos, data=data1)

#Divisão em vários datasets dos diferentes algoritmos
subsetDinic <- subset(data1, algoritmo == "A")
subsetEK <- subset(data1, algoritmo == "B")
subsetMPM <- subset(data1, algoritmo == "C")

#Extração de todos os valores do tempo dos 3 subsets
temposDinic <- subsetDinic[subsetDinic$probabilidade >= 0 & subsetDinic$vertices >= 0,]$tempo
temposEK <- subsetEK[subsetEK$probabilidade >= 0 & subsetEK$vertices >= 0,]$tempo
temposMPM <- subsetMPM[subsetMPM$probabilidade >= 0 & subsetMPM$vertices >= 0,]$tempo

#Diferenças entre os algoritmos Ek/Dinik 
dif_tempos_EK_Dinic <- temposEK - temposDinic
dif_tempos_EK_MPM <- temposEK - temposMPM


#Q-Q Plots
qqnorm(dif_tempos_EK_Dinic, main = "Normal Q-Q PLOT - Diferença EK-Dinic")
qqline(dif_tempos_EK_Dinic, col = "red", lwd = 2)

qqnorm(dif_tempos_EK_MPM, main = "Normal Q-Q PLOT - Diferença EK-MPM")
qqline(dif_tempos_EK_MPM, col = "red", lwd = 2)

#Após a análise dos Q-Q Plots, descartamos o t-test pareado, pois nem todos os pontos relativos às diferenças
#ficam próximos da linha média de referência, sendo que não se grante assim a normalidade das diferenças

#Shapiro Test
shapiro.test(dif_tempos_EK_Dinic)
shapiro.test(dif_tempos_EK_MPM)

wilcox.test(dif_tempos_EK_Dinic, dif_tempos_EK_MPM, paired = TRUE)
wilcox.test(temposEK, temposMPM, paired = TRUE)