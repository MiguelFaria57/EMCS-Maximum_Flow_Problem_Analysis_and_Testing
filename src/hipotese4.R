#Quarta Hipótese
library(graphics)
library(ggplot2)
COLNAMES <- c("vertices","probabilidade","capacidade","tempo","algoritmo")
data1 <- read.csv("C:/Users/filip/Desktop/Meta3_Mei/dados.csv", sep = ";")

#Divisão em vários datasets dos diferentes algoritmos
subsetEK <- subset(data1, algoritmo == "B")

subsetEK$notTLE <- 0
subsetEK[subsetEK$tempo < 10,]$notTLE <- 1

semTempoLim <- subsetEK$notTLE
prob <- subsetEK$probabilidade

aov.out <-aov(semTempoLim ~ factor(prob))

qqnorm(residuals(aov.out))
qqline(residuals(aov.out))
shapiro.test(semTempoLim)
model <- lm(semTempoLim ~ prob)
fitted_values <- predict(model)
residuals <- semTempoLim - fitted_values
dados_residuais <- data.frame(fitted = fitted_values, residuals = residuals)
ggplot(data = dados_residuais, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm")

kruskal.test(semTempoLim ~ prob)

