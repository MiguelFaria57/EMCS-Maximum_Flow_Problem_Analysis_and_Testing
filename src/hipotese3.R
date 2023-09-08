#Terceira Hipótese
COLNAMES <- c("vertices","probabilidade","capacidade","tempo","algoritmo")
data1 <- read.csv("C:/Users/filip/Desktop/Meta3_Mei/dados.csv", sep = ";")

tempos_dinic <- data1$tempo[data1$algoritmo == "A"]
probabilidades <- data1$probabilidade[data1$algoritmo == "A"]

aov.out <-aov(tempos_dinic~probabilidades)

qqnorm(residuals(aov.out))
qqline(residuals(aov.out))
shapiro.test(tempos_dinic)
model <- lm(tempos_dinic ~ probabilidades)
fitted_values <- predict(model)
residuals <- tempos_dinic - fitted_values
dados_residuais <- data.frame(fitted = fitted_values, residuals = residuals)
ggplot(data = dados_residuais, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm")

kruskal.test(tempos_dinic~probabilidades)