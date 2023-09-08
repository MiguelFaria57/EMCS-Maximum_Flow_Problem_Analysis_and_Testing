#Primeira hipótese
COLNAMES <- c("vertices","probabilidade","capacidade","tempo","algoritmo")
data1 <- read.csv("C:/Users/filip/Desktop/Meta3_Mei/dados.csv", sep = ";")
#transformar o tempo dos algoritmos em cúbica para analisar melhor os resultados
formula <- data1$tempo ~ data1$capacidade

#resultado <- shapiro.test(data1$tempo~ data1$capacidade)

aov.out2 <- aov(tempo ~ capacidade + Error(factor(algoritmo)),data = data1)

library(ggplot2)
qqnorm(residuals(aov.out$Within))
qqline(residuals(aov.out$Within))
residuos <- residuals(aov.out2$Within)

res <- kruskal.test(tempo~ capacidade,data=data1)
#friedman.test(tempo ~ capacidade | algoritmo, data = data1)
#plot de residuos vs fitted
model <- lm(tempo ~ capacidade+factor(algoritmo), data = data1)
fitted_values <- predict(model,data = data1)
residuals <- data1$tempo - fitted_values
dados_residuais <- data.frame(fitted = fitted_values, residuals = residuals)
ggplot(data = dados_residuais, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm")

















