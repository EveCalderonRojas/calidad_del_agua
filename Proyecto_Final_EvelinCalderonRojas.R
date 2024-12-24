#--------------------------------------------------------------------------------
#Evelin Calderón Rojas
#Lenguajes de minería de datos
#Proyecto Final
#Grupo 1
#III Cuatrimestre 2024
#--------------------------------------------------------------------------------

#Librerías con las que se trabajará
library(ggplot2)
library(dplyr)
library(corrplot)
library(openintro)
library(cluster)
library(NbClust)
library(factoextra)

#Dataset a utilizar
Water_Quality_Testing


#
#Se hace la correlación entre las variables
#
corrMat <- cor(Water_Quality_Testing)
corrplot(corrMat,method="ellipse")
x11()
#
#Se puede observar lo que contiene el dataset
#
summary(Water_Quality_Testing)

#
#Se crea el gráfico que contiene los outliers
#
boxplot(pH ~ Conductivity_microsegundocm,data = Water_Quality_Testing, col = "lightblue")
title("Conductividad del agua según el pH")

#
#Se hace un pequeño gráfico de dispersión para mostrar los datos
#
ggplot(Water_Quality_Testing, aes(x = Conductivity_microsegundocm, y = pH)) +
  geom_point() 
x11()
#
#Se hacen varios cálculos para el clúster
#
df<- Water_Quality_Testing
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")
fviz_nbclust(df, kmeans, method = "gap_stat")


#
#Se crea el clúster, en este caso se optó por uno de dos clasificaciones
#
k2 <- kmeans(df, centers = 2, nstart = 25)
k2




fviz_cluster(k2, data = df)

fviz_cluster(k2, data = df, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) 
fviz_cluster(k2, data = df, ellipse.type = "norm")
fviz_cluster(k2, data = df, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

#
#Ponemos en términos de X y Y las variables a utilizar en el gráfico que nos va a ayudar con el modelo lineal
#
x <- Water_Quality_Testing$pH
y <- Water_Quality_Testing$Conductivity_microsegundocm


# 
#Se prepara el gráfico para el modelo lineal
#
plot(x, y,
     main = "Relación entre pH y la conductividad",
     xlab = "pH",
     ylab = "Conductividad",
     pch = 19, col = "blue")

# 
#Se crea una línea de tendencia, en este caso en color rojo
#
abline(lm(Conductivity_microsegundocm ~ pH, data = Water_Quality_Testing), col = "red", lwd = 2)

#
#Se crea el modelo lineal
#
modelo <- lm(Conductivity_microsegundocm  ~ pH, data = Water_Quality_Testing)


#
#Datos obtenidos del modelo
#
summary(modelo)

#
#Se crea un modelo nuevo, esta vez agregando la variable de temperatura para mejorar el modelo
#Y se muestra la información como en el caso anterior
#
modelo_temperatura <- lm(Conductivity_microsegundocm ~ pH + Temperature_Celcius, data = Water_Quality_Testing)
summary(modelo_temperatura)

#
#Gráfico del modelo nuevo
#
ggplot(Water_Quality_Testing, aes(x = pH, y = Conductivity_microsegundocm, color = Temperature_Celcius)) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = NULL), se = FALSE) +
  labs(
    title = "Regresión Lineal Múltiple: Relación entre la Conductuvidad y el pH junto con la Temperatura",
    x = "pH",
    y = "Conductivity"
  ) +
  theme_minimal()


