########### 1 - creacion de la tabla y tipos ################
## cambiar esta linea al path del .csv
## si se mantiene la estructura de carpetas del repositorio, basta con
## dejar el working directory en la ubicacion de este archivo
library(readr)
videogames <-
  read_csv("../data/Video_Games_Sales_as_at_22_Dec_2016.csv")

# se eliminan las filas que tengan NA (quedan aprox 7000 resultados)
filtered_vid = na.omit(videogames)

# se arreglan los tipos: user-score a numeric, los otros char a factor
filtered_vid$User_Score <- as.numeric(filtered_vid$User_Score)
character_vars <- lapply(filtered_vid, class) == "character"
filtered_vid[, character_vars] <-
  lapply(filtered_vid[, character_vars], as.factor)

# se define la diferencia necesaria para considerar que un juego fue un fiasco
# (al comparar user_score con critic_score)
dif_for_fail = 2

#se crea y pobla la columna que define si un juego es un fiasco o no
filtered_vid["Is_Fiasco"] <- NA
filtered_vid$Is_Fiasco <-
  ((filtered_vid$Critic_Score / 10) - filtered_vid$User_Score) > dif_for_fail

########## 2 - reduccion de dimensionalidad para Machine Learning ##########
## eliminar categorias
reduced <- filtered_vid
reduced["User_Count"] <- NULL
reduced["User_Score"] <- NULL
reduced["Critic_Count"] <- NULL
reduced["Other_Sales"] <- NULL
reduced["EU_Sales"] <- NULL
reduced["JP_Sales"] <- NULL
reduced["NA_Sales"] <- NULL
reduced["Year_of_Release"] <- NULL
reduced["Name"] <- NULL
reduced["Developer"] <- NULL
reduced["Developer"] <- NULL
## tomar solo las n consolas mas populares:
n = 5
popular_console <- reduced
popular_console["counter"] <- 1
popular_console = aggregate(counter ~ Platform, popular_console, FUN = sum)
popular_console = popular_console[order(popular_console$counter, decreasing = T), ]
reduced <-
  reduced[reduced$Platform %in% popular_console[1:n, ]$Platform, ]

## tomar solo los n mayores publicadores:
n = 10
popular_publisher <- reduced
popular_publisher["counter"] <- 1
popular_publisher = aggregate(counter ~ Publisher, popular_publisher, FUN =
                                sum)
popular_publisher = popular_publisher[order(popular_publisher$counter, decreasing = T), ]
reduced <-
  reduced[reduced$Publisher %in% popular_publisher[1:n, ]$Publisher, ]
#### Descomentar siguiente linea para exportar el dataset
#write.csv(reduced, file = "../data/data_para_clasificadores.csv")



################    CLUSTERING     ##############


datacluster <-
  data.frame(reduced["Global_Sales"], reduced["Critic_Score"])#se hace un vector de 2 variables para hacer un cluster
plot(datacluster) #lo miro, el me mira

# vemos el codo para ver cual k tomar ---------------
set.seed(2)
wss <- 0
clust = 15 # graficaremos hasta 15 clusters
for (i in 1:clust) {
  wss[i] <-
    sum(kmeans(datacluster, centers = i)$withinss)
}
plot(1:clust,
     wss,
     type = "b",
     xlab = "Numero de clusters",
     ylab = "wss")

# hacemos kmeans k=3 ------------------------------------
set.seed(2)
km.out <- kmeans(datacluster, 3, nstart = 20)
# vemos los resultados
plot(
  datacluster,
  col = (km.out$cluster),
  main = "Resultados usando k = 3 (plot con colores)",
  xlab = "Global_Sales",
  ylab = "Critic_Scores",
  pch = 20,
  cex = 2
)

library(ggplot2) # instalar si es necesario
# install.packages("GGally")
library(GGally)
#otra forma de visualizar
datacluster$cluster <- factor(km.out$cluster)
ggpairs(datacluster, aes(colour = cluster), alpha = 0.4)
#más aún
library("cluster") # instalar si es necesario
clusplot(
  datacluster[, c(1, 2)],
  km.out$cluster,
  color = TRUE,
  shade = TRUE,
  labels = 2,
  lines = 0,
  main = "IPCAP (k=3)"
)


hc.complete <- hclust(dist(datacluster), method = "complete")
hc.single <- hclust(dist(datacluster), method = "single")
hc.average <- hclust(dist(datacluster), method = "average")
par(mfrow = c(1, 3))
plot(
  hc.complete,
  main = "Complete",
  xlab = "Global_Sales",
  ylab = "Critic_Scores",
  cex = .9
)
plot(
  hc.single,
  main = "Single",
  xlab = "Global_Sales",
  ylab = "Critic_Scores",
  cex = .9
)
plot(
  hc.average,
  main = "Average",
  xlab = "Global_Sales",
  ylab = "Critic_Scores",
  cex = .9
)

library("dbscan")
db <- dbscan(datacluster[, c(1, 2)], eps = 1, minPts = 20)