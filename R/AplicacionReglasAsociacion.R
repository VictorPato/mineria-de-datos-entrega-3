library(arules)

#luego de importar la librer?a necesaria para el m?todo de las reglas de asociaci?n
#se leen los archivos .csv creados como transacciones

conventas <- read.transactions("../data/conventas.csv", sep=",")
soloventasglobales<- read.transactions("../data/soloventasglobales.csv", sep=",")
sinventas <- read.transactions("../data/sinventas.csv", sep=",")
singeneroplataformaano <- read.transactions("../data/singeneroplataformaano.csv", sep=",")
sinnumerodecriticas <- read.transactions("../data/sinnumerodecriticas.csv", sep=",")
sindesarrollador <- read.transactions("../data/sindesarrollador.csv", sep=",")
sinpublicador <- read.transactions("../data/sinpublicador.csv", sep=",")



#se aplica la metodolog?a de las reglas de asociaci?n para cada uno de las transacciones

rules <- apriori(conventas, parameter=list(support=0.01, confidence=0.5))
rules.sorted <- sort(rules, by="lift")
rules.sorted.first10 <- head(rules.sorted, 10)
inspect(rules.sorted.first10)



rules2 <- apriori(soloventasglobales, parameter=list(support=0.01, confidence=0.5))
rules2.sorted <- sort(rules2, by="lift")
rules2.sorted.first10 <- head(rules2.sorted, 10)
inspect(rules2.sorted.first10)

rules3 <- apriori(sinventas, parameter=list(support=0.01, confidence=0.5))
rules3.sorted <- sort(rules3, by="lift")
rules3.sorted.first10 <- head(rules3.sorted, 10)
inspect(rules3.sorted.first10)

rules4 <- apriori(singeneroplataformaano, parameter=list(support=0.01, confidence=0.5))
rules4.sorted <- sort(rules4, by="lift")
rules4.sorted.first10 <- head(rules4.sorted, 10)
inspect(rules4.sorted.first10)


rules5 <- apriori(sinnumerodecriticas, parameter=list(support=0.01, confidence=0.5))
rules5.sorted <- sort(rules5, by="lift")
rules5.sorted.first10 <- head(rules5.sorted, 30)
inspect(rules5.sorted.first10)


rules6 <- apriori(sinpublicador, parameter=list(support=0.01, confidence=0.5))
rules6.sorted <- sort(rules6, by="lift")
rules6.sorted.first10 <- head(rules6.sorted, 10)
inspect(rules6.sorted.first10)



rules7 <- apriori(sindesarrollador, parameter=list(support=0.01, confidence=0.5))
rules7.sorted <- sort(rules7, by="lift")
rules7.sorted.first10 <- head(rules7.sorted, 10)
inspect(rules7.sorted.first10)




#dandose cuenta de que las etiquetas de los puntajes incurren en un error, dado a que
#lass cr?ticas pueden ser muy altas por parte de los cr?ticos y muy altas por parte de los usuarios
#pero de todas formas puede haber una diferencia de puntajes de 2 puntos lo que clasificar?a como una decepci?n
#Se retoma entonces el atributo Is.fiasco que tiene valor "true" si hay una diferencia de puntaje de m?s
#de dos entre el promedio de puntajes de cr?ticos y usuarios

# Se toma el dataset que cuenta con el atributo "is.fiasco"
data_is_fiasco <- read_csv("../data/data_para_clasificadores.csv")

#se borran los atributos que no aportaron a la asociaci?n anterior
data_is_fiasco["Genre"] <- NULL
data_is_fiasco["Platform"] <- NULL
data_is_fiasco["Global_Sales"] <- NULL
data_is_fiasco["Rating"] <- NULL

data_is_fiasco["Critic_Score"] <- NULL

#se crea el dataset para poder ser leido como transacci?n
write.csv(data_is_fiasco, file = "../data/data_para_reglas_de_asociacion.csv")


#se lee el dataset
data_reglas_asociacion <- read.transactions("../data/data_para_reglas_de_asociacion.csv", sep=",")


#se aplica el m?todo de reglas de asociaci?n al dataset que cuenta con 
#el atributo is.fiasco
rules9 <- apriori(data_reglas_asociacion, parameter=list(support=0.001, confidence=0.2))
rules9.sorted <- sort(rules9, by="lift")
rules9.sorted.first10 <- head(rules9.sorted, 30)
inspect(rules9.sorted.first10)


