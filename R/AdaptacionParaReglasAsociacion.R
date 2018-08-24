library(readr)  #cargamos readr para poder leer csv's
library(arules)  # cargamos arules
videogames <- read_csv("../data/Video_Games_Sales_as_at_22_Dec_2016.csv")
auxiliar <- videogames #usamos un auxiliar para realizar cambios sin modificar el original


# se eliminan las filas que tengan NA (quedan aprox 7000 resultados)
sinna = na.omit(videogames)
sinnauxiliar = na.omit(videogames)
#se definen los intervalos para calificar los atributos num?ricos y asignarles una etiqueta
#En caso de haber un n?mero mucho m?s alto que los dem?s, se considera outlier y se le asigna
#la etiqueta "much?simo" mientras que el segundo n?mero m?s alto se divide en 5 para crear
#los intervalos si es que no es un outlier como el anterior

#ventas en Norte Am?rica
Muchisimas = 40
Muchas = 3.136+3.136+3.136+3.136+3.136
Bastante = 3.136+3.136+3.136+3.136
Intermedio = 3.136+3.136+3.136
Pocas= 3.136+3.136
Muypocas = 3.136
ventasnulas_o_sin_informacion = 0

#ventas en Europa
Muchisimas2 = 28.96
Muchas2 = 2.552+2.552+2.552+2.552+2.552
Bastante2 = 2.552+2.552+2.552+2.552
Intermedio2 = 2.552+2.552+2.552
Pocas2= 2.552+2.552
Muypocas2 = 2.552
ventasnulas_o_sin_informacion2 = 0

#ventas en Jap?n
Muchas3 = 1.3+1.3+1.3+1.3+1.3
Bastante3 = 1.3+1.3+1.3+1.3
Intermedio3 = 1.3+1.3+1.3
Pocas3= 1.3+1.3
Muypocas3 = 1.3
ventasnulas_o_sin_informacion3 = 0

#Otras ventas
Muchas4 = 2.114+2.114+2.114+2.114+2.114
Bastante4 = 2.114+2.114+2.114+2.114
Intermedio4 = 2.114+2.114+2.114
Pocas4 = 2.114+2.114
Muypocas4 = 2.114
ventasnulas_o_sin_informacion3 = 0

#Ventas globales
Muchas5 = 82.53
Bastante5 = 2+2+2+2
Intermedio5 = 2+2+2
Pocas5 = 2+2
Muypocas5 = 2
ventasnulas_o_sin_informacion4 = 0

#se crea una columna extra para ventas de norte am?rica
sinna["Ventas NA"] <- NA

# se asignan las etiquetas a las ventas de norte am?rica
for (i in 1:6947){
  if(sinna[i,6]>Muchisimas){
    sinna[i,17]="muchisimas ventas en NA"  }
  else if (sinna[i,6]>Bastante){
    sinna[i,17]="muchas ventas en NA"  }
  else if (sinna[i,6]>Intermedio){
    sinna[i,17]="Bastantes ventas en NA"  }
  else if (sinna[i,6]>Pocas){
    sinna[i,17]="ventas en NA intermedias"  }
  else if (sinna[i,6]>Muypocas){
    sinna[i,17]="pocas ventas en NA"  }
  else if (sinna[i,6]>ventasnulas_o_sin_informacion){
    sinna[i,17]="muy pocas ventas en NA"  }
  else if (sinna[i,6]==ventasnulas_o_sin_informacion){
    sinna[i,17]="nulas ventas en NA o sin informacion"  }
}


#se crea una columna extra para ventas de Europa
sinna["Ventas EU"] <- NA

for (i in 1:6947){
  if(sinna[i,7]==Muchisimas2){
    sinna[i,18]="muchisimas Ventas en EU"  }
  else if (sinna[i,7]>Bastante2){
    sinna[i,18]="muchas Ventas en EU"  }
  else if (sinna[i,7]>Intermedio2){
    sinna[i,18]="Bastantes Ventas en EU "  }
  else if (sinna[i,7]>Pocas2){
    sinna[i,18]=" Ventas en EU intermedias"  }
  else if (sinna[i,7]>Muypocas2){
    sinna[i,18]="pocas Ventas en EU"  }
  else if (sinna[i,7]>ventasnulas_o_sin_informacion){
    sinna[i,18]="muy pocas Ventas en EU"  }
  else if (sinna[i,7]==ventasnulas_o_sin_informacion){
    sinna[i,18]="Ventas en EU nulas o sin informacion"  }
}


#se crea una columna extra para ventas de Japon
sinna["Ventas JP"] <- NA

for (i in 1:6947){
  if (sinna[i,8]>Bastante3){
    sinna[i,19]="muchas Ventas en JP"  }
  else if (sinna[i,8]>Intermedio3){
    sinna[i,19]="Bastantes Ventas en JP "  }
  else if (sinna[i,8]>Pocas3){
    sinna[i,19]="Ventas en JP intermedias"  }
  else if (sinna[i,8]>Muypocas3){
    sinna[i,19]="pocas Ventas en JP"  }
  else if (sinna[i,8]>ventasnulas_o_sin_informacion){
    sinna[i,19]="muy pocas Ventas en JP"  }
  else if (sinna[i,8]==ventasnulas_o_sin_informacion){
    sinna[i,19]="nulas Ventas en JP o sin informacion"  }
}


#se crea una columna extra para "otras" ventas
sinna["Ventas otras"] <- NA

for (i in 1:6947){
  if (sinna[i,9]>Bastante4){
    sinna[i,20]="muchas Ventas otras"  }
  else if (sinna[i,9]>Intermedio4){
    sinna[i,20]="Bastantes Ventas otras "  }
  else if (sinna[i,9]>Pocas4){
    sinna[i,20]="Ventas otras intermedias"  }
  else if (sinna[i,9]>Muypocas4){
    sinna[i,20]="pocas Ventas otras"  }
  else if (sinna[i,9]>ventasnulas_o_sin_informacion){
    sinna[i,20]="muy pocas Ventas otras"  }
  else if (sinna[i,9]==ventasnulas_o_sin_informacion){
    sinna[i,20]="Ventas otras nulas o sin informacion"  }
}


#se crea una columna extra para "otras" ventas
sinna["Ventas globales"] <- NA

for (i in 1:6947){
  if (sinna[i,10]>Bastante5){
    sinna[i,21]="muchas Ventas globales"  }
  else if (sinna[i,10]>Intermedio5){
    sinna[i,21]="Bastantes Ventas globales"  }
  else if (sinna[i,10]>Pocas5){
    sinna[i,21]="Ventas globales intermedias"  }
  else if (sinna[i,10]>Muypocas5){
    sinna[i,21]="pocas Ventas globales"  }
  else if (sinna[i,10]>ventasnulas_o_sin_informacion){
    sinna[i,21]="muy pocas Ventas globales"  }
  else if (sinna[i,10]==ventasnulas_o_sin_informacion){
    sinna[i,21]="Ventas globales nulas o sin informacion"  }
}


# se crean los intervalos para las etiquetas de los puntajes en las cr?ticas de cr?ticos y usuarios y
#el n?mero de cr?ticas de cada uno de estos

#Puntajes cr?ticos
Muchas6 = 100
Bastante6 = 80
Intermedio6 = 60
Pocas6 = 40
Muypocas6 = 20
ventasnulas_o_sin_informacion4 = 0

#Puntajes usuarios
Muchas7 = 10
Bastante7 = 8
Intermedio7 = 6
Pocas7 = 4
Muypocas7 = 2
ventasnulas_o_sin_informacion7 = 0

#N? cr?ticas cr?ticos
Muchas8 = 22.6+22.6+22.6+22.6+22.6
Bastante8 = 22.6+22.6+22.6+22.6
Intermedio8 = 22.6+22.6+22.6
Pocas8 = 22.6+22.6
Muypocas8 = 22.6
ventasnulas_o_sin_informacion8 = 0


#N? cr?ticas usuarios
Muchas9 = 2133+2133+2133+2133+2133
Bastante9 = 2133+2133+2133+2133
Intermedio9 = 2133+2133+2133
Pocas9 = 2133+2133
Muypocas9 = 2133
ventasnulas_o_sin_informacion8 = 0

#se asignan las etiquetas
#se crea una columna extra para puntajes de cr?ticos

sinna["Puntajes Cr?ticos"] <- NA

for (i in 1:6947){
  if (sinna[i,11]>Bastante6){
    sinna[i,22]="Puntajes Cr?ticos muy alto"  }
  else if (sinna[i,11]>Intermedio6){
    sinna[i,22]="Puntajes Cr?ticos alto"  }
  else if (sinna[i,11]>Pocas6){
    sinna[i,22]="Puntajes Cr?ticos intermedio"  }
  else if (sinna[i,11]>Muypocas6){
    sinna[i,22]="Puntajes Cr?ticos bajo"  }
  else if (sinna[i,11]>ventasnulas_o_sin_informacion){
    sinna[i,22]="Puntajes Cr?ticos muy bajo"  }
  else if (sinna[i,11]==ventasnulas_o_sin_informacion){
    sinna[i,22]="Puntajes Cr?ticos nulas o sin informacion"  }
}





#se crea una columna extra para cantidad de cr?ticas de cr?ticos
sinna["n? criticas criticos"] <- NA

for (i in 1:6947){
  if (sinna[i,12]>Bastante8){
    sinna[i,23]="n? criticas criticos muy alto"  }
  else if (sinna[i,12]>Intermedio8){
    sinna[i,23]="n? criticas criticos alto"  }
  else if (sinna[i,12]>Pocas8){
    sinna[i,23]="n? criticas criticos intermedio"  }
  else if (sinna[i,12]>Muypocas8){
    sinna[i,23]="n? criticas criticos bajo"  }
  else if (sinna[i,12]>ventasnulas_o_sin_informacion){
    sinna[i,23]="n? criticas criticos muy bajo"  }
  else if (sinna[i,12]==ventasnulas_o_sin_informacion){
    sinna[i,23]="n? criticas criticos nulas o sin informacion"  }
}


#se crea una columna extra para puntajes de usuarios
sinna["Puntajes usuarios"] <- NA

for (i in 1:6947){
  if (sinna[i,13]>Bastante7){
    sinna[i,24]="Puntajes usuarios muy alto"  }
  else if (sinna[i,13]>Intermedio7){
    sinna[i,24]="Puntajes usuarios alto"  }
  else if (sinna[i,13]>Pocas7){
    sinna[i,24]="Puntajes usuarios intermedio"  }
  else if (sinna[i,13]>Muypocas7){
    sinna[i,24]="Puntajes usuarios bajo"  }
  else if (sinna[i,13]>ventasnulas_o_sin_informacion){
    sinna[i,24]="Puntajes usuarios muy bajo"  }
  else if (sinna[i,13]==ventasnulas_o_sin_informacion){
    sinna[i,24]="Puntajes usuarios nulas o sin informacion"  }
}


#se crea una columna extra para n?mero de cr?ticas de usuarios
sinna["n? cr?ticas usuarios"] <- NA

for (i in 1:6947){
  if (sinna[i,14]>Bastante9){
    sinna[i,25]="n? cr?ticas usuarios muy alto"  }
  else if (sinna[i,14]>Intermedio9){
    sinna[i,25]="n? cr?ticas usuarios alto"  }
  else if (sinna[i,14]>Pocas9){
    sinna[i,25]="n? cr?ticas usuarios intermedio"  }
  else if (sinna[i,14]>Muypocas9){
    sinna[i,25]="n? cr?ticas usuarios bajo"  }
  else if (sinna[i,14]>ventasnulas_o_sin_informacion){
    sinna[i,25]="n? cr?ticas usuarios muy bajo"  }
  else if (sinna[i,14]==ventasnulas_o_sin_informacion){
    sinna[i,25]="n? cr?ticas usuarios nulas o sin informacion"  }
}

#habiendo asignado todas las etiquetas, se eliminan las columnas num?ricas que
#contaminar?an la regla de asociaci?n
sinna["NA_Sales"] <- NULL
sinna["EU_Sales"] <- NULL
sinna["JP_Sales"] <- NULL
sinna["Other_Sales"] <- NULL
sinna["Global_Sales"] <- NULL
sinna["Critic_Score"] <- NULL
sinna["Critic_Count"] <- NULL
sinna["User_Score"] <- NULL
sinna["User_Count"] <- NULL


#se crea un dataset con todos los atributos nuevos con etiquetas
write.csv(sinna, file = "../data/conventas.csv")


#se crea otro dataset pero sin atributos que no aportan a las reglas de asociaci?n
#se considera que no aportan pues las reglas encontradas asocian esas las etiquetas de esos atributos
#sin aportar informaci?n porque o bien la asociaci?n es obvia (por ejemplo: muchas ventas en las distintas regiones del mundo
#implican muchas ventas a nivel global)
#el nuevo dataset tiene solo las ventas a nivel global, borrando las a nivel de regiones
# tambi?n se elimina el rating por las mismas razones
sinna["Rating"] <- NULL
sinna["Ventas JP"] <- NULL
sinna["Ventas NA"] <- NULL
sinna["Ventas EU"] <- NULL
sinna["Ventas otras"] <- NULL
write.csv(sinna, file = "../data/soloventasglobales.csv")


#se crea un dataset eliminando el atributo de ventas globales, dejando uno sin ventas
sinna["Ventas globales"] <- NULL

write.csv(sinna, file = "../data/sinventas.csv")


#se eliminan las columnas relacionadas con el g?nero, la plataforma y el a?o de lanzamiento del videojuego
sinna["Genre"] <- NULL
sinna["Platform"] <- NULL
sinna["Year_of_Release"] <- NULL

write.csv(sinna, file = "../data/singeneroplataformaa?o.csv")


# se eliminan el n?mero de cr?ticas
sinna["n? criticas criticos"] <- NULL
sinna["n? cr?ticas usuarios"] <- NULL
write.csv(sinna, file = "../data/sinnumerodecriticas.csv")
sinna2 <- sinna

#Luego, se prueba qu? pasa si se deja solamente el atributo del desarrollador o del publicador
sinna["Developer"] <- NULL
write.csv(sinna, file = "../data/sindesarrollador.csv")

sinna2["Publisher"] <- NULL
write.csv(sinna, file = "../data/sinpublicador.csv")

