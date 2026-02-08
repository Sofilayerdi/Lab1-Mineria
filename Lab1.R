#Laboratorio 1
#Sofia Lopez

library(tidyverse)
library(dplyr)



movies <- read.csv("movies_2026.csv")


#Ejercicio 1
summary(movies)
str(movies)
dim(movies)

table(movies$originalLanguage)
table(movies$video)
table(movies$releaseYear)

#Ejercicio 2
tablaTipos <- data.frame(
  Variable = names(movies),
  Tipo = sapply(movies, function(x) {
    if (is.numeric(x)) {
      "Cuantitativa"
    } else {
      "Cualitativa"
    }
  })
)

tablaTipos
tablaTipos <- edit(tablaTipos)
tablaTipos

#Ejercicio 3
hist(movies$budget[!is.na(movies$budget)],
     main = "Presupuesto",
     xlab = "Presupuesto",
     breaks = 30)

hist(movies$revenue[!is.na(movies$revenue)],
     main = "Ingresos",
     xlab = "Ingresos",
     breaks = 30)

hist(movies$runtime[!is.na(movies$runtime)],
     main = "Duracion",
     xlab = "Duracion",
     breaks = 30)

hist(movies$popularity[!is.na(movies$popularity)],
     main = "Popularidad",
     xlab = "Popularidad",
     breaks = 30)

hist(movies$voteAvg[!is.na(movies$voteAvg)],
     main = "Promedio de votos",
     xlab = "Promedio de votos",
     breaks = 30)

hist(movies$voteCount[!is.na(movies$voteCount)],
     main = "Conteo de votos",
     xlab = "Conteo de votos",
     breaks = 30)

hist(movies$genresAmount[!is.na(movies$genresAmount)],
     main = "Cantidad de generos",
     xlab = "Cantidad de generos",
     breaks = 30)

hist(movies$productionCoAmount[!is.na(movies$productionCoAmount)],
     main = "Cantidad de compañias",
     xlab = "Cantidad de compañias",
     breaks = 30)

hist(movies$productionCountriesAmount[!is.na(movies$productionCountriesAmount)],
     main = "Cantidad de paises",
     xlab = "Cantidad de paises",
     breaks = 30)

hist(movies$actorsAmount[!is.na(movies$actorsAmount)],
     main = "Cantidad de actores",
     xlab = "Cantidad de actores",
     breaks = 30)

hist(movies$castWomenAmount,
     main = "Cantidad de mujeres",
     xlab = "Cantidad de mujeres",
     breaks = 30)

hist(movies$castMenAmount,
     main = "Cantidad de hombres",
     xlab = "Cantidad de hombres",
     breaks = 30)


generos_sep <- strsplit(as.character(movies$genres), "\\|")
generos_individuales <- unlist(generos_sep)
freq_generos <- table(generos_individuales)
tabla_generos <- data.frame(
  Genero = names(freq_generos),
  Frecuencia = as.vector(freq_generos)
)

tabla_generos

table(movies$productionCompany)
table(movies$productionCompanyCountry)
table(movies$video)
table(movies$director)

actores_sep <- strsplit(as.character(movies$actors), "\\|")
actores_individuales <- unlist(actores_sep)
freq_actores <- table(actores_individuales)
tabla_actores <- data.frame(
  Actor = names(freq_actores),
  Frecuencia = as.vector(freq_actores)
)

tabla_actores


table(movies$actorsCharacter)
table(movies$title)
table(movies$originalLanguage)
table(movies$releaseDate)


#Ejercicio 4

#1
head(movies[order(movies$budget, decreasing = TRUE), c("title", "budget")],10)


#2
head(movies[order(movies$revenue, decreasing = TRUE), c("title", "revenue")],10)

#3
head(movies[order(movies$voteCount, decreasing = TRUE), c("title", "voteCount")],1)

#4
head(movies[order(movies$voteCount, decreasing = FALSE), c("title", "voteCount")],1)

#5
tabYear <- table(movies$releaseYear)
tabYear

barplot(tabYear,                    
        main="Peliculas por año",        
        xlab="Años de publicación", 
        ylab="Número de películas"
) 

#Año con mayor publicaciones?
head(tabYear[order(tabYear, decreasing = TRUE)], 1)

#6

#genero principal de las peliculas mas recientes
allGenres <- unlist(strsplit(movies$genres, "\\|"))
top20 <- movies[order(movies$releaseYear, decreasing = TRUE), ][1:20, ]

top20genres <- unlist(strsplit(top20$genres, "\\|"))

tableRecent <- table(top20genres)

barplot(
  tableRecent,
  main = "Géneros de las 20 películas más recientes",
  xlab = "Género",
  ylab = "Frecuencia",
  col = "lightblue",
  las = 2
)

#genero que predomina
genre_all <- table(allGenres)

barplot(
  genre_all,
  main = "Distribución de géneros en todo el dataset",
  xlab = "Género",
  ylab = "Frecuencia",
  col = "lightgreen",
)

names(which.max(genre_all))


#genero de las peliculas mas largas
longMovies <- movies[order(movies$runtime, decreasing = TRUE), ][1:20, ]

genresLong <- unlist(strsplit(longMovies$genres, "\\|"))

tableLong <- table(genresLong)
tableLong

barplot(
  tableLong,
  main = "Generos de las peliculas mas largas",
  xlab = "Genero",
  ylab = "Frecuencia",
  col = "pink"
)

#7
movies$profit <- movies$revenue - movies$budget

movies_clean <- movies[!is.na(movies$profit) & !is.na(movies$genres), ]

genresRevenue <- strsplit(movies_clean$genres, "\\|")

genres_x <- data.frame(
  genre = unlist(genresRevenue),
  profit = rep(movies_clean$profit, times = sapply(genresRevenue, length))
)

profit_by_genre <- aggregate(profit ~ genre, data = genres_x, sum)


profit_by_genre <- profit_by_genre[
  order(profit_by_genre$profit, decreasing = TRUE),
]

profit_by_genre[1, ]
