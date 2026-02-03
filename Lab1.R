#Laboratorio 1
#Sofia Lopez

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






