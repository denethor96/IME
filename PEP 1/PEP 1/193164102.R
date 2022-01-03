# BRYAN PATRICIO SALAS VALENZUELA <- 19.316.410-2

# Importar un paquete, instalándolo de ser necesario.

if (!require(dplyr)) {
  install.packages("dplyr", dependencies=TRUE)
  require(dplyr)
}
if (!require(ggpubr)) {
  install.packages("ggpubr", dependencies=TRUE)
  require(ggpubr)
}

# Leer datos desde csv
datos <- read.csv2("C:/Users/Dene/Documents/2-2021/IME/PEP 1/PEP 1/Datos PEP 1.csv")

# Pregunta 1

# (14 puntos) Obi-Wan cree que, al ser Naboo un planeta gobernado por una reina y Tatooine otro bajo la tiranía
# de los Hutt, la proporción de mujeres es mayor entre los reclutas de Naboo que entre los de Tatooine. Para
# verificarlo, le ha solicitado usar dos muestras aleatorias de 40 reclutas (una para cada planeta, con la semilla 523)
# y un nivel de confianza de 95%

# se define la semilla
set.seed(523)

# Filtramos por planetas de interes
df.naboo = datos %>% filter(planeta == "Naboo")
df.tatooine = datos %>% filter(planeta == "Tatooine")

# obtenemos una muestra aleatoria de 40 reclutas para cada planeta
sp.naboo <- sample_n(df.naboo, 40)["sexo"]
sp.tatooine <- sample_n(df.tatooine$sexo, 40)["sexo"]


# Como queremos verificar si la proporción de mujeres es mayor entre los reclutas de Naboo que entre los de Tatooine.
# Haremos uso del método de WILSON para dos proporciones

# En ambos casos, las observaciones son independientes entre sí, pues provienen de personas diferentes que 
# representan a menos del 10 % de la población de cada planeta y Se cumple la condición de éxito-fracaso, que establece 
# que se espera observar al menos 10 observaciones correspondientes a éxito y al menos 10, y se asume que para ambos
# planetas la distribución de sexos de los reclutas sigue una distribución normal.

# Definimos el éxito como los reclutas que son de sexo = F y los fracasos como los recultas que son de sexo = M

# Definimos las hipótesis como:
# H0: No hay diferencia en la proporción de mujeres entre los reclutas de Naboo y Tatooine.
# HA: La proporción de mujeres es mayor entre los reclutas de Naboo que entre los de Tatooine.

# Valores conocidos (naboo, tatooine)
alfa <- 0.05
n <- c(40, 40)
exitos <- c(sum(sp.naboo == "F"), sum(sp.tatooine == "F")) # (naboo, tatooine)

# Prueba de Wilson
prueba <- prop.test(exitos, n = n ,alternative = "greater",
                    conf.level = 1 - alfa
                    )

print(prueba)
print(prueba$p.value)
# El valor p correspondiente es p = 0.3209, dado que el valor p es mayor que alfa = 0.05, se
# falla en rechazar la hipótesis nula. Así, podemos decir con 95 % de confianza que no existe evidencia suficiente
# para concluir que hay diferencia en la proporción de mujeres entre los reclutas de Naboo y Tatooine.













