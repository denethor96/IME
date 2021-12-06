# Pregunta 1
# Se realizó un ensayo clínico para estudiar la toxina botulínica, una toxina muy potente que puede ser
# usado como medicamento en dosis diminutas, como posible tratamiento para el dolor de espalda crónico.
# Un total de 31 pacientes participaron del estudio, de los cuales 15 fueron asignados aleatoriamente al
# grupo de tratamiento y los otros 16 al grupo de control (placebo). Luego de ocho semanas, 9 personas
# voluntarias del grupo de tratamiento reportaron alivio del dolor, mientras que 5 personas lo hicieron en el
# grupo de control. ¿Qué se podría decir del tratamiento?




# Pregunta 2
# La escuela de psicología quiere evaluar el impacto de una intervención que han diseñado para ayudar a
# dejar de fumar. Con este fin, reclutaron 25 personas fumadoras que tenían la intención de dejar de hacerlo
# y 25 personas fumadoras que no consideraban esta opción. Dos semanas después de finalizada la
# intervención (que consistía en mirar videos emotivos que mostraban el impacto que las muertes por cáncer
# asociado al cigarrillo tenía en las familias, seguidas de rondas de conversación), se les preguntó a las y los
# participantes si tenían intención de intentar dejar de fumar. 2 personas que tenían la intención de hacerlo
# antes de la intervención, cambiaron de opinión y ya no quieren intentarlo, mientras que 11 participantes
# que no pensaban en dejar de fumar, ahora lo estaban considerando. ¿Qué se puede decir del impacto de
# la intervención?

# Las hipótesis asociadas a la prueba de mcNemar son:
# H0: no hay cambios significativos en las respuestas.
# HA: sí hay cambios significativos en las respuestas.

# Construir la tabla de contingencia .
fumadores <- seq (1:25)
# fumadoras que tenían la intención de dejar de hacerlo antes de la intervención
grupo_1 <- c(rep("dejar de fumar", 23), rep("seguir fumando", 2))
# fumadores que NO consideraban dejar de hacerlo
grupo_2 <- c(rep("dejar de fumar", 11), rep("seguir fumando", 14))

datos <- data.frame (fumadores, grupo_2, grupo_1)
tabla <- table(grupo_2, grupo_1)
print (tabla)

# Aplicar prueba de McNemar
test_mcnemar <- mcnemar.test (tabla)
print(test_mcnemar)
print(test_mcnemar$p.value)

# Como p = 0.001496164 < α = 0.05, se rechaza la hipotesis nula a favor de la hipotesis alternativa,
# por lo que sí hay cambios significatos en las respuestas después de la intervención



# Pregunta 3
# Un grupo de activistas ha denunciado racismo en la conformación de los jurados de un pequeño condado
# en Texas, EE.UU. Su denuncia se basa que, según ellos, las proporciones raciales de las personas
# seleccionadas para ser jurado el año pasado (208 blancos, 28 negros, 20 latinos y 19 de otras razas) no
# se corresponde con las proporciones reportadas en el último censo (72% adultos blancos, 7% adultos
# negros, 12% adultos latinos y 9% adultos que se declaran de otras raza). ¿Tienen razón los denunciantes?

# H0: las proporciones de persona para cada raza son las mismas para el jurado y el censo
# HA: las proporciones de persona para cada raza son diferentes para el jurado y el censo

pob <- 100000 # Número arbitrario

# Crear tabla de contingencia
censo <- c(0.72*pob, 0.07*pob, 0.12*pob, 0.09*pob)
jurado <- c(208, 28, 20, 19)

tabla_3 <- as.table(rbind(censo, jurado))
dimnames(tabla_3) <- list(grupo = c("Censo", "jurado"), raciales = c("blancos", " negros", "latinos ", "Otras"))

print(tabla_3)

# Verificar si se esperan más de 5 observaciones por cada grupo .
n_censo <- sum(censo)
n_jurado <- sum(jurado)

proporciones <- round(censo/n_censo, 3)
esperados <- round(proporciones * n_jurado, 3)
print(esperados)

# Hacer prueba chi - cuadrado de homogeneidad .
test_chisq <- chisq.test(tabla_3, correct = FALSE)
print(test_chisq)
print(test_chisq$p.value)

# Como p = 0.01221836 < α = 0.05, se rechaza la hipotesis nula a favor de la hipotesis alternativa,
# si existe diferencia entre las proporciones raciales de las personas seleccionadas para ser jurado 
# y las reportadas por el último censo



# Pregunta 4
# Enuncie un ejemplo novedoso (no discutido en clase ni en las lecturas) relacionado con las expectativas
# de los chilenos de ir al mundial de Catar, a la luz de los resultados de la selección nacional de fútbol en la
# jornada clasificatoria triple de octubre 2021, que requiera utilizar una prueba Q de Cochran. Identifique las
# variables involucradas y las hipótesis a contrastar