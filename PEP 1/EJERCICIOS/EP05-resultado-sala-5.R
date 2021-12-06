#INTEGRANTES: Sofía Castro     -> 20.055.286-5
#             Sebastián Astete -> 18.562.196-0
#             Bastián Loyola   -> 20.552.001-5
#             Bryan Salas      -> 19.316.410-2

library(ggpubr)
library(ggplot2)
library(dplyr)

set.seed(100)
desv_estandar <-10
tam_muestra <- 50
media_nula<- 170


#PREGUNTA 1
#H0: La dureza media es igual a 170, µ0 = 174 ///
#H1: La dureza media es mayor a 170, µ0 > 174 ///
#El ingeniero recharzá la H0 si µ0 > 174

#Se nos pide encontrar bajo estas hipótesis el valor de alfa o Error Tipo I
#Se realizará una prueba de PODER ESTADÍSTICO
#Se crea una muestra de distribución normal con los datos iniciales.
media_alternativa <-174
poblacion1<- rnorm(n = 1000, mean = media_alternativa, sd = desv_estandar)
muestra1 <-sample(poblacion1,tam_muestra,replace=TRUE)


#Se calcula el valor de alpha utilizando la ecuación conocida luego se utiliza
#La tabla de distribución normal con sd = 1
z <- (media_alternativa - media_nula)/ desv_estandar
alpha<- 1 - pnorm(z, mean = 0, sd = 1, lower.tail = TRUE)
alpha

#Se crea el gráfico para demostrar el valor alfa
y1 <- dnorm(muestra1, mean = media_alternativa, sd = desv_estandar)
g1 <- ggplot(data = data.frame(muestra1, y1), aes(muestra1))
g1 <- g1 + stat_function(fun = dnorm ,args = list(mean = media_alternativa, sd = desv_estandar), 
                       colour = "blue", size = 1)
g1 <- g1 + ylab("")
g1 <- g1 + scale_y_continuous(breaks = NULL)
g1 <- g1 + theme_pubr()
g1 <- g1 + stat_function(fun =dnorm ,args = list(mean = media_nula, sd = desv_estandar),
                       colour = "green")
g1 <- g1 + stat_function(fun = dnorm ,args = list(mean = media_nula, sd = desv_estandar), 
                         geom="area", fill="green", xlim=c(174,205))
#El la parte pintada del gráfico representa alfa
print(g1)


#PREGUNTA 2
#Si µ0 = 174

#Se nos pide encontrar bajo estas hipótesis el valor de beta o Error Tipo II
#Se realizará una prueba de PODER ESTADÍSTICO
#Se crea una muestra de distribución normal con los datos iniciales.
set.seed(100)
media_alternativa <- 173
media_verdadera <- 173
poblacion1<- rnorm(n = 1000, mean = media_alternativa, sd = desv_estandar)
muestra1 <-sample(poblacion1,tam_muestra,replace=TRUE)

#Se calcula el valor de alpha utilizando la ecuación conocida luego se utiliza
#La tabla de distribución normal con sd = 1
z <- (media_alternativa - media_verdadera)/ desv_estandar
beta<- pnorm(z, mean = 0, sd = 1, lower.tail = TRUE) #///

poder <- 1 - beta
poder

#Se crea el gráfico para demostrar el valor del PODER
y1 <- dnorm(muestra1, mean = media_alternativa, sd = desv_estandar)
g2 <- ggplot(data = data.frame(muestra1, y1), aes(muestra1))
g2 <- g2 + stat_function(fun = dnorm ,args = list(mean = media_alternativa, sd = desv_estandar), 
                       colour = "blue", size = 1)
g2 <- g2 + ylab("")
g2 <- g2 + scale_y_continuous(breaks = NULL)
g2 <- g2 + theme_pubr()
g2 <- g2 + stat_function(fun =dnorm ,args = list(mean = media_nula, sd = desv_estandar),
                         colour = "green", size = 1)
g2 <- g2 +stat_function(fun = dnorm ,args = list(mean = media_alternativa, sd = desv_estandar), 
                        geom="area", fill="blue", xlim=c(146,173))

#La parte no pintada del gráfico representa beta
print(g2)

#PREGUNTA 3
#Si µ0 = {170:178} entconces generamos los gráficos de PODER ESTADISTICO
library(ggplot2)
efecto <- seq ( 170 , 178,0.2 )
poder <- power.t.test(tam_muestra,
                             delta =efecto,
                             sd=desv_estandar,
                            sig.level = 0.01,
                            type = "one.sample",
                            alternative = "two.sided")$power
print(poder)
valores <- data.frame(tam_muestra, poder)
g <- ggplot(tam_muestra ,aes(poder))
g <- g + geom_line()
g <- g+ theme_pubr()
g <- g + ylab("Poder estadístico")
g <- g + xlab("Tamaño de la muestra")
print(g)


# PREGUNTA 4
# ¿Cuántas barras deberían revisarse para conseguir un poder estadístico de 0,8 y un nivel de significación
# de 0,05? 

#Utilizando los datos previos, creamos un gráfico de poder estadístico 
#agregando los datos nuevos
muestra <- NULL
d <- 4
desv_estandar <- 10
alfa <- 0.05
poder <- 0.8

#Usando la funcion power.t.test, dejamos como NULL el parámetro "n"
#de modo que al aplicar la función se obtiene el valor de "n" que cumple
#con las condiciones dadas en el resto de los argumentos de la función
resultado <- power.t.test(n = muestra, 
                          delta=d, sd=desv_estandar, 
                          sig.level=alfa,
                          power=poder, 
                          "one.sample", 
                          "two.sided")

print(resultado$n)

#PREGUNTA 5
#¿Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error 
#de tipo 1 a un 1% solamente?

#Se aplica de manera similiar la función "power.t.test" con los nuevos valores
#solicitados, en este caso alfa = 0.01
muestra <- NULL
d <- 4
desv_estandar <- 10
alfa <- 0.01
poder <- 0.8

resultado <- power.t.test(n = muestra, 
                          delta=d, sd=desv_estandar, 
                          sig.level=alfa,
                          power=poder, 
                          "one.sample", 
                          "two.sided")
print(resultado$n)




