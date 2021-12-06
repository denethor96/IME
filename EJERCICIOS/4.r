# Pregunta 3
# La avicultura de carne es un negocio muy lucrativo, y cualquier método que ayude al rápido crecimiento de
# los pollitos es beneficioso, tanto para las avícolas como para los consumidores. En el paquete datasets
# de R están los datos (chickwts) de un experimento hecho para medir la efectividad de varios
# suplementos alimenticios en la tasa de crecimiento de las aves. Pollitos recién nacidos se separaron
# aleatoriamente en 6 grupos, y a cada grupo se le dio un suplemento distinto. Para productores de la 6ta
# región, es especialmente importante saber si existe diferencia en la efectividad entre el suplemento
# basado en caseína (casein) y el basado en soya (soybean).
library(ggpubr)

data("chickwts")
summary(chickwts)
by(chickwts$weight, chickwts$feed, mean)

plot(chickwts$weight~chickwts$feed)

hist(chickwts$weight)
     
qqnorm(chickwts$weight)
qqline(chickwts$weight)
g0 <- ggqqplot(chickwts, x = "weight", color = "steelblue",
         xlab = "Teórico", ylab = "Muestra",
         title = "chickwts - Gráfico Q-Q muestra v/s distr. normal"
)
print(g0)


# Comprobamos la normalidad de los datos, usando la prueba de normalidad de Shapiro-Wilk
sha_chick<- shapiro.test(chickwts$weight)
print(sha_chick)

if(sha_chick$p > 0.05){
  print("chikens: Normal")
} else{
  print("chikens: No Normal")
}
# Los p valores son mayores que 0.05 por lo que los datos siguen una distribución normal



# Filtramos los datos segun el interes del estudio, en este caso casein y soybean
p_casein <- subset(chickwts, chickwts$feed == "casein")
p_soybean <- subset(chickwts, chickwts$feed == "soybean")

hist(p_casein$weight)
hist(p_soybean$weight)

qqnorm(p_casein$weight, xlab = "", ylab = "",
       main = "casein", col = "firebrick")
qqline(p_casein$weight)

qqnorm(p_soybean$weight, xlab = "", ylab = "",
       main = "soybean", col = "firebrick")
qqline(p_soybean$weight)

g1 <- ggqqplot(p_casein, x = "weight", color = "steelblue",
         xlab = "Teórico", ylab = "Muestra",
         title = "Casein - Gráfico Q-Q muestra v/s distr. normal"
        )

g2 <- ggqqplot(p_soybean, x = "weight", color = "steelblue",
         xlab = "Teórico", ylab = "Muestra",
         title = "Soybean - Gráfico Q-Q muestra v/s distr. normal"
)

print(g1)
print(g2)

# Comprobamos la normalidad de los datos, usando la prueba de normalidad de Shapiro-Wilk

sha_cas <- shapiro.test(p_casein$weight)
sha_soy <- shapiro.test(p_soybean$weight)
print(sha_cas)
print(sha_soy)

if(sha_cas$p > 0.05){
  print("Casein: Normal")
} else{
  print("Casein: No Normal")
}

if(sha_soy$p > 0.05){
  print("soybean: Normal")
} else{
  print("soybean: No Normal")
}
# Los p valores son mayores que 0.05 por lo que los datos siguen una distribución normal



# Aplicamos la prueba t para dos muestras independientes
# Hipotesis nula: no hay diferencia en las medias de los pesos de los pollitos al usar casein vs usar soybean
# Hipotesis alternativa: Si hay diferencia en las medias de los pesos de los pollitos
# Valor de significancia alfa = 0.01

alfa <- 0.01
testT <- t.test(x = p_casein$weight,
                y = p_soybean$weight,
                paired = FALSE,
                alternative = "two.sided",
                mu = 0,
                conf.level = 1 - alfa
               )

print(testT)

# Vemos que existen diferencias en las medias, por lo que se rechaza la hipotesis nula
# a favor de la hipotesis alternativa, es decir hay diferencia en las medias de los pesos de los pollitos 
# al usar Casein vs Soybean



diferencia <- mean(p_casein$weight) - mean(p_soybean$weight)
cat("La diferencia de medias es =", diferencia, "\n")






