# Pregunta 1
# Un estudio, encargado por bomberos, investigó si los sistemas aspersores de prevención, que deben ser
# instalados en los edificios de más de 4 pisos construidos después de 2001, cumplen con la norma que les
# obliga a que el tiempo promedio de activación no sobrepase los 25 s. Con una serie de pruebas obtuvieron
# la siguiente muestra: 

data <- c(27, 41, 22, 27, 23, 35, 30, 33, 24, 27, 28, 23, 24)
n <- length(data)
gl <- n-1
v0 = 25

# El estudio concluyó que la norma no se estaba cumpliendo. ¿Sugieren los datos esta conclusión?

boxplot(data)

hist(data)

g0 <- ggqqplot(data, x = NULL, color = "steelblue",
               xlab = "Teórico", ylab = "Muestra",
               title = "chickwts - Gráfico Q-Q muestra v/s distr. normal"
)
print(g0)

sha_test <- shapiro.test(data)
print(sha_test)
# El p valor es mayor a 0.05, por lo que los datos siguen una distribución normal


alfa <- 0.01
media <- mean(data)
dst <- sd(data)
error <- dst / sqrt(n)
t_prueba <- (media - v0) / error
cat ("T de prueba =", t_prueba, "\n")

p <- pt(t_prueba, df = gl, lower.tail = TRUE)
cat ("p =", p , "\n")

t_critico <- qt(alfa, df = gl, lower.tail = FALSE)
# superior <- media + t_critico * error
# cat ("Intervalo de confianza = ( -Inf , ", superior , "]\n", sep = "")

# Aplicar la prueba t de Student con la función de R.
test <- t.test(data, alternative = "less", mu = v0, conf.level = 1-alfa)
print(test)

# La hipotesis 

# p = 0.9627483
# 
# t_prueba = 1.953258
# 
# 
# t_tabla = -2.680998 | 2.680998


