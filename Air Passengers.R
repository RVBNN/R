
# LIBRERIAS ---------------------------------------------------------------
# Cargamos las librerías necesarias para operar con las series de tiempo:
#install.packages(c("tseries","tsoutliers","data.table","ggplot2","dplyr","tidyr", "xts"))
library("tseries")
library("tsoutliers")
library("data.table")
library("ggplot2")
library("dplyr") 
library("tidyr") 
library("xts")
library("lubridate")


# Cargamos los datos
AP_ts = AirPassengers


# MODELO ADITIVO ----------------------------------------------------------

AP_add = decompose(AP_ts,type = "additive")

# Temporalidad:
st_a = AP_add$seasonal

# Tendencia:
mt_a = AP_add$trend

# Aleatoriedad:
et_a = AP_add$random

# Implementación del modelo
Xt_a = st_a + mt_a + et_a

# MODELO MULTIPLICATIVO ---------------------------------------------------

# Descomponemos la serie de tiempo en un modelo mutliplicativo:
AP_multip = decompose(AP_ts,type = "multiplicative")

# Temporalidad:
st_m = AP_multip$seasonal

# Tendencia:
mt_m = AP_multip$trend

# Aleatoriedad:
et_m = AP_multip$random

# Implementación del modelo
Xt_m = (st_m * mt_m) + et_m





# PROMEDIOS MOVILES SIMPLES -----------------------------------------------
AP_ts

# Lo convierto en un objeto con el que sí sé trabajar: Matrices
AP <-matrix(data = AP_ts,ncol = 12)
colnames(AP) <- c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
rownames(AP) <- c(1949:1960)

class(AP1)

# PM(2)
AP1 <- matrix(AP)

v<-AP1[,1]

filter(data = v, rep(1/2,2), sides = 1)


v <- 1:20  
class(v)

v <- as.integer(v)

demanda <- c(120,90,110,90)
filter(demanda, rep(1/3,3), sides=1)


library(dplyr)

stats::filterfilter(x, rep(1 / n, n), sides = 2)


ma <- function(x, n = 5){filter(x, rep(1 / n, n), sides = 1)}
ma(demanda,3)

filter(x, rep(1 / 3, 3))




# Media Movil de orden 2
ma2<- stats::filter(AP_ts, rep(1 / 2, 2), sides = 1)

# Media Movil de orden 10
ma10 <- stats::filter(AP_ts, rep(1 / 10, 10), sides = 1)




plot(AP_ts, type='l', col='chartreuse3', xlab='Tiempo', ylab='Miles de Pasajeros', main = "Original con PS(2)")
lines(ma2, col='dodgerblue3')


plot(AP_ts, type='l', col='chartreuse3', xlab='Tiempo', ylab='Miles de Pasajeros', main = "Original con PS(10)")
lines(ma10, col='dodgerblue3')



# PROMEDIO MOVIL PONDERADO ------------------------------------------------





# Ponderadores
c_12 <-0.01282051282 * 1:12
sum(c_12)


AP[1,1:12]



AP1[1:12] %*% c_12
AP1[2,12] <- 2


s <- 0 * 1:nrow(AP)
for (i in 1:length(s)) {
  s[i + 11] <- AP[i:(11 + i)] %*% c_12
}

plot(s,type = 'l')

s[12] <- AP1[2:(11 + 2)] %*% c_12





s <- 0 * 1:nrow(AP1)
for (i in 1:length(s)) {
  s[i + 11] <- AP1[i:(11 + i)] %*% c_12
}
s
