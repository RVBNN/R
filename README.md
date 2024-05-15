# SERIES DE TIEMPO

### Introducción
Para este proyecto, trabajaremos con la bases de datos de: **Air Passengers**.
El objetivo de este proyecto es el utilizar las herramientas vistas a lo largo del curso de *Modelos de Supervivencia y Series de Tiempo* para el análisis de series de tiempo.

Nos interesará conocer los componentes principales tales como:

* Tendencia
* Periodicidad
* Aleatoriedad

Y al mismo tiempo integraremos modelos de suavización para los datos.


#  PASAJEROS AÉREOS

La base Air Passengers hace referencia al número de personas (en miles) que viajan mensualmente en una cierta aerolínea internacional de pasajeros durante el periodo de enero de 1949 a diciembre de 1960. Esta base la podemos encontrar precargada en R por el nombre de *AirPassengers*. 

### Análisis Exploratorio

```{r}
# Cargamos los datos
AP_ts = AirPassengers
AP_ts
```

Una vez teniendo listos nuestros datos para trabajar, vamos a darnos una idea del comportamiento que podemos encontrar en la cantidad de pasajeros que se transportaron de **Enero de 1949 a Diciembre de 1960**

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r echo=TRUE, message=TRUE, include=FALSE}
#Librerias
#install.packages(c("tseries","tsoutliers","data.table","ggplot2","dplyr","tidyr", "xts"))
library("tseries")
library("tsoutliers")
library("data.table")
library("ggplot2")
library("dplyr") 
library("tidyr") 
library("xts")
```


```{r}
AP_xts = as.xts(AP_ts)
plot(AP_xts,
     xlab= 'Tiempo', 
     ylab = 'Miles de pasajeros',
     col = 'chartreuse3', 
     grid.ticks.on = 'quarters', #Crea líneas verticales sobre la gráfica cada trimestre
     major.ticks = 'quarters',  #Crea líneas verticales sobre la gráfica cada trimestre
     minor.ticks ='years', # Escribe la etiqueta de los meses cada cierto tiempo
     grid.col = 'lightgrey', #color líneas verticales
     main='Frecuencia de pasajeros de una aerolínea'
)
```

Con esto, podemos darnos una buena idea del comportamiento que van a seguir nuestros datos. De primer instancia, notamos que existe una tendencia alcista en la cantidad de pasajeros conforme pasa el tiempo y esto lo podemos corroborar con la función *aggregate()*

```{r}
AP.tendencia = aggregate(AP_ts)
plot(AP.tendencia, col='chartreuse3',
main='Tendencia de los pasajeros')
```



### Análisis de las Series de Tiempo

Abordaremos una serie de preguntas para generar insights acerca del comportamiento de los pasajeros a través del tiempo.


Notemos que por construcción una serie de tiempo cuenta con 3 componentes principales: Tendencia, Periodicidad y Aleatoriedad.

En nuestros modelos los representaremos con la siguiente notación:


* $X_t$: Valor de la serie de tiempo al momento t
* $m_t$: Tendencia en el t-ésimo momento
* $s_t$: Periodicidad (Estacionalidad)
* $\epsilon_t$: Componente Aleatorio

#### Modelo Aditivo
El modelo aditivo se representa por:

$$X_t = m_t+s_t+ \epsilon_t$$


Este modelo presenta una mayor variablidad en comparación del Modelo Multiplicativo, y al mismo tiempo cada uno de los factores tienen la misma relevancia al momento de hacer predicciones.

```{r}
# Descomponemos la serie de tiempo en un modelo aditivo:
AP_add = decompose(AP_ts,type = "additive")
```

```{r}
# MODELO ADITIVO: Componentes

# Temporalidad:
st_a = AP_add$seasonal

# Tendencia:
mt_a = AP_add$trend

# Aleatoriedad:
et_a = AP_add$random

# Implementación del modelo
Xt_a = mt_a + st_a + et_a
```

```{r, echo=FALSE}
AP_add_xts = as.xts(Xt_a)
plot(AP_add_xts,
     xlab= 'Tiempo', 
     ylab = 'Miles de pasajeros',
     col = 'darkorange2', 
     grid.ticks.on = 'quarters', #Crea líneas verticales sobre la gráfica cada trimestre
     major.ticks = 'quarters',  #Crea líneas verticales sobre la gráfica cada trimestre
     minor.ticks ='years', # Escribe la etiqueta de los meses cada cierto tiempo
     grid.col = 'lightgrey', #color líneas verticales
     main='Modelo Additivo'
)
```


#### Modelo Multiplicativo

El modelo multiplicativo se representa por:

$$X_t = (m_t \cdot s_t) + \epsilon_t$$
Este modelo cuenta con una menor variabilidad en los datos y tendrá un mayor peso el factor de tendencia y periodicidad 

```{r}
# Descomponemos la serie de tiempo en un modelo mutliplicativo:
AP_multip = decompose(AP_ts,type = "multiplicative")

```


```{r}
# MODELO MULTIPLICATIVO: Componentes

# Temporalidad:
st_m = AP_multip$seasonal

# Tendencia:
mt_m = AP_multip$trend

# Aleatoriedad:
et_m = AP_multip$random

# Implementación del modelo
Xt_m = (mt_m * st_m) + et_m

```
```{r, echo=FALSE}
AP_multip_xts = as.xts(Xt_m)
plot(AP_multip_xts,
     xlab= 'Tiempo', 
     ylab = 'Miles de pasajeros',
     col = 'dodgerblue4', 
     grid.ticks.on = 'quarters', #Crea líneas verticales sobre la gráfica cada trimestre
     major.ticks = 'quarters',  #Crea líneas verticales sobre la gráfica cada trimestre
     minor.ticks ='years', # Escribe la etiqueta de los meses cada cierto tiempo
     grid.col = 'lightgrey', #color líneas verticales
     main='Modelo Multiplicativo'
)
```


* **¿Que descomposición (aditiva/multiplicativa) consideras que ajusta mejor a cada serie?**

Para responder a la pregunta de qué modelo ajusta mejor a nuestros datos, contrastemos los dos modelos con el original


```{r, echo=FALSE}
#plot first line
plot(AP_ts, type='l', col='chartreuse3', xlab='Tiempo', ylab='Miles de Pasajeros', main = "Original vs Aditivo")

#add second line to plot
lines(Xt_a, col='darkorange2')
```

```{r, echo=FALSE}
#plot first line
plot(AP_ts, type='l', col='chartreuse3', xlab='Tiempo', ylab='Miles de Pasajeros', main = "Original vs Multiplicativo")

#add second line to plot
lines(Xt_m, col='dodgerblue4')
```

De estas graficas podemos concluir que el modelo que mejor ajusta a los datos observados está dado por la **descomposición aditiva**. En ella encontramos que la tendencia y periodicidad se mantienen a lo largo del tiempo, además de que preserva un comportamiento de homocedasticidad[^1]

[^1]: Del griego ***Homos*** (igual) y ***cedastitis*** (dispersión).
Hipotesis referente a la dispersión de los valores de una perturbación aleatoria en un modelo de regresión lineal, que consiste en suponer que **la variable se distribuye con igual varianza** en cualquiera de las estimaciones hechas mediante el modelo. Fuente: [wiktionary.org](https://es.wiktionary.org/wiki/homocedasticidad#:~:text=Del%20griego%20Homos%20(igual)%20y,estimaciones%20hechas%20mediante%20el%20modelo.)


* **Realice con ayuda de R la descomposición de la serie identificando los tres componentes
de manera gráfica**

```{r}
# AP_ts = AirPassengers
AP_add = decompose(AP_ts, type = "additive")
plot(AP_add)
```

* **¿Qué puedes concluir sobre la tendencia, periodicidad y aleatoriedad de la serie?**

 **Tendencia**: Notamos que existe un comportamiento creciente en el número de pasajeros en la Aerolinea a lo largo de los 10 años del estudio.

 **Periodicidad**: Aquí encontramos un comportamiento que se repite año con año, lo que nos lleva a concluir que existe una estacionareidad en el flujo de pasajeros porcada trimestre. Con lo que aunado al punto anterior, desde el punto de vista del negocio vamos a tener un flujo de pasajeros creciente, que sigue un mismo comportamiento al momento de realizar sus vuelos.<br /> <br />
Es decir: Se eligen las mismas fechas para volar, con la consideración de que año con año tendremos más clientes, por lo que habría que considerar un posible incremento de unidades de transporte para poder satisfacer la demanda de los clientes.


 **Aleatoriedad**: En este último aspecto pero no menos relevante encontramos que los primeros 4 años de la serie de tiempo se repitió casi el mismo comportamiento aleatorio en los datos. <br />
<br />
Sin embargo un periodo interesante es el que abarca de 1954 a 1957, pues existió una estabilidad en el flujo de pasajeros.

* **¿Consideras que las series son estacionarias?**

Una primer consideración que tendremos es en el uso del termino variación estacional, que es con el que explicaremos y responderemos la pregunta formulada.

Entenderemos en terminos simples a la **variación estacional** de una serie de tiempo como: ***información que sigue un patrón en cada periodo de tiempo de referencia***

Analicemos el comportamiento de los pasajeros:

```{r}
boxplot(
  AP_ts ~ cycle(AP_ts),
  col = "orange",
  # Color de relleno
  border = "brown",
  #Color de contorno
  main = 'Flujo de pasajeros',
  xlab = 'Tiempo',
  ylab = 'Cantidad de pasajeros (10^3)',
  names = month.abb # Agregar nombres
)
```

Aquí podemos encontrar que existe poca variación estacional para los meses de Enero a Junio, por lo que para el primer semestre del mes podriamos concluir que existe una estacionariedad gracias a la poca variación estacional que experimenta el flujo de pasajeros.

Con un razonamiento similar podemos inquirir que en los meses correspondientes al Verano (Junio a Septiembre) tenemos una estacionariedad parcial.

En un análisis más global, considerando todo el año vemos que el flujo de pasajeros se mantiene en unas bandas de 100 a 200 mil pasajeros con una media cercana a 250 mil pasajeros. 


* **Con los tres componentes de la serie realice las operaciones necesarias, dependiendo del
modelo elegido, para volver a obtener $X_t$.**


Apoyandonos en el análisis de incisos pasados llegamos a la conclusión de que el mejor ajuste se encuentra en la descomposición aditiva.

```{r}
# MODELO ADITIVO: Componentes
AP_add = decompose(AP_ts, type = "additive")

# Temporalidad:
st_a = AP_add$seasonal

# Tendencia:
mt_a = AP_add$trend

# Aleatoriedad:
et_a = AP_add$random

# Implementación del modelo
Xt_a = mt_a + st_a + et_a

# Mostramos los valores que recreamos para la serie de tiempo
Xt_a

# Valores reales observados
AP_ts
```

* **Calcula sus autocorrelaciones, ¿Puedes asumir para cada serie que los datos se encuentran autocorrelacionados? ¿Por qué crees que el autocorrelograma sigue ese patrón?**

```{r}
acf(AP_ts)
```




### Suavización de la Serie de Tiempo

#### Promedios Moviles Simples

**Realiza una suavización de promedios móviles simple (PS) de orden 2 y 10**

Recordemos la definición de promedio móvil de orden (k)

$$\hat{X}_t = \frac{\sum_{i=1}^k X_{t-i}}{k}$$


```{r}
# Media Movil de orden 2
ma2 <- stats::filter(AP_ts, rep(1 / 2, 2), sides = 1)

# Media Movil de orden 10
ma10 <- stats::filter(AP_ts, rep(1 / 10, 10), sides = 1)
```

```{r, echo=FALSE}
plot(AP_ts, type='l', col='chartreuse3', xlab='Tiempo', ylab='Miles de Pasajeros', main = "Original con PS(2)")
lines(ma2, col='dodgerblue3')


plot(AP_ts, type='l', col='chartreuse3', xlab='Tiempo', ylab='Miles de Pasajeros', main = "Original con PS(10)")
lines(ma10, col='dodgerblue3')
```

Con estos Promedios Moviles podemos identificar el comportamiento medio de la serie de tiempo, mientras mayor sea el parámetro de la media móvil, la tendencia será menos influenciada por los cambios en los valores de la serie.


* **Suavización de promedios móviles ponderados (PPM) de orden 4 con los pesos
$(0.5, 0.3, 0.2, 0.1)$ para $(X_{t}, X_{t−1}, X_{t−2}, X_{t−3})$ y otra modelo (PPM) de orden 12
seleccionando ustedes mismos los pesos y mencionado el porque de esos pesos.**


Los promedios móviles ponderados sonuna variación de los promedios móviles simples. En estos los pesos de cada dato dependen de su temporalidad respecto al tiempo. En este modelo contamos con ponderaciones que denotaremos por $C_i$ tales que satisfacen:

$$\sum_{i=1}^k C_i = 1$$


Y el modelo se expresa por:

$$\hat{X_t} = \sum_{i=1}^k C_i \cdot X_{t-1}$$


Para la media movil ponderada de orden 12 le daremos mayor importancia a los últimios valores. Para asignarle valores a los ponderadores utilizaremos el siguiente proceso.

Fijaremos el ponderador más alejado de nuestra serie de tiempo $W_{t-12}$. Queremos una serie de ponderadores tales que le den más importancia a los ultimos valores de la serie de tiempo. Por lo que queremos unos pesos crecientes, entonces para cada tiempo tendremos: $t\cdot W_{t-12}$, $\forall t \in {1,...,12}$

Luego como queremos que estos ponderadores sumen 1:

$$\sum_{t=1}^{12}t\cdot W_{t-12} = 1\\
\iff W_{t-12} = \frac{1}{\sum_{t=1}^{12}t}\\
\iff W_{t-12} = \frac{2}{12\cdot 13} = 0.01282051282$$


```{r}
# Ponderadores
c_2 <- c(0.5, 0.3, 0.2, 0.1)

c_12 <-0.01282051282 * 1:12

```

```{r}
# Lo convierto en un objeto con el que sí sé trabajar: Matrices
AP <-matrix(data = AP_ts,ncol = 12)
colnames(AP) <- c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
rownames(AP) <- c(1949:1960)

# Matriz para los promedios móviles
AP1 <- matrix(AP)
```


```{r}
# Media Móvil Ponderada(2)

ma_4 <- 0 * 1:nrow(AP1)


for (i in 1:length(ma_4)) {
  ma_4[i + 4] <- AP1[i:(3 + i)] %*% c_2
}

# Quitamos los valores NA
ma_4 <- ma_4[!is.na(ma_4)]
plot(ma_4,type = 'l', ylab= "Media Movil(4)", col = "dodgerblue3")

ma_4

```



```{r}
# Media Móvil Ponderada(12)

ma_12 <- 0 * 1:nrow(AP1)


for (i in 1:length(ma_12)) {
  ma_12[i + 12] <- AP1[i:(11 + i)] %*% c_12
}

# Quitamos los valores NA
ma_12 <- ma_12[!is.na(ma_12)]
plot(ma_12,type = 'l', ylab= "Media Movil(12)", col = "dodgerblue3")

ma_12

```

* **Suavización Exponencial Simple con $\alpha = 0.01, \alpha=0.05, \alpha = 0.99$ ¿Cuál es el papel de $\alpha$ en la estimación de valores pronóstico**

Este método se suavización es ideal cuando no conocemos o no hay tendencia ni periodicidad en nuestra serie de tiempo. su principal función es generar pronósticos y se define como:

$$\hat{X}_{t+1} = \alpha X_t +  (1-\alpha)  \hat{X}_t$$

De este resultado tenemos la siguiente observación:

$$
\mbox{Pronóstico} =
\left\{
	\begin{array}{ll}
		\alpha \to 1  & \mbox{Pronóstico } \approx \mbox{Valores Actuales} \\
		\alpha \to 0  & \mbox{Pronóstico } \approx \mbox{Valores Anteriores Estimados}
	\end{array}
\right.
$$


```{r}
# Para alpha = 0.01
library("forecast")
datos <- AP1
modelo = ses(datos, alpha = 0.01, initial = 'simple')
a1 <- modelo$fitted
```
```{r, echo=FALSE}
a1_xts = as.xts(a1)
plot(a1_xts,
     xlab= 'Tiempo', 
     ylab = 'Miles de pasajeros',
     col = 'darkorange2', 
     grid.ticks.on = 'quarters', #Crea líneas verticales sobre la gráfica cada trimestre
     major.ticks = 'quarters',  #Crea líneas verticales sobre la gráfica cada trimestre
     minor.ticks ='years', # Escribe la etiqueta de los meses cada cierto tiempo
     grid.col = 'lightgrey', #color líneas verticales
     main='Suavizado Exponencial alpha = 0.01'
)
```
```{r}
# Para alpha = 0.5
library("forecast")
datos <- AP1
modelo = ses(datos, alpha = 0.5, initial = 'simple')
a2 <- modelo$fitted
```
```{r, echo=FALSE}
a2_xts = as.xts(a2)
plot(a2_xts,
     xlab= 'Tiempo', 
     ylab = 'Miles de pasajeros',
     col = 'darkorange2', 
     grid.ticks.on = 'quarters', #Crea líneas verticales sobre la gráfica cada trimestre
     major.ticks = 'quarters',  #Crea líneas verticales sobre la gráfica cada trimestre
     minor.ticks ='years', # Escribe la etiqueta de los meses cada cierto tiempo
     grid.col = 'lightgrey', #color líneas verticales
     main='Suavizado Exponencial alpha = 0.5'
)
```
```{r}
# Para alpha = 0.99
library("forecast")
datos <- AP1
modelo = ses(datos, alpha = 0.99, initial = 'simple')
a3 <- modelo$fitted
```

```{r, echo=FALSE}
a3_xts = as.xts(a3)
plot(a3_xts,
     xlab= 'Tiempo', 
     ylab = 'Miles de pasajeros',
     col = 'darkorange2', 
     grid.ticks.on = 'quarters', #Crea líneas verticales sobre la gráfica cada trimestre
     major.ticks = 'quarters',  #Crea líneas verticales sobre la gráfica cada trimestre
     minor.ticks ='years', # Escribe la etiqueta de los meses cada cierto tiempo
     grid.col = 'lightgrey', #color líneas verticales
     main='Suavizado Exponencial alpha = 0.99'
)
```

* **Suavización Holt , con $\alpha = 0.5, \beta = 0.2, \beta = 0.8$ ¿Cuál es el papel de β en la
estimación de valores pronostico?**
```{r}
library("forecast")
h1 <- holt(AP_ts, alpha = 0.5,beta = 0.2)
h1
```


El papel de $\beta$ en estos modelos representa la tendencia de la serie de tiempo
Sin embargo el modelo no nos permite usar un parámetro de beta mayor al 0.5

* **Suavización Holt-Winters , con $\alpha = 0.5, \beta = 0.2, \gamma = 0.8$ y otro modelo Holt-Winters
con una selección automática de R.**

```{r}
library("forecast")
hw <- HoltWinters(AP_ts, alpha = 0.5,beta = 0.2,gamma = 0.8)
hw
```
```{r}
# Segundo modelo de Holt-Winters con valores aleatorios para sus parámetros
library("forecast")
parametros <- runif(3)
a <- parametros[1]
b <- parametros[2]
g <- parametros[3]

hw2 <- HoltWinters(AP_ts, alpha = a,beta = b,gamma = g)
hw2
```


#### Suavización por promedios móviles simples centrados

Elabora una suavización de promedios móviles simple centrados de acuerdo a la periodicidad de la serie de tiempo; es decir, si los datos tienen una periodicidad mensual selecciona orden 12.

Este método de suavizamiento disminuye la pérdida de información correspondiente al orden. Y sigue un mismo estilo que el de promedios móviles simples, pues sólo cambia un parámetro dentro de la función ***filter***  con este método no podemos realizar pronósticos

```{r}
frec <- frequency(AP_ts)
frec

cma <- stats::filter(AP_ts, rep(1 / 12, 12), sides = 2)

cma


plot(cma,type = 'l', ylab= "Media Movil Centrada(12)", col = "dodgerblue3")

```


* **De la suavización anterior ahora verifica la tendencia obtenida en la descomposición del
primer punto, ¿Se parecen? ¿En que modelo se puede asumir la anterior descomposición
puede ser usado como estimador de la tendencia?**

```{r}
# Serie de tiempo original
plot(AP_ts, type='l', col='chartreuse3', xlab='Tiempo', ylab='Miles de Pasajeros', main = "Original vs Aditivo")

# Descomposición Aditiva
lines(Xt_a, col='darkorange2')

# Tendencia mostrada por Media Movil Centrada (12)
lines(cma, col='dodgerblue3')

# Tendencia mostrada por descomposición aditiva
lines(mt_a, col='deeppink3')

```

Notemos que las tendencias mostradas tienen un gran parecido en sus valores pues la gráfica rosa se empalma con la azul. Con esto tenemos una conclusión importante y es que podemos asumir la tendencia de la descomposición aditiva como un buen estimador del comportamiento de la serie de tiempo.
