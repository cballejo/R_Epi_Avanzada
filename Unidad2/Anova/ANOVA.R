## ANOVA

## La concentraci�n plasm�tica elevada de lipoprote�nas de alta densidad (HDL) se acompa�a de 
## un menor riesgo de padecer cardiopat�a coronaria. 

## Varios estudios sugieren que el ejercicio vigoroso eleva la concentraci�n de HDL. 
## Con el fin de investigar si el trote incrementa la concentraci�n plasm�tica de HDL, 
## G. Harley Hartung et al. cuantificaron la concentraci�n de HDL en corredores de marat�n, 
## trotadores y varones sedentarios (35 a 66 a�os de edad). 

## La concentraci�n promedio de HDL en estos �ltimos fue de 43,3 mg/100 ml con una desviaci�n 
## estandar de 14,2 mg/100 ml. 

## La media y desv�o estandar de la concentraci�n de HDL en los trotadores y maratonistas 
## fueron de 58,0 y 17,7 mg/100 ml y de 64,8 y 14,3 mg/100 ml, respectivamente. 

## Si cada grupo constaba de70 varones, compruebe la hip�tesis que sostiene que no existen 
## diferencias en la concentraci�n promedio de HDL en los diversos grupos.

HDL <- read.csv2("Anova-HDL.csv",as.is=T) # leemos la tabla de datos

## Testeamos normalidad

# como tenemos m�s de 50 observaciones, lo hacemos con el test de Kolmogorov-Smirnov (con la 
# correcci�n de Lilliefors)

require(nortest)  # debemos instalar previamente el paquete nortest
lillie.test(HDL$hdl[HDL$grupo=="Sedentario"]) # en cada caso filtramos por una categoria de grupo
lillie.test(HDL$hdl[HDL$grupo=="Trotador"])
lillie.test(HDL$hdl[HDL$grupo=="Maratonista"])

# En los tres grupos los test de hip�tesis no muestran evidencias de falta de normalidad.

###  Gr�fico Cuantil-cuantil

qqnorm(HDL$hdl[HDL$grupo=="Sedentario"], pch= 16, 
       col="red", main="QQ plot HDL grupo Sedentario")
qqline(HDL$hdl[HDL$grupo=="Sedentario"])

qqnorm(HDL$hdl[HDL$grupo=="Trotador"], pch= 16, 
       col="red", main="QQ plot HDL grupo Trotador")
qqline(HDL$hdl[HDL$grupo=="Trotador"])

qqnorm(HDL$hdl[HDL$grupo=="Maratonista"], pch= 16, 
       col="red", main="QQ plot HDL grupo Maratonista")
qqline(HDL$hdl[HDL$grupo=="Maratonista"])

# Las gr�ficas muestran que los puntos se distribuyen a lo largo de la l�nea, 
# por lo que junto al resultado del test anal�tico podemos asumir la normalidad 
# de la distribuci�n.

### Pobamos varianza constante entre grupos (homocedasticidad)

bartlett.test(hdl ~ grupo, data=HDL)

require(car)   
leveneTest(hdl ~ factor(grupo), data=HDL)

# No hay evidencias significativas de falta de homocedasticidad en ninguno de los dos test.

## An�lisis de varianza ANOVA

resultado <- aov(formula = hdl ~ grupo, data = HDL)
summary(resultado)

# Dado que el p-value es inferior a 0.05 hay evidencias suficientes para considerar que al 
# menos dos medias son distintas.

### Comparaciones m�ltiples

TukeyHSD(resultado)

plot(TukeyHSD(resultado))

# Hay mayor diferencia entre los grupos Sedentario-Maratonista y Trotador-Sedentario que 
# en Trotador-Maratonista, aunque todos tiene valor-p ajustado significativo.


## Conclusi�n

## En el estudio realizado con la t�cnica de inferencia ANOVA se ha encontrado significancia 
## estad�stica para rechazar que las medias son iguales entre todos los grupos.
