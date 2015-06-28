#ESTUDIO DE GRAFICOS Y VALORES CON MEDIA CENTRADA 
#data <-read.table("data_rls_uti.txt", header=TRUE, dec=",", sep="\t")
#str(data)
#View(data)

install.packages("readxl", dependencies=TRUE)
library(readxl)
ls("package:readxl")

data <- read_excel("data_rls_uti.xlsx", sheet=1,na="")#na:sign la representacion de los datos perdidos
str(data)
View(data)

reg <- lm(Utilidad~Ventas, data)
str(reg)
summary(reg) 
anova <- aov(reg)
summary(anova)
qt(0.975,df=38)
qf(0.95,df=1,df2=38)

#PROCESO REALIZADO CON DATOS CENTRADOS


med1 <- mean(data[,"Utilidad"])
med1
med2 <- mean(data[,"Ventas"])
med2

utilidad1 <- data[,"Utilidad"]-med1
utilidad1
ventas1 <- data[,"Ventas"]-med2
ventas1

regresion1 <- lm(utilidad1~ventas1, data)
str(regresion1)
summary(regresion1) 
anova1 <- aov(regresion1)
summary(anova1)


#INTERVALOS DE CONFIANZA
confint(regresion1, level=0.95) #level: nivel de confianza

names(regresion1)
str(regresion1)
str(regresion1[["residuals"]])
residuo1 <-regresion1[["residuals"]]
predicciones1 <- regresion1[["fitted.values"]]
data2 <- data.frame(utilidad1, ventas1, predicciones1, residuo1)
View(data2)
#utilidad:Yi, ventas:Xi, predicciones:Yi techo, Residuos:Ui techo

hist(residuo1,15,labels=T,plot=T,main="Histograma de Residuos",xlab="Residuos",ylab="Frecuencia") #Histograma de los Ui techo
mean(residuo1) #media
qqnorm(residuo1,,main="Gráfico de Normalidad") #grafico de la normalidad
qqline(residuo1,col="green",plot=T) #grafico de la normalidad con la recta

plot(ventas1,utilidad1)
plot(residuo1,predicciones1)

#grafico de los residuos en funcion de y^
plot(data2[,"ventas1"],data2[,"utilidad1"])
plot(residuo1,predicciones1)
plot(residuo1,data2[,"ventas1"])
