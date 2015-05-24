##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre:
##-- Nombre: Paúl Ubillús


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()

data <- read.table("data.txt",header=T,dec=",",sep="\t")
str(data)

# 2.2 Calcular el mínimo, la media, el máximo de la variable Edad.
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE

edad<-data[,"Edad"]
minimo<-min(edad,na.rm =T )
media<-mean(edad,na.rm = T)
maximo<-max(edad, na.rm =T)
minimo
media
maximo



# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()

data_fem<-subset(data, subset=data[,"Genero"]=="Femenino")
table(data_fem[,"Genero"])
num_fem<-nrow(data_fem)
num_fem

# 2.4 Encontrar la Edad mínima, media, máxima de los sujetos que Si son dependientes.

data_dep<-subset(data, subset=data[,"Dependiente"]=="Si")
table(data_dep[,"Dependiente"])
edad_dep<-data_dep[,"Edad"]
minimo_dep<-min(edad_dep,na.rm =T )
media_dep<-mean(edad_dep,na.rm = T)
maximo_dep<-max(edad_dep, na.rm =T)
minimo_dep
media_dep
maximo_dep

# 2.5 Identificar el tipo de elementos que contiene cada variable.
# Utilice la función typeof()

tipo <- numeric(ncol(data))
for(i in 1:ncol(data)){
  tipo[i] <- typeof(data[,i])
}
tipo

# 2.6 Identificar la clase de cada variable (columna).
# Utilice la función class()

clase <- numeric(ncol(data))
for(i in 1:ncol(data)){
  clase[i] <- class(data[,i])
}
clase

# 2.7 Calcular la media de todas las variables numéricas (double, integer).
# Recordar que para un factor no es posible obtener la media debido a que 
# éstos representan variables

media <- numeric(ncol(data))
for(i in 1:ncol(data)){
media[i] <- mean(data[,i])
}
media

# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
# Utilice la función is.na()

valores_perd <- numeric(ncol(data))
porcentaje <- numeric(ncol(data))
total <- nrow(data)
for(i in 1:ncol(data)){
  valores_perd[i] <- sum(is.na(data[,i]))
  porcentaje[i] <- (valores_perd[i]/total)                         
}
valores_perd
porcentaje

# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.
# Utilice la función subset()

data_crit1<-subset(data, subset=data[,"Edad"]>="40")
table(data_crit1[,"Edad"])

# 3.2 Seleccione los sujetos que tienen Vivienda Propia.

data_crit2<-subset(data, subset=data[,"Vivienda"]=="Propia")
table(data[,"Vivienda"])
table(data_crit2[,"Vivienda"])


# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiatres.

data_crit3<-subset(data, subset=data[,"Cargas"]>2)
table(data[,"Cargas"])
table(data_crit3[,"Cargas"])

# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.

data_crit4<-subset(data, subset=data[,"Deuda"]>="500")&(subset=data[,"Dias_Atraso"]>8)
table(data[,"Deuda"])
table(data_crit4[,"Deuda"])

# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC).

data_crit5<-subset(data, subset=data[,"Score"]>=900)&(subset=data[,"Edad"]<=35)&(subset=data[,"Numero_TC"]>3)

# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red
hist(data$Edad,col="red",main="Diagrama de Caja de Edad",ylab="Frecuencia",xlab="Edad en Años",xlim=c(10,70),breaks=100)

# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green
# Utilice la función boxplot()
boxplot(data$Edad,col="green",main="Diagrama de Caja de Edad",ylab="Datos")

