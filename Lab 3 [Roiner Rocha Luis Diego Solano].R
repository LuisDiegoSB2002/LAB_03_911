library(readxl)
install.packages("readxl")
# %>% = Ctrl + Shif + M


ruta_del_archivo <- "Files/estadsticaspoliciales2023.xlsx"
datos_excel <- read_excel(ruta_del_archivo)


##a. Exploración Inicial de los Datos:
#i.# Obtener información sobre las columnas y tipos de datos
head(datos_excel)
class(datos_excel)
names(datos_excel)
glimpse(datos_excel)
str(datos_excel)
dim(datos_excel)
colnames(datos_excel)

#ii.Resumen estadístico de las variables numéricas (5 puntos)


summary(datos_excel)


##b.Limpieza y Tratamiento de Datos:

#i. Determine valores nulos (10 puntos)
colSums(is.na(datos_excel))
is.na(datos_excel)       # Vector lógico con T==NA
any(is.na(datos_excel))   # TRUE = hay al menos un valor NA
anyNA(datos_excel)        # Alternativa a lo anterior
which(is.na(datos_excel)) # Indica las coordenadas donde están los NA
mean(is.na(datos_excel))  # Porcentaje de valores NA
sum(is.na(datos_excel))   # Cantidad de valores perdidos en el vector





# ii. Determine valores atípicos (10 puntos)
library(dplyr)

delitos<- datos_excel %>% group_by(Delito) %>% 
  summarise(cantidad=n())

boxplot(delitos$cantidad)
barplot(delitos$cantidad, main = "Tabla de Delitos", xlab = "Delitos",ylab = "Cantidad", col = "lightblue")

#######
SubDelito<- datos_excel %>% group_by(SubDelito) %>% 
  summarise(cantidad=n())

boxplot(SubDelito$cantidad)
barplot(SubDelito$cantidad, main = "Tabla de SubDelito", xlab = "SubDelito",ylab = "Cantidad", col = "lightblue")

#######
Victima<- datos_excel %>% group_by(Victima) %>% 
  summarise(cantidad=n())

boxplot(Victima$cantidad)
barplot(Victima$cantidad, main = "Tabla de Victima", xlab = "Victima",ylab = "Cantidad", col = "lightblue")
# Generar el gráfico de barras
grafico_barras <- barplot(Victima$cantidad)

# Crear un vector de etiquetas
etiquetas <- Victima$cantidad

# Agregar etiquetas a las barras
text(x = grafico_barras, y = Victima$cantidad, labels = etiquetas, pos = 3)




#######
Genero<- datos_excel %>% group_by(Genero) %>% 
  summarise(cantidad=n())

boxplot(Genero$cantidad)
barplot(Genero$cantidad, main = "Tabla de Genero", xlab = "Genero",ylab = "Cantidad", col = "lightblue")

#######
Nacionalidad<- datos_excel %>% group_by(Nacionalidad) %>% 
  summarise(cantidad=n())

boxplot(Nacionalidad$cantidad)
barplot(Nacionalidad$cantidad, main = "Tabla de Nacionalidad", xlab = "Nacionalidad",ylab = "Cantidad", col = "lightblue")

#######
Provincia<- datos_excel %>% group_by(Provincia) %>% 
  summarise(cantidad=n())

boxplot(Provincia$cantidad)
barplot(Provincia$cantidad, main = "Tabla de Provincia", xlab = "Provincia",ylab = "Cantidad", col = "lightblue")

#######
Distrito<- datos_excel %>% group_by(Distrito) %>% 
  summarise(cantidad=n())

boxplot(Distrito$cantidad)
barplot(Distrito$cantidad, main = "Tabla de Distrito", xlab = "Distrito",ylab = "Cantidad", col = "lightblue")

#######


# iii. Determine concentraciones y tendencia de datos (10 puntos)

hist(delitos$cantidad)



# iv.Determine acciones correctivas (imputación) (15 puntos)

# Imputación con un valor específico (por ejemplo, desconocido)
datos_excel$Delito [is.na(datos_excel$Delito )] <- "desconocido"
datos_excel$SubDelito [is.na(datos_excel$SubDelito  )] <- "desconocido"
datos_excel$Victima [is.na(datos_excel$Victima  )] <- "desconocido"
datos_excel$SubVictima [is.na(datos_excel$SubVictima   )] <- "desconocido"
datos_excel$Edad [is.na(datos_excel$Edad  )] <- "desconocido"
datos_excel$Genero [is.na(datos_excel$Genero  )] <- "desconocido"
datos_excel$Nacionalidad [is.na(datos_excel$Nacionalidad  )] <- "desconocido"
datos_excel$Provincia [is.na(datos_excel$Provincia )] <- "desconocido"
datos_excel$Canton [is.na(datos_excel$Canton  )] <- "desconocido"
datos_excel$Distrito [is.na(datos_excel$Distrito )] <- "desconocido"



###c. Visualización de datos.
##i. Realice las gráficas de datos y agrupaciones para las variables categóricas en el dataset (20 puntos)

library(dplyr)
library(ggplot2)

# Crear un nuevo conjunto de datos que incluya todas las variables de interés
datos_combinados <- rbind(
  data.frame(Variable = "Delito", Cantidad = delitos$cantidad),
  data.frame(Variable = "SubDelito", Cantidad = SubDelito$cantidad),
  data.frame(Variable = "Victima", Cantidad = Victima$cantidad),
  data.frame(Variable = "Genero", Cantidad = Genero$cantidad),
  data.frame(Variable = "Nacionalidad", Cantidad = Nacionalidad$cantidad),
  data.frame(Variable = "Provincia", Cantidad = Provincia$cantidad),
  data.frame(Variable = "Distrito", Cantidad = Distrito$cantidad)
)

# Crear un gráfico combinado
ggplot(datos_combinados, aes(x = Variable, y = Cantidad)) +
  geom_boxplot() +
  geom_histogram(stat = "identity", fill = "red", alpha = 0.5) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Valores Atípicos, Concentraciones y Tendencia de Datos",
       x = "Variable",
       y = "Cantidad")









































































# acceder a columnas y filas de un dataframe

datos_excel[,]
datos_excel[,1]
datos_excel[c(1:3),c(1,2)]
datos_excel[,c(1:3)]



is.na(datos_excel)       # Vector lógico con T==NA
any(is.na(datos_excel))   # TRUE = hay al menos un valor NA
anyNA(datos_excel)        # Alternativa a lo anterior
which(is.na(datos_excel)) # Indica las coordenadas donde están los NA
mean(is.na(datos_excel))  # Porcentaje de valores NA
sum(is.na(datos_excel))   # Cantidad de valores perdidos en el vector


sum(is.na(datos)) / length(datos) #proporción de valores faltantes

apply(is.na(datos), 2, mean)   # Porcentaje de NA por columna
apply(is.na(datos), 2, sum)    # Cantidad de NA por columna
apply(is.na(datos), 2, which)  # Posición de NA por columna

sapply(datos, function(x) sum(is.na(x)))
summarise_all(datos, funs(sum(is.na(.))))

which(is.na(datos[ , 5]))

heatmaply::heatmaply_na(datos) # ver los na de forma grafica


# obtener solo loelims elementos completos
complete.cases(datos)
datos[complete.cases(datos), ]

na.omit(datos)

#acceder a las observaciones donde alguna de las variables tiene un valor NA.
datos[!complete.cases(datos), ]

# Generar algunas graficas

library(hexbin)
library(ggplot2)
install.packages("hexbin")

plot(datos$nacionales)

ggplot(datos, aes(anio, nacionales)) + geom_point()

ggplot(datos, aes(anio,nacionales)) +
  #geom_hex() +
  geom_jitter(width =.3, alpha = 0.5) +
  labs(title="Diagrama de dispersión",
       subtitle= "ScatterPlot",
       caption="Fuente: CPS1985 (paquete AER)",
       x="Años",
       y="Cantidad de nacionales") +
  scale_fill_continuous("Pasajeros nacionales") +
  theme_classic()


hist(datos$nacionales, main = "Histograma de las ventas",
     xlab = "pasajeros", ylab = "Frecuencia",
     col = "purple")


boxplot(datos$nacionales,boxwex=0.8)
unique(datos$aeropuerto)

nacionales_ae1<-datos %>% filter(aeropuerto=="Limón" )

boxplot(nacionales ~ anio, 
        col = rainbow(ncol(nacionales_ae1)),
        data = nacionales_ae1,
        boxwex=0.8,
        main = "Nacionales por mes",
        scale_y_continuous(labels=scales::comma)
)
stripchart(data=datos,nacionales ~ anio,vertical = TRUE, 
           method = "jitter",pch=19,add = TRUE,
           col = 1:12
)

unique(datos$aeropuerto)

ggplot(datos, aes(x =aeropuerto , y = nacionales, fill = aeropuerto) ) +
  geom_boxplot(alpha = 0.3, outlier.colour = "blue") +
  labs(x = "Nacionales", y = "Años") +
  #scale_x_discrete(labels =  c(unique(datos$aeropuerto))) +
  guides(fill = FALSE) +
  coord_flip() +              
  geom_point(stat =  "summary", fun.y = mean, shape = 16, size = 4, color = "red") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.2) 



valores<-datos$nacionales
valores<-na.omit(valores)


hist(valores,freq = FALSE,main = "Cambiar color", ylab = "Frecuencia", col = "lightblue")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 2)
lines(density(valores), lwd = 2, col = 'red')
lines(density(valores), col = "blue", lwd = 2)
ylim=c(0,0.004)
par(new = TRUE)
boxplot(valores, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))

hist(valores, prob = TRUE,
     main = "Histograma con curva normal", ylab = "Densidad", col="ivory")
options(scipen = 99999)
x <- seq(min(valores), max(valores), length = length(valores))
f <- dnorm(x, mean = mean(valores), sd = sd(valores))
lines(x, f, col = "red", lwd = 2)
lines(density(valores), lwd = 2, col = 'blue')
legend("topright", c("Histograma", "Densidad", "Normal"), box.lty = 0,
       lty = 1, col = c("black", "blue", "red"), lwd = c(1, 2, 2))
par(new = TRUE)
boxplot(valores, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))



grupos<- datos %>% group_by(aeropuerto) %>% 
  summarise(totales=sum(total))

grupos

unique(datos$aeropuerto)

ggplot(grupos,aes(x=aeropuerto,y=totales,fill=aeropuerto))+
  geom_bar(stat="identity")+
  geom_text(aes(label = format(totales, big.mark= ".", trim = TRUE)), 
            position = "dodge",vjust = 0,fontface = "bold",size=5,color="blue"
  )+
  ggtitle("Grafico de pasajeros por aeropuerto")+
  xlab("Aeropuertos") +
  ylab("Totales") +
  scale_y_continuous(labels=scales::comma)+
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")+
  coord_flip()+
  theme(legend.position = "none")  


## El histograma 
hist(datos$nacionales) 
## El boxplot
boxplot(datos$nacionales)


grupo01<-datos %>% filter(aeropuerto=="Juan Santamaría")

ggplot(grupo01, aes(aeropuerto, nacionales)) + 
  geom_boxplot(notch = TRUE,fill = "white", colour = "#3366FF")

frecuencia<-table(datos$nacionales,datos$mes)

#prop.table(table(datos$nacionales,datos$mes),2)
#CrossTable(datos$nacionales,datos$mes)
#cor(datos$nacionales,datos$extranjeros)


#########################################

head(datos)
unique(datos$aeropuerto)

grupo02<-
  datos %>%  
  filter(aeropuerto == "Limón")

ggplot(grupo02, aes(grupo02$aeropuerto, grupo02$nacionales)) + 
  geom_boxplot(notch = TRUE,fill = "white", colour = "#3366FF")

data_aeropuerto1 %>% filter(nacionales>97000)

ggplot(grupo02, aes( x=mes, y=nacionales, fill=mes)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colores
               alpha = 0.9, outlier.colour = "red",size=1)+
  geom_point()+
  scale_y_continuous(name = "nacionales") +  # Etiqueta de la variable continua
  scale_x_discrete(name = "mes") +        # Etiqueta de los grupos
  stat_boxplot(geom = "errorbar", width = 0.5)+  # Bigotes
  geom_jitter(position=position_jitter(0.4),col="blue",size=2)+
  ggtitle("Boxplot por grupos en ggplot2") +       # Título del plot
  theme(axis.line = element_line(colour = "green", size = 0.5)) + # Personalización del tema
  scale_color_brewer(palette="RdBu")+ theme_minimal()+
  stat_summary(fun=mean,geom = "point",colour="darkred",size=3)+
  stat_summary(fun.data = mean_cl_boot,geom = "linerange",colour="red",size=3,alpha=0.2)


## grupo 3 

grupo03<-
  datos %>%  
  filter(aeropuerto == "Limón")

ggplot(grupo03, aes(aeropuerto, nacionales)) + 
  geom_boxplot(notch = TRUE,fill = "white", colour = "#3366FF")

limon<-grupo03$nacionales

head(limon)
dim(limon)


x2 <- seq(min(limon), max(limon), length = length(limon))
f <- dnorm(x2, mean(limon), sd(limon))


hist(limon,freq = FALSE,main = "Cambiar color", ylab = "Frecuencia", col = "lightblue")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
lines(density(limon), lwd = 2, col = 'red')
lines(density(limon), col = "blue", lwd = 2)
lines(x2, f, col = "red", lwd = 2) # Normal
par(new = TRUE)
boxplot(limon, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))



calculo <-function(numero, valor){
  resultado <- numero * valor
  return(resultado) 
  
}

calculo(1052,520)

install.packages("ggstance")
library(ggstance)

nbreaks <- pretty(range(limon), n = nclass.Sturges(limon),
                  min.n = 1)
View(limon)
colnames(limon)<-c("provincia")

limondf<-as.data.frame(limon)
colnames(limondf)<-c("provincia")

head(datos)
limondf<-datos %>% select(aeropuerto,nacionales) %>% filter(aeropuerto=="Aeropuerto Internacional Daniel Oduber Quirós")

ggplot(limondf, aes(x = nacionales)) +
  geom_histogram( bins = 30,alpha=0.5 ,color = "blue", fill = "blue") +
  geom_boxploth(aes(y = 3), width = 2, color = "red") +
  theme_minimal() 

########## analizar la correlacion de las variables  ##########################

# La correlación es una medida estadística que expresa hasta qué punto dos
# variables están relacionadas linealmente

library(corrplot)
library(GGally)
library(corrplot)
library(PerformanceAnalytics)
require(psych)
library(dplyr)
library(caret)

names(datos)

iris
variables<-data.frame(datos$nacionales,datos$extranjeros,datos$transito)
cor(ozono)
chart.Correlation(variables, histogram = TRUE, method = "pearson")

variables<-data.frame(datos$nacionales,datos$extranjeros,datos$transito)
cor(ozono)


ozono<-read.csv("Files/ozone.csv",header = T,sep = ";")
ozono<-na.omit(ozono)
variables<-data.frame(ozono$Humidity,ozono$Wind_speed,ozono$Visibility,ozono$pressure_height,ozono$Inversion_temperature    )

chart.Correlation(variables, histogram = TRUE, method = "pearson")
cor(iris[,c(1:4)])
corrplot.mixed(cor(ozono),
               lower = "number", 
               upper = "circle",
               tl.col = "black")

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
library(mongolite)

mongo(
  collection = "test",
  db = "test",
  url = "mongodb://localhost",
  verbose = FALSE,
  options = ssl_options()
)
my_collection$insert(name_of_collection)
data <- mongo_db$find(query = '{}')

##########  analizar la importacia de las variables xxxxxxxxxxxxxxxxxxxxxxxxx

install.packages("lattice")
set.seed(4543)
data(mtcars)

library(randomForest)
mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000, keep.forest=FALSE,
                          importance=TRUE)
imp <- varImpPlot(mtcars.rf,type = 2) # let's save the varImp object

library(lattice)
dotplot(imp, scales=list(y =list(cex=1,
                                 at = c(1:10),
                                 col='red',
                                 rot =15,
                                 axs='i') ,
                         x =list(cex=1,col='blue')) )

# otra forma
library(data.table)
library(Boruta)
mtcars
boruta.model <- Boruta(mpg~., data = mtcars, doTrace = 2)
plot(boruta.model)
boxplot(mtcars$qsec)

# en otro juego de datos

ozono<-read.csv("Files/ozone.csv",header = T,sep = ";")
library(randomForest)
names(ozono)
ozono<-na.omit(ozono)
View(ozono)
ozono.rf <- randomForest(Humidity ~ ., data=ozono, ntree=1000, keep.forest=FALSE,
                         importance=TRUE)
imp <- varImpPlot(ozono.rf,type = 2) # let's save the varImp object
cor.plot(ozono)

library(lattice)
dotplot(imp, scales=list(y =list(cex=1,
                                 at = c(1:10),
                                 col='red',
                                 rot =15,
                                 axs='i') ,
                         x =list(cex=1,col='blue')) )


# this part just creates the data.frame for the plot part
library(dplyr)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) 
rownames(imp) <- NULL  
imp$var_categ <- rep(1:2, 5) 

# this is the plot part, be sure to use reorder with the correct measure name
library(ggplot2) 
ggplot(imp, aes(x=reorder(varnames, IncNodePurity), weight=IncNodePurity, fill=as.factor(var_categ))) + 
  geom_bar() +
  scale_fill_discrete(name="Variable Group") +
  ylab("IncNodePurity") +
  xlab("Variable Name")


ggplot(imp, aes(x=reorder(varnames, IncNodePurity), y=IncNodePurity, color=as.factor(var_categ))) + 
  geom_point(size=5) +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=IncNodePurity)) +
  scale_color_discrete(name="Variable Group") +
  ylab("IncNodePurity") +
  xlab("Variable Name") +
  coord_flip()

########################################



