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
barplot(delitos$cantidad, main = "Tabla de Delitos", xlab = "Delitos",ylab = "Cantidad", col = "lightblue", names.arg =delitos$Delito)

#######
SubDelito<- datos_excel %>% group_by(SubDelito) %>% 
  summarise(cantidad=n())

boxplot(SubDelito$cantidad)
barplot(SubDelito$cantidad, main = "Tabla de SubDelito", xlab = "SubDelito",ylab = "Cantidad", col = "lightblue", names.arg =SubDelito$SubDelito)

#######
Victima<- datos_excel %>% group_by(Victima) %>% 
  summarise(cantidad=n())

boxplot(Victima$cantidad)
barplot(Victima$cantidad, main = "Tabla de Victima", xlab = "Victima",ylab = "Cantidad", col = "lightblue", names.arg =Victima$Victima )


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













