#grillos
grillos <- readxl::read_excel("grillos.xlsx")
#ojito con el session

#dimensiones
nrow(grillos)
ncol(grillos) 
dim(grillos)


summary(grillos)
str(grillos) #VARIABLES Y SUS CLASES


#HISTOGRAMA
hist(grillos$temp_F, breaks = 10)

library(ggplot2)
ggplot(grillos) + geom_histogram(aes( x = temp_F), color = "pink", fill = "pink", bins = 8) +
labs(title = "Histograma de la Temperatura en Fahrenheit") +
theme_minimal() +
theme(plot.title = element_text(hjust = 0.5))


ggplot(grillos) + geom_boxplot(aes( x = temp_F), color = "deeppink", bins = 13) +
labs(title = "Diagrama de Caja de la Temperatura en Fahrenheit") +
theme_minimal() +
theme(plot.title = element_text(hjust = 0.5))

#RANGO Y CONCENTRACION // ASIMETRIA


#Descripcion univariada (una de pos y otra de variabilidad)
summary(grillos)[,3:4]
sd(grillos$temp_F) 
sd(grillos$chirps_15s)

#HACER UN CUADRATITO // ESCRIBIR MAS DE LOS QUE MUESTRA // Buscar 


#BIVARIADA
#Para numericas// correlacion lineal
cor(grillos$temp_F, grillos$chirps_15s) #0.970876

ggplot(grillos) + geom_point(aes( x = chirps_15s, y = temp_F), color = "deeppink") +
  labs(title = "Temperatura en Fahrenheit vs. Estridulaciones") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



#Observamos una asociacion alta, fuerte, entre la temp y la bla bla bla. Con un coefinciente de correlacion positivo y cercano a 1

x <- grillos$chirps_15s
y<- grillos$temp_F
lm(y ~ x)


mod <- lm(temp_F ~ chirps_15s, grillos)
summary(mod) #depende del objeto // es un metodo

#SIGNIFICADO DE LOS BETITAS 




