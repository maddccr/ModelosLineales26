install.packages("xlsx")
library(xlsx)
library(tidyverse)
library(dplyr)



condados <- read.xlsx("condados.xlsx", sheetIndex = 1)
#OJO EN EL IMPORT SE PUEDE ACLARAR COMO PONER LOS NAs



Alabama <- subset(condados, condados$estado=="Alabama")


#CHEQUEAMOS QUE LAS VARIABLES A USAR SEAN NUMERICAS
str(Alabama)
Alabama$esp_vida <- as.numeric(Alabama$esp_vida)

Alabama$ing_med_hog <- as.numeric(Alabama$ing_med_hog)
Alabama$pct_bpeso <- as.numeric(Alabama$pct_bpeso)

modelito <- lm(esp_vida ~ pct_diabetes + pct_obesidad + pct_est_terc + ing_med_hog + pct_bpeso,
   data = Alabama)
summary(modelito)

#Beta de ing_med_hog --> 0,000085 PERO CUAL ES LA UNIDAD??? valor chico por la dif con la escala
#USAR ESCALAS MAS PARECIDAS
#significacion individual, p < 0,01 rechazo que no sirve pa nada. 
#Si tiene una asociacion positiva con la esperanza de vida 
#que significa???EN PROMEDIO unidad 1 dollar con lo demas constante aunmeta una horita. Cambiar la unidad a un dolar
#cambiar la escala 

#cuando aumenta una unidad es 1%


#no tiene relacion, ajustando por las demas variables!!!!! si traen la mismo info 
#SABER HACER TODO EL CUENTITO :(((((

#r2 con tanto porcentage de la variabilidad explica 


