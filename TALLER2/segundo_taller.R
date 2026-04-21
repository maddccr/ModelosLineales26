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

#Beta de ing_med_hog --> 0,000085 PERO CUAL ES LA UNIDAD???
