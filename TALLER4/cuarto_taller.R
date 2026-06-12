library(readxl)
library(ggplot2)
library(dplyr)
library(car)

datos <- read_xlsx("audicion.xlsx")

str(datos)

mean(datos$post)
sd(datos$post)

datos %>% 
  ggplot(aes(x=post)) +
  geom_boxplot() +
  theme_minimal()

datos <- datos %>% mutate(tratamiento = as.factor(tratamiento),
                          sexo = as.factor(sexo))

datos %>% 
  ggplot(aes(x=tratamiento, y=post)) +
  geom_boxplot() +
  theme_minimal()

datos %>% 
  ggplot(aes(x=sexo, y=post)) +
  geom_boxplot() +
  theme_minimal()

cor <- with(datos, cor(pre,post))

datos %>% 
  ggplot(aes(x=pre, y=post)) +
  geom_point(aes(color = tratamiento)) +
  theme_minimal() +
  geom_abline()

datos %>% 
  ggplot(aes(x=pre, y=post)) +
  geom_point(aes(color = sexo)) +
  theme_minimal() +
  geom_abline()

#efecto tratamiento con estadistico t
dif_tratamientos <- t.test(post ~ tratamiento, data = datos, var.equal = TRUE)
dif_tratamientos

#idem. con modelin lineal
modelin <- lm(post ~ tratamiento, data=datos)
summary(modelin)

#rechazamos muexp = muctrl

#efecto tratamiento y sexo
modelin2 <- lm(post ~ tratamiento + sexo, data=datos)
summary(modelin2)
#sea cual sea el sexo el tratamiento es igual

#pero el efecto del tratamiento esta afectado por el sexo?
#interacción
modelin3 <- lm(post ~ tratamiento*sexo, data=datos)
summary(modelin3)
#la interaccion no es significativa,no?

Anova(modelin3)
#aparentemente NO es significativa, el sexo no modifica el efecto del tratamiento
#volvemos al anterior? la signif del sexo tampoco aportaba, entonces volvemos al primero, solo tratamiento
#modelin tiene mas potencia

#Es posible que el grupo experimental haya tenido mejores resultados debido a que sus
#participantes ya tuvieran mejor audición. 

modelin4 <- lm(post ~ tratamiento + pre, data = datos)
summary(modelin4)
#hay un tratamiento q funciona mejor q el otro incluso incluyendo pre

#considerando sexo
modelin5 <- lm(post ~ tratamiento + pre + sexo, data = datos)
summary(modelin5)
#sexo no significativo

modelin6 <- lm(post ~ tratamiento + pre + sexo + sexo*tratamiento, data = datos)
summary(modelin6)
#duda: incluir sexo y tratamiento + interacción?

#conclusao: no hay diferencias entre sexos

#a realizar: construir ganancia post-pre y ver si da lo mismo :)



