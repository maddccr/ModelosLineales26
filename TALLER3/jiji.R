
library(tidyverse)
library(car)
library(ggplot2)
library(skedastic)

data <- readxl::read_xlsx("diagnostico.xlsx", skip = 7)
str(data) #todo num
head(data)

#ojo falto analisis descriptivo
#CORRELACIONES






modelito <- lm (stress_score ~ workload_index + sleep_quality + screen_time + physical_activity + caffeine_intake, data = data)

summary(modelito)

#PASO 2
vif(modelito)
#hay dos variables con vif alto workload y screentime



#PASO 3 (predichos vs residuos)

data$pred <- fitted(modelito)
# extraemos los residuos
data$r_i <- residuals(modelito)  # residuos
data$s_i <- rstandard(modelito)  # studendizados INTERNAMENTE
data$t_i <- rstudent(modelito)   # studentizados EXTERNAMENTE

ggplot(data, aes(x = pred, y = t_i)) + 
  geom_point() +
  xlab('Predichos') +
  ylab('Residuos') +
  geom_abline(slope = 0, intercept = 0) +
  theme_minimal()

variables <- names(data) 

sapply(variables, function(v){
  ggplot(data, aes_string(x = v, y = "t_i")) + 
    geom_point() +
    ylab('Residuos') +
    geom_abline(slope = 0, intercept = 0) +
    theme_minimal()
})


variables

# TERCER PASO. Test de hipotesis
# H0) E(eps^2_i) = sigma^2
# H1) E(eps^2_i) = sigma^2 * h(X_1, X_2, ..., X_k)

#alpha = 0.05
bp_test <- breusch_pagan(modelito)
bp_test #NOOOOOO SE RECHAZA H0. Entonces cagamos

#PASO 5 normalidad de los residuos!!!!
qqPlot(modelito) 


# PRUEBA DE HIPOTESIS
# H0) Los errores son normales
# H1) Los errores NO son normales

shapiro.test(data$t_i)
tseries::jarque.bera.test(data$t_i)
ks.test(data$t_i, 'pnorm')
#RECHAZAMOS H0 NOOOOOO



sapply(variables[2:6], function(v){
  crPlot(modelito, variable = v)
})

#PASO 7 

# medidas de influencia
h_i <- influence(modelito)$hat
D_i <- cooks.distance(modelito)
df <- data.frame(i = 1:nrow(data),
                 h_i = h_i,
                 D_i = D_i)

# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/nrow(data), col = 2, linetype = 'dashed')


nosacar <- df %>% filter(D_i > 4/nrow(data)) 

#CONLUSION ODIOTODO




modelito2 <- update(modelito, . ~ . -workload_index)



library(EnvStats)
rosnerTest(data$sleep_qualit, k = 3) # k is the maximum number of suspected outliers


#https://rpubs.com/Alema/1000582

# Get studentized residuals
stud_res <- rstudent(modelito)
outliers <- which(abs(stud_res) > 3)
print(data[outliers, ])

sacar


summary(modelito2)


#hacer normal???



