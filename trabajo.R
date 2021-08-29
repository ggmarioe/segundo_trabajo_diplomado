library(readr)
library(skimr)
library(dplyr)
library(janitor)
library(ggplot2)
library(broom)
library(ROCit)

# a Cargar los datos en R
rh_data <- read.csv("datos/rrhh.csv")
rh_data <- rename(rh_data, Ratio_Pago = Ratio.Pago)
rh_data <- rename(rh_data, Estado_Civil = Estado.Civil)
rh_data <- rename(rh_data, Dias_Trabajados = Dias.trabajados)
rh_data <- rename(rh_data, Desempenio = Desempeño)
janitor::clean_names(rh_data)

rh_data <- rh_data %>% mutate(Departamento = as.factor(Departamento),
                              Posicion = as.factor(Posicion),
                              Estado_Civil = as.factor(Estado_Civil),
                              Desempenio = as.factor(Desempenio))
View(rh_data)

# b
# Realice un análisis descriptivo de sus datos. Determinar si existen 
# observaciones faltantes, en el caso de existir tome la decisión de omitirlas 
# del estudio u omitir la variable. Evalúe si existen posibles incongruencias 
# en la fuente de datos (ej: edades negativas). Y finalmente análice la 
# presencia de valores atípicos en las variables. Comente.
skimr::skim(rh_data)

# se determina que no existen datos faltantes y la data no presenta datos erroneos

glimpse(rh_data)
View(rh_data)
#Visualizar la data por tipo

# c
# Realice análisis de cómo se relacionan las variables continuas 
# con la variable de interés. Acompañe con gráficos y estadísticas. 
# ¿Qué variables pudieran resultar significativas a la hora de modelar la 
# probabilidad de que el trabajador sea desvinculado a la empresa?

#Edad
ggplot(data= rh_data, aes(x= "",y= Edad)) + geom_boxplot(color="black",fill= "lightgreen") + 
  theme_minimal()
boxplot.stats(rh_data$Edad)$out

#Ratio de Pago
ggplot(data= rh_data, aes(x= "",y= Ratio_Pago))+geom_boxplot(color="black",fill= "lightgreen") + 
  theme_minimal()
boxplot.stats(rh_data$Ratio.Pago)$out

#Salario
ggplot(data= rh_data, aes(x= "",y= Salario)) + geom_boxplot(color="black",fill= "lightgreen") + 
  theme_minimal()
boxplot.stats(rh_data$Salario)$out

#Dias Trabajados
ggplot(data= rh_data, aes(x= "",y= Dias_Trabajados)) + geom_boxplot(color="black",fill= "lightgreen") + 
  theme_minimal()
boxplot.stats(rh_data$Dias.trabajados)$out

#Ausencias
ggplot(data= rh_data, aes(x= "",y= Ausencias)) + geom_boxplot(color="black",fill= "lightgreen") + 
  theme_minimal()
boxplot.stats(rh_data$Ausencias)$out



analisis_relacion1 <- anova(aov(Estado ~ Ratio_Pago + Salario, data = rh_data))
summary(analisis_relacion1)


# REALIZAR GRAFICO DE CORRELACIÓN

# d 
# Realice análisis de cómo se relacionan las variables categóricas con la variable
# de interés. Acompañe con gráficos y estadísticas. 
# ¿Qué variables pudieran resultar significativas a la hora de modelar la 
# probabilidad de que el trabajador sea desvinculado a la empresa?


categoricas <- rh_data %>% select(c(Sexo,Departamento, Estado_Civil, Posicion, Desempenio))
View(categoricas)


ggplot(data = rh_data, aes(x=Dias_Trabajados , y=Estado )) +
  geom_point(col="#551A8B", size = 1.5) +
  labs(x = "Días Trabajados",
       y = "Estado") 

ggplot(data = rh_data, aes(x=Estado , y=Desempenio )) +
  geom_point(col="#551A8B", size = 1.5) +
  labs(x = "Desempenio",
       y = "Estado") 


 ## Modelamiento

# E 
# Realice una separación de la base de datos en un set de entrenamiento y set 
# de validación, utilice una proporción de 75:25 respectivamente. Para poder 
# replicar sus resultados, fije una semilla antes de obtener los indices. 
# Para ello, utilice la función set.seed(2021).

set.seed(2021)
tamano <- floor(0.75 * nrow(rh_data))
#listado de muestras a tomar
sampling <- sample(seq_len(nrow(rh_data)), size = tamano)

modelo_train <- rh_data %>% slice(sampling) #data de entrenamiento
modelo_test <- rh_data %>% slice(-sampling) #data de validación

#F 
# Con los datos de entrenamiento ajuste un modelo de regresión logística para
# estudiar la probabilidad de que el trabajador sea desvinculado de la empresa. 
# Para ello, utilice las variables edad y desempeño.

modelo_desvinculacion <- glm(Estado ~ Edad + Desempenio, 
                             data = modelo_train,
                             family = binomial(link = "logit"))
summary(modelo_desvinculacion)

# g
# Calcule e interprete los OR correspondientes al modelo, 
# ¿son estos factores protectores o agravantes de la desvinculación del trabajador?
broom::tidy(modelo_desvinculacion) %>% mutate(OR  = exp(estimate))



# h
# Utilizando un método automatizado, encuentre el modelo óptimo usando como 
# criterio el criterio de información de Akaike (AIC). La función step() 
# puede ser de utilidad.
modelo_nulo <- glm(Estado ~ 1, data = rh_data, family = binomial(link = 'logit'))
modelo_full <- glm(Estado ~ ., data = rh_data, family = binomial(link = 'logit'))
modelo_final <- step(object = modelo_nulo, direction = 'forward', 
                           scope= list(upper = modelo_full,
                                       lower = modelo_nulo),
                         trace = T)

formula(modelo_final)
summary(modelo_final)
AIC(modelo_nulo, modelo_final)
# i
# Si usted trabaja en la empresa ABAC, calcule su probabilidad de 
# ser desvinculado. Suponga que sus características son:
# Edad: Edad del trabajador en años.
# Ratio.Pago: Medida de pago por hora (numerico)
# Salario: Salario mensual en dólares que tiene o tenía el trabajador
# Dias.trabajados: Días que lleva o llevaba trabajando en la empresa
# Ausencias: Días que ha faltado a trabajar
# Sexo: Sexo del trabajador (Female , Male)
# Estado.Civil: Estado civil del trabajador (1: divorciado, 2: casado,3: separado, 4: soltero, 5: viuda)
# Departamento: Lugar de trabajo en la empresa (Admin Offices,..)
# Posicion: Cargo del trabajador/empleado (Accountant I ,???. )
# Desempeño: Clasificación del desempeño del trabajador.

new_data <- data.frame(
  Edad = 34,
  Ratio.Pago= 34.95,
  Salario = 3345.2,
  Dias_Trabajados = 3247,
  Ausencias = 16,
  Sexo = "Female",
  Estado_Civil = 2,
  Departamento= "Admin Offices",
  Posicion= "Sr. Accountant",
  Desempeño= "Fully Meets"
)

probabilidad <- predict.glm(modelo_final, newdata = new_data, type = "response")

resultado_prediccion <- ifelse(probabilidad >=0.5, 0, 1 )
View(resultado_prediccion)


# Validación del modelo

# j
# Utilizando la base de validación y el modelo obtenido en la pregunta anterior, 
# calcule las probabilidades de que el trabajador sea desvinculado.

probabilidad_desvinculación <-  predict.glm(modelo_final,
                                                  newdata = modelo_test,
                                                  type = "response")
y_pred <- ifelse(probabilidad_desvinculación >= 0.7, 1 , 0)
probabilidad_desvinculación

# k
# Identifique el punto de corte que optimice la sensibilidad del modelo, 
# pero que cometa como máximo una tasa de falsos positivos (1 - Especificidad) 
# de a lo más un 25%. Use el argumento returnSensitivityMat = TRUE en la 
# función plotROC(). Y obtenga las matrices de confusión y los indicadores de:

predictedscores <- plogis(predict(modelo_final, modelo_test )) 

corte <- InformationValue::optimalCutoff(modelo_test$Estado,
                                       predictedscores)

roc <- InformationValue::plotROC(actuals = modelo_test$Estado, 
               predictedScores = predictedscores,
               returnSensitivityMat = TRUE)
roc

corte_25<-min(roc[roc$One_minus_specificity<0.25, 'Threshold'])
corte_25

# l
# Evalúe el modelo y concluya. Para ello, obtenga e interprete los siguientes 
# estadísticos:
# Área bajo la curva ROC
InformationValue::plotROC(actuals = modelo_test$Estado, 
                          predictedScores = predictedscores,
                          returnSensitivityMat = TRUE)

InformationValue::confusionMatrix(modelo_test$Estado,
                                  predictedscores,
                                  corte)

InformationValue::sensitivity(modelo_test$Estado,
                              predictedscores,
                              corte)

InformationValue::specificity(modelo_test$Estado,
                              predictedscores,
                              corte)

InformationValue::precision(modelo_test$Estado,
                            predictedscores,
                            corte)
# Test de Kolmogorov - Smirnov (Hint: utilice la función ks.test(x, y) ).
ks <- ksplot(rocit(score = predictedscores, class = modelo_test$Estado))

library(ResourceSelection)
# Test de Hosmer - Lemeshow (Hint: utilice la función ResourceSelection::hoslem.test() ).
y_real  =validation_data$Estado
DescTools::HosmerLemeshowTest(fit = probabilidad_desvinculación , obs = y_real)
# H1: Exite una diferencia entre los valores observados y valores pronosticados

MLmetrics::Gini(modelo_test$Estado, y_pred)
