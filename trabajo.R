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
rh_data <- rename(rh_data, Desempenio = Desempe�o)
janitor::clean_names(rh_data)

rh_data <- rh_data %>% mutate(Departamento = as.factor(Departamento),
                              Posicion = as.factor(Posicion),
                              Estado_Civil = as.factor(Estado_Civil),
                              Desempenio = as.factor(Desempenio))
View(rh_data)

# b
# Realice un an�lisis descriptivo de sus datos. Determinar si existen 
# observaciones faltantes, en el caso de existir tome la decisi�n de omitirlas 
# del estudio u omitir la variable. Eval�e si existen posibles incongruencias 
# en la fuente de datos (ej: edades negativas). Y finalmente an�lice la 
# presencia de valores at�picos en las variables. Comente.
skimr::skim(rh_data)

# se determina que no existen datos faltantes y la data no presenta datos erroneos

glimpse(rh_data)
View(rh_data)
#Visualizar la data por tipo

# c
# Realice an�lisis de c�mo se relacionan las variables continuas 
# con la variable de inter�s. Acompa�e con gr�ficos y estad�sticas. 
# �Qu� variables pudieran resultar significativas a la hora de modelar la 
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


# REALIZAR GRAFICO DE CORRELACI�N

# d 
# Realice an�lisis de c�mo se relacionan las variables categ�ricas con la variable
# de inter�s. Acompa�e con gr�ficos y estad�sticas. 
# �Qu� variables pudieran resultar significativas a la hora de modelar la 
# probabilidad de que el trabajador sea desvinculado a la empresa?


categoricas <- rh_data %>% select(c(Sexo,Departamento, Estado_Civil, Posicion, Desempenio))
View(categoricas)


ggplot(data = rh_data, aes(x=Dias_Trabajados , y=Estado )) +
  geom_point(col="#551A8B", size = 1.5) +
  labs(x = "D�as Trabajados",
       y = "Estado") 

ggplot(data = rh_data, aes(x=Estado , y=Desempenio )) +
  geom_point(col="#551A8B", size = 1.5) +
  labs(x = "Desempenio",
       y = "Estado") 


 ## Modelamiento

# E 
# Realice una separaci�n de la base de datos en un set de entrenamiento y set 
# de validaci�n, utilice una proporci�n de 75:25 respectivamente. Para poder 
# replicar sus resultados, fije una semilla antes de obtener los indices. 
# Para ello, utilice la funci�n set.seed(2021).

set.seed(2021)
tamano <- floor(0.75 * nrow(rh_data))
#listado de muestras a tomar
sampling <- sample(seq_len(nrow(rh_data)), size = tamano)

modelo_train <- rh_data %>% slice(sampling) #data de entrenamiento
modelo_test <- rh_data %>% slice(-sampling) #data de validaci�n

#F 
# Con los datos de entrenamiento ajuste un modelo de regresi�n log�stica para
# estudiar la probabilidad de que el trabajador sea desvinculado de la empresa. 
# Para ello, utilice las variables edad y desempe�o.

modelo_desvinculacion <- glm(Estado ~ Edad + Desempenio, 
                             data = modelo_train,
                             family = binomial(link = "logit"))
summary(modelo_desvinculacion)

# g
# Calcule e interprete los OR correspondientes al modelo, 
# �son estos factores protectores o agravantes de la desvinculaci�n del trabajador?
broom::tidy(modelo_desvinculacion) %>% mutate(OR  = exp(estimate))



# h
# Utilizando un m�todo automatizado, encuentre el modelo �ptimo usando como 
# criterio el criterio de informaci�n de Akaike (AIC). La funci�n step() 
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
# ser desvinculado. Suponga que sus caracter�sticas son:
# Edad: Edad del trabajador en a�os.
# Ratio.Pago: Medida de pago por hora (numerico)
# Salario: Salario mensual en d�lares que tiene o ten�a el trabajador
# Dias.trabajados: D�as que lleva o llevaba trabajando en la empresa
# Ausencias: D�as que ha faltado a trabajar
# Sexo: Sexo del trabajador (Female , Male)
# Estado.Civil: Estado civil del trabajador (1: divorciado, 2: casado,3: separado, 4: soltero, 5: viuda)
# Departamento: Lugar de trabajo en la empresa (Admin Offices,..)
# Posicion: Cargo del trabajador/empleado (Accountant I ,???. )
# Desempe�o: Clasificaci�n del desempe�o del trabajador.

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
  Desempe�o= "Fully Meets"
)

probabilidad <- predict.glm(modelo_final, newdata = new_data, type = "response")

resultado_prediccion <- ifelse(probabilidad >=0.5, 0, 1 )
View(resultado_prediccion)


# Validaci�n del modelo

# j
# Utilizando la base de validaci�n y el modelo obtenido en la pregunta anterior, 
# calcule las probabilidades de que el trabajador sea desvinculado.

probabilidad_desvinculaci�n <-  predict.glm(modelo_final,
                                                  newdata = modelo_test,
                                                  type = "response")
y_pred <- ifelse(probabilidad_desvinculaci�n >= 0.7, 1 , 0)
probabilidad_desvinculaci�n

# k
# Identifique el punto de corte que optimice la sensibilidad del modelo, 
# pero que cometa como m�ximo una tasa de falsos positivos (1 - Especificidad) 
# de a lo m�s un 25%. Use el argumento returnSensitivityMat = TRUE en la 
# funci�n plotROC(). Y obtenga las matrices de confusi�n y los indicadores de:

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
# Eval�e el modelo y concluya. Para ello, obtenga e interprete los siguientes 
# estad�sticos:
# �rea bajo la curva ROC
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
# Test de Kolmogorov - Smirnov (Hint: utilice la funci�n ks.test(x, y) ).
ks <- ksplot(rocit(score = predictedscores, class = modelo_test$Estado))

library(ResourceSelection)
# Test de Hosmer - Lemeshow (Hint: utilice la funci�n ResourceSelection::hoslem.test() ).
y_real  =validation_data$Estado
DescTools::HosmerLemeshowTest(fit = probabilidad_desvinculaci�n , obs = y_real)
# H1: Exite una diferencia entre los valores observados y valores pronosticados

MLmetrics::Gini(modelo_test$Estado, y_pred)
