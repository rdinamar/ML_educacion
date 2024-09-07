# MODELOS PREDICTIVOS EN EDUCACIÓN

# lectura de datos --------------------------------------------------------

library(tidyverse)
library(caret)
library(e1071)

# Lectura de Archivo desde GitHub
df<-read.csv("https://raw.githubusercontent.com/rdinamar/bases_datos/main/StudentPerformanceFactors.csv")

# Estructura de datos y estadistica descriptiva -----------------------------------------------------

summary(df)

str(df)

table(df$Parental_Involvement)
table(df$Access_to_Resources)
table(df$Extracurricular_Activities)
table(df$Motivation_Level)
table(df$Internet_Access)
table(df$Distance_from_Home)
table(df$Parental_Education_Level)
table(df$Gender)
table(df$School_Type)
table(df$Peer_Influence)

hist(df$Hours_Studied)

hist(df$Attendance)

plot(df$Hours_Studied, df$Exam_Score)

apply(is.na(df), 2, sum)


# Conversión de variables -------------------------------------------------

# Se convierte laas columnas que poseen 3 categorias, se ordenan según criterio 
inv_parental <- c("Low", "Medium", "High")
df$Parental_Involvement <- factor(df$Parental_Involvement, levels = inv_parental)
df$Access_to_Resources <- factor(df$Access_to_Resources, levels = inv_parental)
df$Motivation_Level <- factor(df$Motivation_Level, levels = inv_parental)
df$Access_to_Resources <- factor(df$Access_to_Resources, levels = inv_parental)
df$Family_Income <- factor(df$Family_Income, levels = inv_parental)
df$Teacher_Quality <- factor(df$Teacher_Quality, levels = inv_parental)

peer_infl <- c("Negative", "Neutral", "Positive")
df$Peer_Influence <- factor(df$Peer_Influence, levels = peer_infl)

paren <- c("College", "High School", "Postgraduate")
df$Parental_Education_Level <- factor(df$Parental_Education_Level, levels = paren)

distance <- c("Near", "Moderate", "Far")
df$Distance_from_Home <- factor(df$Distance_from_Home, levels = distance)


# Se convierten las variable categóricas
df$Extracurricular_Activities <- as.factor(df$Extracurricular_Activities)
df$Internet_Access <- as.factor(df$Internet_Access)
df$School_Type <- as.factor(df$School_Type)
df$Learning_Disabilities <- as.factor(df$Learning_Disabilities)
df$Gender <- as.factor(df$Gender)


# Analisis regresión ------------------------------------------------------


model_lm_2 <- lm(Exam_Score ~ ., data = df)

#se decide determinar la media como corte para separar los puntajes altos y bajos
df <- mutate(df, Nivel = ifelse(Exam_Score < 67, 0, 1))

df2 <- select(df, -Exam_Score)

table(df$Nivel)

# creacion de set de entrenamiento y testeo


set.seed(123)
trainIndex <- createDataPartition(
  y = df2$Nivel, # Variable de Interes
  p = 0.8, # Porcentaje de datos de Entrenamiento
  list = FALSE # No entregue un objeto en forma lista, sino que un vector.
)
# La funcion entrega un vector de FALSE y TRUE en que los valores TRUE representan las filas
# de entrenamiento, y los valores FALSE representan los valores de validacion/prueba

train_set <- df2[trainIndex,] #set de entrenamiento
test_set <- df2[-trainIndex,] # set de testeo
dim(train_set)
dim(test_set)

# Modelo de regresión logistica
model_reg_log <- glm(Nivel~., data = df2)
summary(model_reg_log)


# Predición  --------------------------------------------------------------


m <- predict(model_reg_log, test_set, type = "response")
n <- ifelse(m>0.5, 1, 0)
n <- factor(n, levels = c(0, 1))
test_set$Nivel <- factor(test_set$Nivel, levels = c(0, 1))
confusionMatrix(n, test_set$Nivel)


# Resultados --------------------------------------------------------------

#Reference
#Prediction   0   1
#0 528  22
#1  28 696

# Accuracy : 0.9608

#Sensitivity : 0.9496          
#Specificity : 0.9694          
#Pos Pred Value : 0.9600          
#Neg Pred Value : 0.9613


