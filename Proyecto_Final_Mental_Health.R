## PROYECTO DE MENTAL HEALTH ##
## 12/5/2025 ##

## Elimino environment
rm(list = ls())

## Llamo a mi directorio de trabaho
setwd("C:/Users/peio1/Dropbox/Pedro Elizalde/DATA SCIENCE/R/Archivos de excel para Data Frame")

## Importamos librerias necesarias
library(tidyverse)
library(readxl)
library(reshape2)
library(ggplot2)
library(corrplot)
library(forcats)
library(dplyr)
library(nnet)
library(Hmisc)
library(randomForest)
library(caret)
library(cluster)

## Importamos el data set
mental_health= read_excel("Mental_Health_Lifestyle_Dataset.xlsx")

## Miramos informacion general
str(mental_health)
summary(mental_health)
table(mental_health$Country) #nos da que evaluamos 7 paises repartidos equitativamente
dim(mental_health)
sum(is.na(mental_health))
colSums(is.na(mental_health))

## Pasamos algunas variables de character a factor y a numerico
mental_health$Country<-as.factor(mental_health$Country)
levels(mental_health$Country)

mental_health$Gender<-as.factor(mental_health$Gender)
levels(mental_health$Gender)

mental_health$"Exercise Level"<-as.factor(mental_health$"Exercise Level")
levels(mental_health$"Exercise Level")

mental_health$"Diet Type"<-as.factor(mental_health$"Diet Type")
levels(mental_health$"Diet Type")

mental_health$"Sleep Hours"<-as.numeric(mental_health$"Sleep Hours")
summary(mental_health$`Sleep Hours`)

mental_health$"Stress Level"<-as.factor(mental_health$"Stress Level")
levels(mental_health$"Stress Level")

mental_health$"Mental Health Condition"<-as.factor(mental_health$"Mental Health Condition")
levels(mental_health$"Mental Health Condition")

mental_health$`Screen Time per Day (Hours)`<-as.numeric(mental_health$`Screen Time per Day (Hours)`)
summary(mental_health$`Screen Time per Day (Hours)`)

mental_health$`Social Interaction Score`<-as.numeric(mental_health$`Social Interaction Score`)
summary(mental_health$`Social Interaction Score`)

mental_health$`Happiness Score`<-as.numeric(mental_health$`Happiness Score`)
summary(mental_health$`Happiness Score`)

## Miramos estructura del dataset
str(mental_health)

# ordenamos los niveles del estres para que estén de manera correcta cuando lo pasemos a numerico
mental_health$`Stress Level` <- factor(
  mental_health$`Stress Level`,
  levels = c("Low", "Moderate", "High"),
  ordered = TRUE
)

## Sacamos desviacion standar
sd(mental_health$`Happiness Score`)

## Hacemos la correlacion de las variables numéricas
vars_cor <- mental_health[, c(
  'Age',
  'Sleep Hours',
  'Work Hours per Week',
  'Screen Time per Day (Hours)',
  'Social Interaction Score',
  'Happiness Score'
)]

cor_matrix <- cor(vars_cor, use = "complete.obs", method = "spearman")
rcorr(as.matrix(vars_cor), type = "spearman")

# Paleta de colores de rojo a blanco a azul (bajo a alto) para el heatmap
colores <- colorRampPalette(c("darkred", "white", "darkblue"))(200)

# Heatmap de correlaciones entre 6 variables numericas
corrplot(cor_matrix,
         method = "square",           # Cuadrados completos
         col = colores,               # Paleta personalizada
         type = "full",               # Toda la matriz (no solo triangular)
         addCoef.col = "black",       # Coeficientes en negro dentro de cada cuadrado
         number.cex = 0.8,            # Tamaño de los coeficientes
         tl.col = "black",            # Texto de etiquetas en negro
         tl.cex = 0.9,                # Tamaño de texto de las etiquetas
         cl.cex = 0.8,                # Tamaño de la leyenda
         diag = TRUE,                 # Mostrar diagonal
         mar = c(1, 1, 2, 1),         # Márgenes
         title = "Mapa de Correlación de Variables Numéricas"
)
# Analizando y correlacionando las variables numericas no hay ningun indicador de que haya correlaciones estadisticamente significativas

## Hacemos grafico de barras para visualizar los diferentes niveles de mental health condition
ggplot(mental_health, aes(x = fct_infreq(`Mental Health Condition`), fill = `Mental Health Condition`)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.4, size = 4) +  # etiquetas arriba de cada barra
  labs(
    title = "Condiciones de Salud Mental Reportadas",
    x = "Condición de Salud Mental",
    y = "Cantidad de personas",
    fill = "Condición"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Inclina etiquetas si son largas
    legend.position = "none"  # Quita leyenda si ya está en eje X
  )
# Interpretando el grafico nos damos cuenta de que la distribucion de las condiciones de salud mental es bastante homogenea, sin resaltar alguna en particular.

## Vemos cual es el pais con el menor % y el mayor % de happiness score
# Hacemos el promedio de happines score por pais
promedios_pais <- mental_health %>%
  group_by(Country) %>%
  summarise(
    mean_happiness = mean(`Happiness Score`, na.rm = TRUE)
  )
head(promedios_pais,7)

# Mayor felicidad
promedios_pais[which.max(promedios_pais[["mean_happiness"]]), ]
# Menor felicidad
promedios_pais[which.min(promedios_pais[["mean_happiness"]]), ]
# aca nos indica que Canada tiene el mayor indice promedio de happines score y que Japon el minimo

# Vemos que el minimo de happiness score es 1 y lo tienen 13 filas que incluyen 6 paises menos India
min(mental_health$`Happiness Score`)
which(mental_health$`Happiness Score` == 1)
mental_health[mental_health$`Happiness Score` == 1, c("Country", "Happiness Score")]

##TEST DE HIPOTESIS
#Hipotesis nula (=) no hay diferencia en la media de felicidad entre personas con y sin alguna salud mental
#Hipotesis alternativa (desigual) hay diferencia en la media de felicidad entre personas con y sin salud mental

#Paso 1: creamos dos categorias dentro de condicion mental por si o por no
happiness_dissordes<- mental_health$'Happiness Score'[
  mental_health$'Mental Health Condition' %in% c("Anxiety", "PTSD", "Depression", "Bipolar")
]

# Felicidad de quienes NO tienen condición mental (Mental Health Condition == "None")
happiness_none <- mental_health$'Happiness Score'[
  mental_health$'Mental Health Condition' == "None"]

#Sacamos las medias de happines score con y sin condicion de salud mental
mean_happines_dessordes <-mean(happiness_dissordes)
mean_happines_none<-mean(happiness_none)

#hacemos test de normalidad(usamos shapiro)
shapiro.test(happiness_dissordes)
shapiro.test(happiness_none)
#Si p>a 0.05 aceptamos que hay normalidad y usamos t.test
#si p< a 0.05 no hay normalidad en los datos y usamos wilcox(compara entre dos grupos) o kruskal(si tenemos mas de dos grupos)
#en esta caso los resultados de ambos son < a 0.05 por lo tanto usamos wilkoz o kruskal

wilcox.test(happiness_dissordes,happiness_none)
#W = 703450, p-value = 0.5245, entonces p value es > a 0.05, por lo tanto en este caso no evidencias estadisticamente significativas para rechar la hipotesis nula y decir que no hay diferencias entre la media de felicdad de personas con y sin condicion de salud mental.

##TEST DE HIPOTESIS PARA MEDIA POBLACIONAL(HAPPINESS SCORE SOBRE NIVEL DE ESTRES)
#Test de hipótesis: ¿La felicidad de personas con estrés alto difiere de la media general?
#H0: la media de felicidad con estres alto = media de felicidad con estres moderate

# Filtramos grupo con estrés alto y estres moderado
grupo_alto <- subset(mental_health, `Stress Level` == "High")$`Happiness Score`
grupo_moderado <- subset(mental_health, `Stress Level` == "Moderate")$`Happiness Score`

#Sacamos las medias de las dos grupos que generamos
media_alto <- mean(grupo_alto)
media_moderado<-mean(grupo_moderado)

# Prueba de normalidad (Shapiro)
shapiro.test(grupo_alto)
shapiro.test(grupo_moderado)

#usamos este test ya que el p value me dio menos que 0-05 y por lo tanto no hay normalidad en los datos
wilcox.test(media_alto,media_moderado)
# No hay evidencia estadísticamente significativa de que las personas con nivel de estrés alto tengan una media de felicidad distinta al promedio moderado.

## Modelo de clustering (k-means)
# 1. Selección de variables numéricas
vars <- mental_health[, c("Age", "Sleep Hours", "Happiness Score", "Screen Time per Day (Hours)")]
# 2. Escalar variables
vars_scaled <- scale(vars)

# 3. Elegir número de clusters con método del codo
wss <- numeric(10)
for(k in 1:10) {
  km <- kmeans(vars_scaled, centers = k, nstart = 25, iter.max = 100)
  wss[k] <- km$tot.withinss
}
plot(1:10, wss, type = "b", xlab = "Número de clusters", ylab = "Suma de cuadrados dentro del cluster")
# 4. Elegir k (por ejemplo 8) y hacer clustering
set.seed(123)
km_final <- kmeans(vars_scaled, centers = 8, nstart = 25)
# 5. Añadir cluster al dataset original
mental_health$cluster <- factor(km_final$cluster)
# 6. Explorar perfiles por cluster
aggregate(vars, by = list(Cluster = mental_health$cluster), FUN = mean)

#Elegimos el metodo Silueta
avg_sil <- numeric()

for (k in 2:10) {
  km <- kmeans(vars_scaled, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(vars_scaled))
  avg_sil[k] <- mean(sil[, 3])
}

plot(2:10, avg_sil[2:10], type = "b",
     xlab = "Número de clusters (k)",
     ylab = "Silhouette promedio",
     main = "Método del silhouette")

## Graficamos boxplots de Cluster para cada variable
# Combinar datos originales con cluster
plot_data <- cbind(vars, Cluster = mental_health$cluster)
# Convertir a formato largo
plot_data_long <- melt(plot_data, id.vars = "Cluster")
# Graficar
ggplot(plot_data_long, aes(x = Cluster, y = value, fill = Cluster)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distribución de variables por cluster") +
  theme_minimal()

# Hacemos grafico de dispersion para visualizar bien los diferentes grupos de cluster 
# 1. PCA
pca <- prcomp(vars_scaled)

# 2. Agregar resultados PCA al dataset + cluster
mental_health$pca1 <- pca$x[, 1]
mental_health$pca2 <- pca$x[, 2]

# 3. Graficar
ggplot(mental_health, aes(x = pca1, y = pca2, color = cluster)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Clusters de Salud Mental (PCA + K-means)",
       x = "Componente Principal 1",
       y = "Componente Principal 2",
       color = "Cluster") +
  theme_minimal()

# Resumen de clusters:
# Se identificaron 8 perfiles según edad, horas de sueño, felicidad y uso de pantallas.
# - Grupos jóvenes (clusters 1, 2, 5, 6) muestran contrastes: algunos con bajo sueño y felicidad moderada,
#   otros con alto bienestar y buen descanso.
# - Grupos mayores (clusters 3, 4, 7, 8) también varían: hay perfiles con baja felicidad y mal descanso,
#   y otros con alta satisfacción, buen sueño y uso equilibrado de tecnología.
# - No hay relación lineal entre uso de pantallas y felicidad.
#   Algunos grupos felices tienen alta exposición, lo que sugiere diferencias en el tipo de uso.

## Buscamos indicadores de OUTLIERS(pareciera que en horas de sueños los hay)
ggplot(mental_health, aes(x = "", y = `Happiness.Score`)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot de Felicidad", x = "", y = "Score de Felicidad") +
  theme_minimal()

ggplot(mental_health, aes(x = "", y = `Sleep Hours`)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot de Horas de Sueño", x = "", y = "Horas de sueño") +
  theme_minimal()

max(mental_health$`Sleep Hours`)
min(mental_health$`Sleep Hours`)

max(mental_health$'Happiness Score')
min(mental_health$'Happiness Score')

## Hacer un Random Foret con la variable target con o sin condicion mental de salud
# Renombramos las variables que tenian espacio en el medio con un punto.
colnames(mental_health) <- make.names(colnames(mental_health))

# Crear variable binaria: 0 = sin trastorno, 1 = con trastorno
mental_health$mental_condition_bin <- ifelse(
  mental_health$'Mental.Health.Condition' == "None", 0, 1
)

# Convertir a factor
mental_health$mental_condition_bin <- factor(mental_health$mental_condition_bin, levels = c(0, 1))

str(mental_health)

# Seleccionamos algunas variables predictoras que podrías probar
datos_rf <- mental_health %>%
  select(mental_condition_bin, 
         Age,
         'Sleep.Hours',
         'Work.Hours.per.Week',
         'Screen.Time.per.Day..Hours.',
         'Social.Interaction.Score',
         'Happiness.Score') %>%
  na.omit()

# Divido datos en entrnamiento y testeo
set.seed(123)
n <- nrow(datos_rf)
train_index <- sample(1:n, size = 0.7 * n)

train_data <- datos_rf[train_index, ]
test_data <- datos_rf[-train_index, ]

# Creo modelo de Random Forest
modelo_rf <- randomForest(
  mental_condition_bin ~ .,
  data = train_data,
  ntree = 500,
  mtry = 3,
  importance = TRUE)
print(modelo_rf)
varImpPlot(modelo_rf)

# Hacer predicciones sobre el test_data
predicciones <- predict(modelo_rf, newdata = test_data)

#Crear la matriz de confusión y calcular métricas
conf_matrix <- confusionMatrix(predicciones, test_data$mental_condition_bin, positive = "1")
print(conf_matrix)
# Este modelo esta desbalanceado por lo que no predice bien los sin condicion mental

#Por lo tanto hacemos un balanceo

# Dividir en train y test (80%-20%)
set.seed(123)
train_index <- sample(seq_len(nrow(datos_rf)), size = 0.8 * nrow(datos_rf))
train_data <- datos_rf[train_index, ]
test_data <- datos_rf[-train_index, ]

# Obtener tamaño de la clase minoritaria en train_data
n_min <- min(table(train_data$mental_condition_bin))

# Entrenar Random Forest balanceado con sampsize
modelo_rf_bal <- randomForest(
  mental_condition_bin ~ .,
  data = train_data,
  ntree = 1000,
  mtry = 2,
  importance = TRUE,
  sampsize = c(n_min, n_min)  # balancea cada clase con el tamaño minoritario
)

# Predecir en test_data
pred_rf <- predict(modelo_rf_bal, test_data)

# Crear matriz de confusión
confusion_matrix <- table(Reference = test_data$mental_condition_bin, Prediction = pred_rf)
print(confusion_matrix)

# Calcular métricas: Accuracy, Sensitivity (Recall), Specificity, F1-score
confusion <- confusionMatrix(pred_rf, test_data$mental_condition_bin, positive = "1")
print(confusion)
## Conclusion: el modelo sigue teniendo bajo rendimiento a la hora de predcir sin condicion mental

# PROBAMOS CON MODELO DE XGBOOSt
# Selección de variables
datos_xgb <- mental_health %>%
  select(
    mental_condition_bin,
    Age,
    Sleep.Hours,
    Work.Hours.per.Week,
    Screen.Time.per.Day..Hours.,
    Social.Interaction.Score,
    Happiness.Score
  ) %>%
  na.omit()

# División en train/test
set.seed(123)
train_index <- createDataPartition(datos_xgb$mental_condition_bin, p = 0.7, list = FALSE)
train_data <- datos_xgb[train_index, ]
test_data <- datos_xgb[-train_index, ]

# Convertimos a matrices para xgboost
train_matrix <- xgb.DMatrix(
  data = as.matrix(train_data[, -1]),
  label = as.numeric(as.character(train_data$mental_condition_bin))
)

test_matrix <- xgb.DMatrix(
  data = as.matrix(test_data[, -1]),
  label = as.numeric(as.character(test_data$mental_condition_bin))
)

# Calculamos el peso para balancear
neg <- sum(datos_xgb$mental_condition_bin == 0)
pos <- sum(datos_xgb$mental_condition_bin == 1)
scale_weight <- neg / pos

# Entrenamos el modelo XGBoost con balanceo de clases
modelo_xgb <- xgboost(
  data = train_matrix,
  label = as.numeric(as.character(train_data$mental_condition_bin)),
  max.depth = 4,
  eta = 0.1,
  nround = 100,
  objective = "binary:logistic",
  eval_metric = "error",
  scale_pos_weight = scale_weight,  # 👈 le damos más peso a la clase minoritaria
  verbose = 0
)

# Predicciones
preds_prob <- predict(modelo_xgb, test_matrix)
preds <- ifelse(preds_prob > 0.7, 1, 0)

# Matriz de confusión
conf_matrix <- confusionMatrix(
  factor(preds, levels = c(0, 1)),
  factor(as.numeric(as.character(test_data$mental_condition_bin)), levels = c(0, 1))
)

print(conf_matrix)
