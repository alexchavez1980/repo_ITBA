#Instalar/Cargar todas las librerías necesarias.
install.packages("ggplot2") #Para el boxplot
install.packages("ggcorrplot") #Para la matriz de correlaciones
install.packages("stats")
install.packages("MASS")
library(ggplot2) #Para el boxplot
library(tidyverse)
library(ggcorrplot)
library(stats)
library(MASS)
library(dplyr)

#Reseteo de variables en memoria
rm(list=ls()) 

data(swiss)
View(swiss)
dataset_FAD_TP1_Swiss <- swiss
#View(dataset_FAD_TP1_Swiss)
#write.csv2(dataset_FAD_TP1_Swiss,"D:\\Alex\\Estudio\\Esp Ciencia de Datos\\01 Fundamentos de análisis de datos\\dataset_FAD_TP1_Swiss.csv", FALSE)

MatrizCovarianzas <- cov(dataset_FAD_TP1_Swiss)
MatrizCovarianzas

MatrizCorrelaciones <- cor(dataset_FAD_TP1_Swiss)
MatrizCorrelaciones

ggcorrplot(MatrizCorrelaciones) + ggtitle('Matriz de correlaciones')
#Cuanto mas rojo, mas correlación positiva.

head(dataset_FAD_TP1_Swiss)
VarSelSwiss <- dataset_FAD_TP1_Swiss[,c(3,4)]
head(VarSelSwiss)

VarSel_MeansSwiss <- colMeans(VarSelSwiss)
head(VarSelSwiss)

Matriz_Cov_VarSelSwiss <- cov((dataset_FAD_TP1_Swiss[,c(3,4)]))
Matriz_Cov_VarSelSwiss

MahalanobisSwiss <- mahalanobis(VarSelSwiss, VarSel_MeansSwiss, Matriz_Cov_VarSelSwiss)  
MahalanobisSwiss

MahalanobisSwiss <- round (MahalanobisSwiss,5)
View(MahalanobisSwiss)

#Cálculo de covarianza, correlación y d. de Mahalanobis sin el cantón de Ginebra.
dataset_FAD_TP1_Swiss_SinGinebra <- dataset_FAD_TP1_Swiss[-c(45),]
MatrizCovarianzas <- cov(dataset_FAD_TP1_Swiss_SinGinebra)
MatrizCorrelaciones <- cor(dataset_FAD_TP1_Swiss_SinGinebra)
MatrizCorrelaciones
ggcorrplot(MatrizCorrelaciones) + ggtitle('Matriz de correlaciones')
VarSelSwiss <- dataset_FAD_TP1_Swiss_SinGinebra[,c(3,4)]
VarSel_MeansSwiss <- colMeans(VarSelSwiss)
Matriz_Cov_VarSelSwiss <- cov((dataset_FAD_TP1_Swiss_SinGinebra[,c(3,4)]))
Matriz_Cov_VarSelSwiss
MahalanobisSwiss <- mahalanobis(VarSelSwiss, VarSel_MeansSwiss, Matriz_Cov_VarSelSwiss)  
MahalanobisSwiss
MahalanobisSwiss <- round (MahalanobisSwiss,8)
MahalanobisSwiss





#Punto 1b
#Chequeo cuáles registros contienen el valor 999.99
#-----------------------------------------------------
dataset_FAD_TP1 %>% 
  filter(Grasas_sat==999.99|Alcohol==999.99|Calorías==999.99|Sexo==999.99)

dataset_FAD_TP1[dataset_FAD_TP1==999.99]<-NA #Los reemplazo por NA

#Guardo en el disco duro para chequear que el cambio está hecho.
write.csv2(dataset_FAD_TP1,"D:\\Alex\\Estudio\\Esp Ciencia de Datos\\01 Fundamentos de análisis de datos\\dataset_FAD_TP1_01_NA.csv", FALSE)

#Chequeo los datos (resumen estadístico) y la estructura del data set.
str(dataset_FAD_TP1) 
summary(dataset_FAD_TP1)
#-----------------------------------------------------
#Punto 1c
#Gráficos estadísticos sobre la dieta de 173 personas.Variables individuales.
#-----------------------------------------------------
boxplot(dataset_FAD_TP1$Grasas_sat,
        main="Grasas saturadas",
        #sub="Grasas saturadas",
        xlab="Figura 1",
        ylab="% grasas saturadas en el consumo diario",
        #names=label_in_columns,
        col="lightgreen"
        )
jpeg("D:\\Alex\\Estudio\\Esp Ciencia de Datos\\01 Fundamentos de análisis de datos\\TP1_Figura01_Grasas saturadas.JPG")
dev.off()

boxplot(dataset_FAD_TP1$Alcohol,
        main="Consumo de alcohol",
        #sub="Grasas saturadas",
        xlab="Figura 2",
        ylab="Unidades de bebida (1 cada 100/125 ml)",
        #names=label_in_columns,
        col="lightgreen"
        )
jpeg("D:\\Alex\\Estudio\\Esp Ciencia de Datos\\01 Fundamentos de análisis de datos\\TP1_Figura02_Alcohol.JPG")
dev.off()


boxplot(dataset_FAD_TP1$Calorías,
        main="Calorías",
        #sub="Grasas saturadas",
        xlab="Figura 3",
        ylab="Cantidad de calorías",
        #names=label_in_columns,
        col="lightgreen"
        )
jpeg("D:\\Alex\\Estudio\\Esp Ciencia de Datos\\01 Fundamentos de análisis de datos\\TP1_Figura03_Calorías.JPG")
dev.off()


table(dataset_FAD_TP1$Sexo) 
barplot(table(dataset_FAD_TP1$Sexo),
        main="Sexo",
        xlab="Figura 4",
        ylab="Cantidad de personas por sexo"
        )
jpeg("D:\\Alex\\Estudio\\Esp Ciencia de Datos\\01 Fundamentos de análisis de datos\\TP1_Figura04_Sexo.JPG")
dev.off()
#-----------------------------------------------------
#Punto 1d
#Gráficos estadísticos sobre la dieta de 173 personas.
#Variables analizadas con respecto al sexo del individuo.
#-----------------------------------------------------
boxplot(dataset_FAD_TP1$Grasas_sat ~ dataset_FAD_TP1$Sexo, 
        col = "lightblue",
        main = "Grasas saturadas en hombres y mujeres", 
        xlab = "Figura 5", 
        ylab = "% grasas saturadas en el consumo diario", 
        )
jpeg("D:\\Alex\\Estudio\\Esp Ciencia de Datos\\01 Fundamentos de análisis de datos\\TP1_Figura05_Grasas saturadas VS Sexo.JPG")
dev.off()

boxplot(dataset_FAD_TP1$Alcohol ~ dataset_FAD_TP1$Sexo, 
        col = "lightblue",
        main = "Consumo de alcohol en hombres y mujeres", 
        xlab = "Figura 6", 
        ylab = "Unidades de bebida", 
        )
jpeg("D:\\Alex\\Estudio\\Esp Ciencia de Datos\\01 Fundamentos de análisis de datos\\TP1_Figura06_Alcohol VS Sexo.JPG")
dev.off()

boxplot(dataset_FAD_TP1$Calorías ~ dataset_FAD_TP1$Sexo, 
        col = "lightblue",
        main = "Cantidad de calorías en hombres y mujeres", 
        xlab = "Figura 7", 
        ylab = "Unidades de bebida", 
)
jpeg("D:\\Alex\\Estudio\\Esp Ciencia de Datos\\01 Fundamentos de análisis de datos\\TP1_Figura07_Calorías VS Sexo.JPG")
dev.off()
#-----------------------------------------------------
#Punto 1e
#Calorías por categorías
#-----------------------------------------------------
dataset_FAD_TP1$CaloríasxCateg = case_when(dataset_FAD_TP1$Calorías  <=1100 ~ "CATE1",
                                           dataset_FAD_TP1$Calorías  %in%  1100:1700   ~ "CATE2",
                                           dataset_FAD_TP1$Calorías  > 1700 ~ "CATE3")

#El boxplot para las tres variables no muestra en detalle la Categoría 1
boxplot(dataset_FAD_TP1$Alcohol ~ dataset_FAD_TP1$CaloríasxCateg, 
        col = "violet",
        main = "Consumo de alcohol vs categ. de calorías", 
        xlab = "Figura 8", 
        ylab = "Unidades de bebida", 
)
jpeg("D:\\Alex\\Estudio\\Esp Ciencia de Datos\\01 Fundamentos de análisis de datos\\TP1_Figura08_Alcohol VS CaloríasxCategoría.JPG")
dev.off()

#Filtro por aparte para que mejore la gráfica
dataset_FAD_TP1_Filtro <- dataset_FAD_TP1 %>% 
  filter(CaloríasxCateg=="CATE1")

boxplot(dataset_FAD_TP1_Filtro$Alcohol ~ dataset_FAD_TP1_Filtro$CaloríasxCateg, 
        col = "violet",
        main = "Consumo de alcohol vs categ. 1 de calorías", 
        xlab = "Figura 9", 
        ylab = "Unidades de bebida", 
)
jpeg("D:\\Alex\\Estudio\\Esp Ciencia de Datos\\01 Fundamentos de análisis de datos\\TP1_Figura08_Alcohol VS CaloríasxCategoría.JPG")
dev.off()






















#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------




























tamano=nrow(dataset0)
tamano
View(dataset0)
View(dataset0$Fumadores)
View(dataset0$No_fuman)
str(dataset0) #Comando para ver la estructura del data set
summary(dataset0) #comando para ver el resumen estadÃ­stico del data set
#Organizo las dos columnas de manera decreciente
f_ord<-sort(dataset0$Fumadores,decreasing=TRUE)
f_ord
nf_ord<-sort(dataset0$No_fuman,decreasing=TRUE)
nf_ord
#...
#-----------------------------------------------------
#         GraficaciÃ³n
#-----------------------------------------------------
plot(f_ord,type = "p") 
plot(nf_ord,type = "p") 
hist(f_ord, 
     col = "lightgreen", 
     main = "Histograma de fumadores", 
     xlab= "DL (mL/min/mmHg )",
     ylab= "Frecuencia (Cant. de fumadores x intervalo"
     )
hist(nf_ord, 
     col = "lightblue", 
     main = "Histograma de No fumadores", 
     xlab= "DL (mL/min/mmHg )",
     ylab= "Frecuencia (Canti. de fumadores x intervalo"
     )
#Paso adicional: Almacenar en un vector los resultados del histograma
hist_f_ord=hist(f_ord,col="green",probability=TRUE,plot=F)
hist_nf_ord=hist(nf_ord, col="green",probability=TRUE,plot=F)
#La regla de Stugres determina el nÃºmero de intervalos. Se usa en R. PÃ¡gina 42 de la guÃ­a.
#Para modificar el nÃºmero de intervalos: ,breaks=25,


#GrÃ¡fico de tallos/hojas
stem(f_ord,2)
stem(nf_ord,2)
stem(f_ord,nf_ord,2)

#GrÃ¡fico de tallos/hojas espalda con espalda
#stem.leaf(co2)
#stem.leaf.backback(co2[1:234],co2[235:468], show.no.depths=TRUE)
stem.leaf.backback(f_ord,nf_ord, show.no.depths=TRUE)
#-----------------------------------------------------
#        Armado del boxPlot
#-----------------------------------------------------
#Carga de variables a usar
title <- paste("Estudio sobre el efecto del cigarrillo en la", "\n", "capacidad de difusiÃ³n de monoxido de carbono (DL) del pulmÃ³n")
#subtitle <- "SubtÃ­tulo"
label_in_x <- "Grupos de pacientes"
label_in_y <- "Valor de DL en mililitros :P"
label_in_columns <- c("Fumadores", "No fumadores")
color_in_boxes <- c("lightgreen","lightblue")

boxplot(f_ord, nf_ord,
        main=title,
        #sub=subtitle,
        xlab=label_in_x,
        ylab=label_in_y,
        names=label_in_columns,
        col=color_in_boxes
        )

qqnorm(f_ord,main="QQ-plot de Fumadores",xlab="Cuantiles normales", ylab=" Cuantiles muestrales", col="blue")
qqline(f_ord)

qqnorm(nf_ord,main="QQ-plot de No fuman",xlab="Cuantiles normales", ylab=" Cuantiles muestrales", col="brown")
qqline(nf_ord)


shapiro.test(f_ord)
shapiro.test(nf_ord)

f_ord
nf_ord
varianza=var(nf_ord)
varianza

tamano=nrow(dataset0)
df=tamano-1
desv_std=sqrt(df/(df-2))
desv_std
vacadesv_std_pob=desv_std/sqrt()

sd?
  t.test(Alambre1,Alambre2,var.equal=TRUE)
  
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#             De acÃ¡ para abajo es un espacio de pruebas
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------






#-----------------------------------------------------
#-----------------------------------------------------
#         Lectura del archivo
#-----------------------------------------------------
#-----------------------------------------------------
dataset0=read.csv2("D:/Alex/Estudio/EspecializaciÃ³n Ciencia de Datos/02 EstadÃ­stica/TP Alex Claudio/FumadoresMOD.csv")
View(dataset0)
View(dataset0$paciente)
View(dataset0$dl)
str(dataset0)
summary(dataset0)



#f_ord<-sort(dataset0$Fumadores,decreasing=TRUE)
#nf_ord<-sort(dataset0$No_fuman,decreasing=TRUE)
paciente_ord<-sort(dataset0$paciente,decreasing=TRUE)
paciente_ord
dl_ord<-sort(dataset0$dl,decreasing=TRUE)
dl_ord


#-----------------------------------------------------
#-----------------------------------------------------
#         GraficaciÃ³n
#-----------------------------------------------------
#-----------------------------------------------------
plot(f_ord,type = "p") 
plot(nf_ord,type = "p") 
hist(f_ord, col = "lightsalmon1", main = "Histograma de fumadores")
hist(nf_ord, col = "lightsalmon1", main = "Histograma de No fumadores")
#Te da como resultado los valores del histograma
hist_f_ord=hist(f_ord,col="green",probability=TRUE,plot=F)
hist_nf_ord=hist(nf_ord, col="green",probability=TRUE,plot=F)

boxplot(paciente_ord~dl_ord)
stem(dl_ord,2)
stem(paciente_ord,2)


boxplot(Pacientef~DL = "light blue", main = "Fumadores y no fumadores", xlab = "F y no F", ylab = "Capacidad de difusiÃ³n de monoxido de carbono (DL) del pulmÃ³n")




#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#                 AREA DE PRUEBAS
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------

#Carga del archivo
ds0=read.csv2("C:/Users/chavezalex/Desktop/Curso Data Mining UTN/TP UTN/banco0.csv")
#ds_internet=read.csv2("https://drive.google.com/drive/folders/1X09VMY3eSbkLdB1-Vd78Jp3F2ysZ8QUg/banco_alex_gerardo.xlxs")
#View(ds0)

#Prueba con vectores
vector_1 <- c(44,66,23,37,4,51,-8,-19,15,73,89,95)
vector_2 <- c(-20,-10,0,10,20,30,40,50,60,70,80,90,100)
vector_3 <- c(1:100)
vector_4 <- c()

longvector_1 <- length(vector_1)
longvector_2 <- length(vector_2)
longvector_3 <- length(vector_3)

for (i in 1:longvector_1)
  {
    for (j in 1:longvector_2)
      {
        if(vector_1[i] >= vector_2[j] & vector_1[i] <= vector_2[j+1])
          {vector_4[i] <- vector_2[j]/10}        }  
  }
View(vector_4)


#Columna balance dividida en rangos
#rangos_balance <- 
#  c(-10,-5,0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105)*1000
#rangos_balance <- 
#  c(-10,0,10,20,30,40,50,60,70,80,90,100,110)*1000
rangos_balance <- 
  c(-10,-5,0,10,20,30,40,50,110)*1000
longrangos_balance <- length(rangos_balance)
for (i in 1:columnas_ds0)
  {
    for (j in 1:longrangos_balance)
      {
        if(ds0$balance[i] >= rangos_balance[j] & ds0$balance[i] <= rangos_balance[j+1])
          {ds0$balancexrangos[i] <- rangos_balance[j]}         
      }
  }


#Prueba de grÃ¡ficos
ggplot(data = ds_num, aes(x = deuda, balance_binario, color = y))+
 geom_point(alpha=0.75)+
 labs(title = "Lo que primero vamos viendo del dataset")+
 theme(legend.position = 'none')+
 facet_wrap(~y)

plot(ds_num$balance,type = "p") 
hist(ds_num$balance, col = "lightsalmon1", main = "Histograma")
table(ds_num$balance, ds_num$y_num)
table(ds_num$deuda, ds_num$balance_binario)
table(ds_num$categlaboral_num, ds_num$estadocivil_num)


#Prueba de funciones
grafica2var <- function(ds_num,edad,categlaboral)

ggplot(data = ds_num, aes(x = edad, categlaboral, color = y))+
 geom_point(alpha=0.75)+
 labs(title = "Lo que primero vamos viendo del dataset")+
 theme(legend.position = 'none')+
 facet_wrap(~y)

#FunciÃ³n para graficar
grafica2var <- function(data_set,var1,var2)
  {
    ggplot(data = data_set, aes(x = var1, var2, color = y))+
    geom_point(alpha=0.75)+
    labs(title = "PRUEBA DE FUNCION")+
    theme(legend.position = 'none')+
    facet_wrap(~y)
  }

#Primer modelo. con el dataset original
row_datasetoriginal=nrow(datasetoriginal)
set.seed(123)
train <- sample(row_datasetoriginal, row_datasetoriginal*.8)
dso_train = datasetoriginal[train,]
dso_test = datasetoriginal[-train,]
mlogistic1=glm(y~.,data=dso_train,family=binomial)
summary(mlogistic1)

#Grabado de histogramas
save_img_hist <- "C:/Users/chavezalex/Desktop/Curso Data Mining UTN/TP UTN/Histograma_edad.PNG"
png(save_img_hist)
hist(ds_num$deuda_num, col = "lightsalmon1", main = "Histograma")
dev.off()

























#----------------------------------------------------------------
#TERCER MODELO(DEFINITIVO), SIN 11 VARIABLES
#----------------------------------------------------------------
ds_m3_num <- ds_m0_num[-c(1,2,3,4,5,6,7,10,11,12,14,16,17,18)]
View(ds_m3_num)
row_ds_m3_num <- nrow(ds_m3_num)
set.seed(123)
entrenamiento <- sample(row_ds_m3_num, row_ds_m3_num*.8)
ds_m3_num_train <- ds_m3_num[entrenamiento,]
ds_m3_num_test <- ds_m3_num[-entrenamiento,]
mlogistic3 <- glm(y~.,data=ds_m3_num_train,family=binomial)
#Informativo: extraigo los coeficientes del modelo
coef(mlogistic3)
summary(mlogistic3)
#----------------------------------------------------------------
#EVALUACI?N DEL MODELO DEFINITIVO
#----------------------------------------------------------------
#Intervalo de confianza
#confint(mlogistic3)

#Prueba de predicci?n:
pred_mlogistic3 <- predict(mlogistic3, type = 'response', newdata = ds_m3_num_test)
pred_mlogistic3[1:30]

#La normalizo entre 0 y 1
pred_norm_mlogistic3 <- ifelse(pred_mlogistic3 > 0.5, 1, 0)
pred_norm_mlogistic3[1:30]

#Armo la matriz de confusi?n
mc <- table(ds_m3_num_test$y, pred_norm_mlogistic3,
                          dnn = c("observaciones", "predicciones"))
mc
#mosaic(mc, shade = T, colorize = T,
#       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

VerPos <- mc[2,2]
VerNeg <- mc[1,1]
FalPos <- mc[2,1]
FalNeg <- mc[1,2]
VerPos
VerNeg
FalPos
FalNeg

#----------------------------------------------------------------
#EVALUACI?N DE LA MATRIZ DE CONFUSI?N
#----------------------------------------------------------------
#Acurracy | ?que % de la data clasifica correctamente?
exactitud <- (VerPos+VerNeg)/(VerPos+VerNeg+FalPos+FalNeg)

#Misclassification Rate | ?que % de la data clasifica incorrectamente?
tasa_de_error <- (FalPos+FalNeg)/(VerPos+VerNeg+FalPos+FalNeg)

#Recall/Sensitivity/True Positive Rate
#Cuando la clase es positiva, ?que porcentage logra clasificar?
sensibilidad <- (VerPos)/(VerPos+VerNeg)

#Especificity/True Negative Rate
#Cuando la clase es negativa, ?que porcentage logra clasificar?
especificidad <- (VerNeg)/(FalPos+FalNeg)

eval_mc <- data.frame(exactitud,tasa_de_error,sensibilidad,especificidad)
eval_mc