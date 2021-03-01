# Vamos a trabajar con un ejemplo de SVM para clasificar 
# los datos de iris de forma LINEAL


#carguemos la librería necesaria
library(e1071)
#carguemos iris
data(iris)

View(iris)
#carguemos una semilla para replicar todo lo que hagamos
set.seed(42)
#dividvamos entre el training set y el test set
iris[,"train"] <- ifelse(runif(nrow(iris))<0.8,1,0)

iris[,"train"]
     
#hagamos la separacion
trainset <- iris[iris$train==1,]
testset <- iris[iris$train==0,]

trainset
testset

########## SIN BINARIZACION

index <- c(1:nrow(iris))
test.index <- sample(index, size = (length(index)/3))
trainset <- iris[-test.index ,]
testset <- iris[test.index ,]

############

#obten un indice para cada variable dentro de train
trainColNum <- grep("train",names(trainset))
# remueve las columnas flag de trainset y testset
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]
# obten la columna indice para las predicciones
typeColNum <- grep("Species",names(iris))

View(typeColNum)



# construye el modelo linear kernel con una C-classification (soft margin)
# con un costo de penalizacion default (C=1)
svm_model <- svm(Species~ ., data=trainset, method="C-classification", kernel="linear")

# Con tune para iterar al mejor modelo

svm_model <- tune(svm,Species~ ., data=trainset, kernel="linear", cost=0.1, factor=F)

svm_model$best.model


# entrenar el conjunto para predicción
pred_train <-predict(svm_model,trainset)
mean(pred_train==trainset$Species)
# probando el conjunto de predicción
pred_test <-predict(svm_model,testset)
mean(pred_test==testset$Species)

head(trainset)

# grafiquemos

head(trainset)

plot(svm_model,data = trainset, Sepal.Length ~ Sepal.Width)

plot(svm_model,data = trainset, Sepal.Length ~ Petal.Length)


################### 2 Ejercicio

library(mlbench)
#cargar el conjunto de datos
data(Sonar)
# cargar una semilla
set.seed(42)
#dividir entre el conjunto prueba y el training
Sonar[,"train"] <- ifelse(runif(nrow(Sonar))<0.8,1,0)
# separación completa
trainset <- Sonar[Sonar$train==1,]
testset <- Sonar[Sonar$train==0,]
# crear columna indice
trainColNum <- grep("train",names(trainset))

trainColNum

# remover indices
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]
# obtener el numero de variables categoricas
typeColNum <- grep("Class",names(Sonar))
# correr la SVM
svm_model <- svm(Class~ ., data=trainset, method="C-classification", kernel="linear")
# prediccion
pred_train <-predict(svm_model,trainset)

# grafica

head(trainset)

plot(svm_model,data = trainset, V1 ~ V2)

plot(svm_model,data = trainset, V10 ~ V20)

plot(svm_model,data = trainset, V50 ~ V60)
