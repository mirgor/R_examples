#------------------------------------------------------------
# Логічна классифікація  iris (Regression Tree)
#	
#	M.Botsula botsula@gmail.com
# 	21.10.2015
#------------------------------------------------------------


# можна побудувати точки залежності параметру 'Sepal.Width' від 'Sepal.Length'
plot(iris$Petal.Length ~ iris$Petal.Width)

# застосуємо функцію rainbow() для розфарбування точок в залежності від класу 'Species'
plot(iris$Petal.Length ~ iris$Petal.Width, col=rainbow(3)[iris$Species], pch=16)

# для фрейму даних можна побудувати матрицю кореляції - всі можливі комбінації графіків
plot(iris,col=rainbow(3)[unclass(iris$Species)], pch=16)

#------------------------------------------------------------
# Досліджуємо масив даних iris3
#------------------------------------------------------------

# Формуємо навчальну вибірку як випадкові 50 значень з усіх 3-х класів
# http://stackoverflow.com/a/8273355/1717605
 trainIris<-iris[sample(nrow(iris), 150),] 

# Формуємо вектор класів
# Перетворюємо текстові значення класів в порядкові номери unclass()
# Цей номер буде визначати "висоту" класу
# trainIrisCl <- unclass(trainIris$Species)
trainIrisCl <- trainIris$Species

# Будуємо графік навчальної вибірки (дані з відомою класіфікацією)
# Виводимо залежність Sepal.Length ~ Petal.Lenght (1-ий та 3-ій стовпці даних)
trainSL <- trainIris[,1]    # trainY <- trainIris$Sepal.Length
trainSW <- trainIris[,2]    # trainY <- trainIris$Sepal.Width
trainPL <- trainIris[,3]    # trainY <- trainIris$Petal.Length 
trainPW <- trainIris[,4]    # trainY <- trainIris$Petal.Width

op <- par(mfrow = c(2, 2), # 2 x 2 графіки на одній картинці
          pty = "m")       # квадратні (square) області побудови,
                           # незалежно від розміру екрану

plot(trainIrisCl~trainSL + trainSW + trainPL + trainPW, col=rainbow(3)[trainIrisCl], pch=16)

par(op)

library("rpart") # Recursive Partitioning and Regression Trees
library("rpart.plot")

# Побудова дерева класифікації 
#
# fit <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
#             method="class", data=trainIris)
#

fit <- rpart(Species ~ ., 
             method="class", data=trainIris)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# Побудова дерева
# plot(fit, uniform=TRUE, 
#     main="Regression Tree for Iris detect ")
# text(fit, use.n = TRUE, all=TRUE, cex=.8)

rpart.plot(fit, main="Дерево регресії \n для визначення класів Iris")

# "Обрізка" дерева 
pfit<- prune(fit, cp=0.01160389) # from cptable   

# plot the pruned tree 
rpart.plot(fit, main="Обрізане дерево регресії \n для визначення класів Iris")

