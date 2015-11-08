#------------------------------------------------------------
# Метрична классифікація  iris
#	
#	M.Botsula botsula@gmail.com
# 	21.10.2015
#------------------------------------------------------------


# можна побудувати точки залежності параметру 'Sepal.Width' від 'Sepal.Length'
plot(iris$Sepal.Length ~ iris$Sepal.Width)

# застосуємо функцію rainbow() для розфарбування точок в залежності від класу 'Species'
plot(iris$Sepal.Length ~ iris$Sepal.Width, col=rainbow(3)[iris$Species], pch=16)

# для фрейму даних можна побудувати матрицю кореляції - всі можливі комбінації графіків
plot(iris,col=rainbow(3)[unclass(iris$Species)], pch=16)

#------------------------------------------------------------
# Досліджуємо масив даних iris3
#------------------------------------------------------------

# Формуємо навчальну вибірку як випадкові 50 значень з усіх 3-х класів
# http://stackoverflow.com/a/8273355/1717605
 trainIris<-iris[sample(nrow(iris), 50),] 

# Формуємо вектор класів
# Перетворюємо текстові значення класів в порядкові номери unclass()
# Цей номер буде визначати "висоту" класу
trainIrisCl <- unclass(trainIris$Species)

# Будуємо графік навчальної вибірки (дані з відомою класіфікацією)
# Виводимо залежність Sepal.Length ~ Petal.Lenght (1-ий та 3-ій стовпці даних)
trainY <- trainIris[,2]    # trainY <- trainIris$Sepal.Length
trainX <- trainIris[,3]    # trainY <- trainIris$Petal.Length 

plot(trainY~trainX, col=rainbow(3)[trainIrisCl], pch=16)

#train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
#test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])

# train <- cbind(trainIris$Sepal.Length, trainIris$Sepal.Width, trainIris$Petal.Length, trainIris$Petal.Width)
# test <- cbind(testIris$Sepal.Length, testIris$Sepal.Width, testIris$Petal.Length, testIris$Petal.Width)
 

# Формуємо тестову вибірку 100х100, яку треба класифікувати за зразком 
# навчальної вибірки
lenNew = 100
# ДЛя класифікації застосуємо метод найближчих сусідів за k_NN сусідніх точок 
k_NN = 3

# формуємо рівномірний масив точок
# після навчаня кожна точка буде класифікована

# length(trainIrisCl)
# length(t(trainIris[,2:3]))

x1<-seq(min(trainY), max(trainY), len=lenNew)  
x2<-seq(min(trainX), max(trainX), len=lenNew) 
x1.new<-rep(x1, lenNew) 
x2.new<-rep(x2, rep(lenNew, lenNew)) 



# Классифікатор
library(class)

rezKNN <- knn(cbind(trainY, trainX),
              cl = trainIrisCl, 
              test = cbind(x1.new, x2.new), 
              k = k_NN, 
              prob=F)

# Масив prob буде містити частку класа для кожної точки (x1.new, x2.new)
# prob <- attr(rezKNN, "prob")

# Масив pt.col буде містити колір класа для кожної точки (x1.new, x2.new)
pt.col <- rainbow(3)[rezKNN]

#prob <- ifelse(rezKNN == "1", prob, 1-prob)
# drawing points on the plot
#plot(x1.new, x2.new, col=pt.col, pch=20, cex=.1)
#points(x1.new~x2.new, col=pt.col, pch=20, cex=.1)

# Будуємо границі визначених классів за навчаним масивом точок
# Для ціого потрібно задати до кожного класу свою "висоту", тоді 
# алгоритм побудови контурів зможе обробити таку "матрицю висот"

require(grDevices) # for colours
image(x2, x1, matrix(c(rezKNN),ncol=lenNew,nrow=lenNew,byrow = T), col = rainbow(3, alpha = 0.2))
points(trainY~trainX, col=rainbow(3)[trainIrisCl], pch=16)
contour(x2, x1,
        matrix(c(rezKNN),ncol=lenNew,nrow=lenNew,byrow = T),
        add = T,
        nlevels = 3,
        levels = c(1,2,3),
        lty = 1,
        lwd = 2,
        drawlabel=T)


