#------------------------------------------------------------
# 	Досліджуємо фрейм даних iris
#	
#	M.Botsula botsula@gmail.com
# 	01.10.2015
#------------------------------------------------------------
# iris - це фрейм даних
help(iris)
library('xlsx')
#Збереження до файлу txt
write.table(iris,"iris.txt",sep = "\t")
write.csv2(iris,"iris.csv")
myIris <- read.csv2("iris.csv") 
myIris <- read.xlsx("iris.xlsx",1)

# Атроибути фрейма
attributes(iris)

# Звертатись до даних можна за назвою:
iris$Species # повертає вектор-колонку 'Species'

# можна побудувати точки залежності параметру 'Sepal.Width' від 'Sepal.Length'
plot(iris$Sepal.Length ~ iris$Sepal.Width)

# застосуємо функцію rainbow() для розфарбування точок в залежності від класу 'Species'
plot(iris$Sepal.Length ~ iris$Sepal.Width, col=rainbow(3)[iris$Species],pch=16)

# для фрейму даних можна побудувати матрицю кореляції - всі можливі комбінації графіків
plot(iris,col=rainbow(3)[unclass(iris$Species)])

#------------------------------------------------------------
# Досліджуємо масив даних iris3
#------------------------------------------------------------

# iris3 - це масив з розмірністю 50х4х3 (див. help(iris3))
# "Sepal L." "Sepal W." "Petal L." "Petal W."
# Сорта квітів: "Setosa"     "Versicolor" "Virginica" 
help(iris3)


# кількість елементів в масиві
length(iris3)

# атрибути массиву
attributes(iris3)

# Це перелік назв класів квітки, що заданий в масиві
attr(iris3,"dimnames")[[3]] 


# Звернення до останнього елементу масиву:
iris3[50,4,3]
iris3[50,4,'Virginica']
iris3[50,'Petal W.','Virginica']


# Побудова 2D 
# x = Sepal.Length, y = Sepal.Width, z = Species
labelX = 'Sepal W.'
labelY = 'Sepal L.'
labelZ = 'Petal L.'

x<-c(iris3[,labelX,1],iris3[,labelX,2],iris3[,labelX,3])
y<-c(iris3[,labelY,1],iris3[,labelY,2],iris3[,labelY,3])

# Відповідний вектор класів квітки
classIris<-c(rep(1, 50),rep(2, 50),rep(3, 50) )

# Будуємо графік
plot(y~x, col=rainbow(3)[classIris], xlab = labelX, ylab = labelY, 
     main = 'Квіти ірису', pch = 16)

# Додамо легенду
legend(x='topright', c("Setosa","Versicolor","Virginica"), 
       col = rainbow(3),
       pch = 16,
       merge = TRUE, 
       bg = "white")

# Побудова 3D 
require(rgl) 
plot3d(x,y,classIris, type="p", col=rainbow(3)[classIris], 
       xlab="Sepal.Length", 
       ylab="Sepal.Width", 
       zlab="Species", site=5, lwd=15)

z <- c(iris3[,labelZ,1],iris3[,labelZ,2],iris3[,labelZ,3])
plot3d(x,y,z, type="p", col=rainbow(3)[classIris], 
       xlab = labelX, 
       ylab = labelY, 
       zlab = labelZ, site=5, lwd=15)

