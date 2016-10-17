

# Load the iris dataset.
data(iris)

# Plot #1: Basic scatterplot matrix of the four measurements
pairs(~A+B+C+D, data=iris)

library(ggplot2)
ggplot(iris,boxplot())
K<-c(iris$Sepal.Width,iris$Petal.Length,iris$Petal.Width)
ggplot(iris,aes(y=K,x=iris$Sepal.Length))+geom_boxplot()
boxplot()
A<-ggplot(iris,aes(y=Sepal.Length,x=Species))+geom_boxplot()
B<-ggplot(iris,aes(y=iris$Sepal.Width,x=Species))+geom_boxplot()
C<-ggplot(iris,aes(y=iris$Petal.Length,x=Species))+geom_boxplot()
D<-ggplot(iris,aes(y=iris$Petal.Width,x=Species))+geom_boxplot()

K<-ggplot(iris, aes())
