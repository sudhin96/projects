setwd("C://Users//user//Desktop//r")
View(iris)
head(iris)
colnames(iris)
install.packages("ggvis")
library(ggvis)

#Iris scatter plot

iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill= ~Species) %>% layer_points()
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill= ~Species) %>% layer_points()

#graph indicates a positive correlation between the petal length and
#the petal width for all different species that are included into the Iris data
#so probably need to test this hypothesis a bit further if you want to be really sure of this:

cor(iris$Petal.Length,iris$Petal.Width)
x=levels(iris$Species)

#print setosa correlation
print(x[1])
cor(iris[iris$Species==x[1],1:4])                 #-1<0<1 are the range of correlation,
cor(iris[iris$Species==x[2],1:4])                  #near to the one is the highest correlation
cor(iris[iris$Species==x[3],1:4])
table(iris$Species)
round(prop.table(table(iris$Species))*100,digits = 1)

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
iris_norm=as.data.frame(lapply(iris[1:4],normalize))
set.seed(1234)
   

#constructing model train & test
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

iris.training=iris[ind==1,1:4]
head(iris.training)
iris.test=iris[ind==2,1:4]
head(iris.test)

#consider train labels
iris.trainLabels=iris[ind==1,5]
iris.testLabels=iris[ind==2,5]
print(iris.testLabels)
print(iris.trainLabels)


#creating knn model
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
# Put `iris.testLabels` in a data frame
irisTestLabels <- data.frame(iris.testLabels)

# Merge `iris_pred` and `iris.testLabels` 
merge <- data.frame(iris_pred, iris.testLabels)

# Specify column names for `merge`
names(merge) <- c("Predicted Species", "Observed Species")

# Inspect `merge` 
merge
