#############################################################
# Data Mining
# Computer Lab Session n°3:
# Mining Data with R: Basics
#############################################################

#################################
# First Steps with Iris Dataset #
#################################

data("iris") 
mydata <- iris[1:4] 
class <- as.matrix(iris[5])

####################
# Data Description #
####################

# Statistics
summary(iris)
help(iris)

cor(iris[1:4])
# The most correlated attributes are Petal.Length and Petal.Width (96%)

# Visualization
pairs(iris[1:4], pch=21,
      bg = c("red", "green3","blue")[unclass(iris$Species)])

# PCA
# 1) PCA with prcomp function
iris.pca <- prcomp(iris[1:4])
iris.pca
# The attribute which is the best represented by the first component is
# Petal.Length (0.86 on the first component PC1)

summary(iris.pca)
# The reduced representation space (in 2D) is a good representation
# of the four attributes of the dataset: the cumulative proportion
# with the first component (1D) represents 92% of the dataset variance
# and with the two components (2D) represents 98% of the datset variance


win.graph(800,600,10)
# for plotting the graph on a new window (on Windows OS)
# for Linux users, use  x11() or dev.new()
# for Mac OS users, use quartz()

pairs(iris.pca$x, main="PCA Plot" ,font.main=4, pch=19)
win.graph(800,600,10)
plot(iris.pca$x, main="PCA Plot" ,font.main=4, pch=19)
win.graph(800,600,10)
plot(iris.pca$x, main="PCA Plot" ,font.main=4, pch=21,
      bg = c("red", "green3", "blue")[unclass(iris$Species)])
# The 3 iris species are well separated on a 2D representation,
# especially with the first component (on x-axis)

summary(iris.pca) # Reminder: the 1st component represents 92% of the variance

# 2) PCA with princomp function
iris.pca2 <- princomp(iris[1:4])
iris.pca2

win.graph(800,600,10)
biplot(iris.pca2)
# The attribute which is the best represented in the plot is Petal.Length 
# (it is the largest arrow), this arrow is nearly parallel to x-axis and
# covers the Petal.Width arrow (due to the high correlation between the 2 attributes) 
# We can easily find 2 groups in iris dataset
# The class "setosa" is easily separated from the two others (the cluster
# composed by "versicolor" and "virginica" classes) 
# We expect that the results obtained by applying clustering and classification
# methods will not be bad, especially for predicting the iris from setosa class
summary(iris.pca2)

##############
# Clustering #
##############
iris.zs <- iris
for (i in 1:4) 
  {
  iris.zs[i] <- (iris.zs[i] - sapply(iris.zs[i],mean)
                 / sapply(iris.zs[i],sd))
  }

cl <- kmeans(iris[1:4], 3)
print(cl)
plot(iris[3:4], col = cl$cluster)

# With the standardized values
cl2 <- kmeans(iris.zs[1:4], 3)
print(cl2)
plot(iris.zs[3:4], col = cl$cluster)
# With the z-score transformation, the results seem to be less stable
# This is due to the effect of the two first attributes (with sepals)
# which can add more noise on the distances computed between the points 

hc <- hclust(dist(iris[1:4]), "ave")
print(hc)
plot(hc)


##################
# Classification #
##################

# Decision Tree

library(rpart)

set.seed(2568) 
n <- nrow(iris) 
train <- sort(sample(1:n, floor(n/2)))
iris.train <- iris[train,] 
iris.test <- iris[-train,]

iris.rp <-rpart(class ~ . ,   
                data = iris[1:4],  # Don't take the 5th attribute (the class) for learing the model 
                subset = train, 
                method = "class",
                parms = list(split = "information"), 
                maxsurrogate = 0, 
                cp = 0, 
                minsplit = 5,
                minbucket = 2,
)

help(rpart)
summary(iris.rp)

win.graph(800, 600, 10)
plot(iris.rp,
     uniform=TRUE,
     compress=TRUE,
     margin = .2)
text(iris.rp,
     use.n=TRUE,
     all = TRUE,
     fancy = TRUE)

# The attributes used for the prediction are:
# - Petal.Length: mostly for separating the setosa class from the others)
# - Petal.Width: for separating the two other classes
# These results are consistent with the previous conclusion
# obtained with the PCA and the clustering

# To test the decision tree model:
pred.rp <- predict(iris.rp,
                   newdata = iris[-train,],
                   type = "class")
pred.rp

predict(iris.rp,
        newdata = iris[-train,],
        type = "prob")
# for example, with the probabilities, a few examples are predicted not with 1
# but 0.25 / 0.75 for versicolor or virginica

predict(iris.rp,
        newdata = iris[-train,],
        type = "vector")
predict(iris.rp,
        newdata = iris[-train,],
        type = "matrix")

table(class[-train], pred.rp)
# The cross table shows a few errors (e.g., 2 examples are predicted virginica
# but are versicolor), the setosa class is predicted without error


# k-NN

library(class)

data(iris3)
train <- rbind(iris3[1:25,,1],iris3[1:25,,2],iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("Setosa",25), rep("Versicolor",25), rep("Virginica",25)))
pred<-knn(train, test, cl, k = 3)
table(pred,cl)

# They are more errors with the k-NN: with some prediction errors for
# the versicolor and the virgina classes
# The class easy to predict is the same: setosa


#######################
# Overfitting Problem #
#######################

rm(list=ls())
x <- 1:10
y <- x + c(-0.5, 0.5)

plot(x,y)

# Models
model1 <- lm(y~x) # Simple linear regression
lines(x, predict(model1, data.frame(x)), lty=1, col="blue")

model2 <- lm(y~poly(x,3)) # Polynomial regression of order 3
lines(x, predict(model2, data.frame(x)), lty=1, col="green")

model3 <- lm(y~poly(x,9)) # Polynomial regression of order 9
lines(x, predict(model3, data.frame(x)), lty=1, col="red")



#################################################################
# Data Understanding, Data Preparation, Modeling and Evaluation #
#################################################################

wisc <- read.csv("~/R/data/breast-cancer-wisconsin.data", header=F, quote="")
summary(wisc)

cor(wisc[2:10])
# Does not work...

str(wisc[2:10])
# Read carrefully the file "names". Section 8, you will see:
# 8. Missing attribute values: 16
# 
# There are 16 instances in Groups 1 to 6 that contain a single missing 
# (i.e., unavailable) attribute value, now denoted by "?".  

n <- nrow(wisc)
nb_at <- ncol(wisc)
p <- nb_at - 1
wisc.ok <- wisc

for (i in 1:n)
  for (j in 1:p)
    if (wisc[i,j]=="?")
      wisc.ok[i,j] <- NA

wisc.ok[1:p] <- lapply(wisc.ok[1:p], FUN = as.numeric) 
str(wisc.ok)
cor(wisc.ok[2:p])

# Whenever we are handling a dataset with missing values, 
# we can follow several strategies. The most common are:
# - Remove the cases with unknowns.
# - Fill in the unknown values by exploring the correlations between variables.
# - Fill in the unknown values by exploring the similarity between cases.
# - Use tools that are able to handle these values.

wisc.ok <- na.exclude(wisc.ok)
str(wisc.ok)
cor(wisc.ok[2:10])
summary(wisc.ok)
class(wisc.ok)

n.new <- nrow(wisc.ok)
for (i in 1:n.new){
  if (wisc.ok[i,nb_at]==2)
  {wisc.ok[i,nb_at] <- "begnin"}
  else 
  {wisc.ok[i,nb_at] <- "malignant"}
}


colnames(wisc.ok) <- c("ID", "Clump_Thickness", "Uniformity_Cell_Size",
                       "Uniformity_Cell_Shape", "Marginal_Adhesion",
                       "Single_Epithelial_Cell_Size", "Bare_Nuclei",
                       "Bland_Chromatin", "Normal_Nucleoli", "Mitoses", "Class")

wisc.ok[,1] <- NULL
head(wisc.ok)
n <- nrow(wisc.ok)
nb_at <- ncol(wisc.ok)
p <- nb_at - 1


write.csv(wisc.ok, file="wisconsin-breast-cancer.csv",row.names=TRUE)


library(rpart)

set.seed(2568) 
n <- nrow(wisc.ok) 
train <- sort(sample(1:n, floor(n/2)))
wisc.train <- wisc.ok[train,] 
wisc.test <- wisc.ok[-train,]

wisc.rp <-rpart(wisc.ok$Class ~ . ,   
                data = wisc.ok[1:p], 
                subset = train, 
                method = "class",
                parms = list(split = "information"), 
                maxsurrogate = 0, 
                cp = 0, 
                minsplit = 5,
                minbucket = 2,
)

summary(wisc.rp)

win.graph(800, 600, 10)
plot(wisc.rp,
     uniform=TRUE,
     compress=TRUE,
     margin = .2)
text(wisc.rp,
     use.n=TRUE,
     all = TRUE,
     fancy = TRUE)

pred.rp <- predict(wisc.rp,
                   newdata = wisc.ok[-train,],
                   type = "class")
pred.rp

predict(wisc.rp,
        newdata = wisc.ok[-train,],
        type = "prob")
predict(wisc.rp,
        newdata = wisc.ok[-train,],
        type = "vector")
predict(wisc.rp,
        newdata = wisc.ok[-train,],
        type = "matrix")

table(wisc.ok$Class[-train], pred.rp)
