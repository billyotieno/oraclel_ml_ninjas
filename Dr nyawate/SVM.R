#support vector machines
#Support Vector Machines (SVMs) apply a simple linear method to the data but
#in a high-dimensional feature space non-linearly related to the input space.
#Training a SVM for classification, regression or novelty detection involves 
#solving a quadratic optimization problem.
 
#In classification,support vector machines separate the different classes 
#of data by a hyperplane corresponding to the decision function.
#HYPERPLANE is the one with the maximal margin of separation between the two classes
#both the quadratic programming problem and the final decision function depend
#only on dot products between patterns. This allows the use of the "kernel trick"
#and the generalization of this linear algorithm to the nonlinear case.
#Furthermore, SVMs can also produce class probabilities as output instead of class labels
#This is equivalent to fitting a logistic regression model to the estimated
#decision values. To extend the class probabilities to the multi-class case,
#all binary classifiers class probability  output can be combined

#NOVELTY DETECTION (one-class classification): where essentially an SVM detects outliers in a data set.
#SVM novelty detection works by creating a spherical decision boundary around 
# a set of data points by a set of support vectors describing the sphere's boundary.

#REGRESSION: By using a different loss function called the E-insensitive loss function
#SVMs can also perform regression. This loss function ignores errors that are smaller than a certain
#threshold thus creating a tube around the true output.

#We can estimate the accuracy of SVM regression by computing the scale parameter
#of a Laplacian distribution on the residuals , where f(x) is the estimated decision function (Lin and Weng 2004).
#packages involved: kernlab, mlbench
#Data in each packages,kernlab:(iris,spam,musk,promotergene), 
#melbench:(vowel.DNA,BreastCancer, BostonHousing,B3)



objects()#lists all the objects
library() #list all packages available
install.packages("kernlab")
library("kernlab")

data("iris")
irismodel <- ksvm(Species ~ ., data = iris,type = "C-bsvc",kernel = "rbfdot",
kpar = list(sigma = 0.1), C = 10,prob.model = TRUE)
irismodel

predict(irismodel, iris[c(3, 10, 56, 68,107,120), -5], type = "probabilities")
predict(irismodel, iris[c(3, 10, 56, 68,107, 120), -5], type = "decision")

 #ksvm allows for the use of any valid user defined kernel function by just defining a function
 #which takes two vector arguments and returns its Hilbert Space dot product in scalar form.
k <- function(x, y) {
(sum(x * y) + 1) * exp(0.001 * sum((x-y)^2))
}

class(k) <- "kernel"
data("promotergene")
gene <- ksvm(Class ~ ., data = promotergene,kernel = k, C = 10, cross = 5)
gene

##The implementation also includes the following computationally efficiently implemented kernels:
#Gaussian RBF, polynomial, linear, sigmoid, Laplace, Bessel RBF, spline, and ANOVA RBF


x <- rbind(matrix(rnorm(120), 2), matrix(rnorm(120,mean = 3), 2))
x
y <- matrix(c(rep(1, 60), rep(-1, 60)))
y
svp <- ksvm(x, y, type = "C-svc", kernel = "rbfdot",kpar = list(sigma = 2))
svp

#A contour plot of the fitted decision values for a simple binary classification problem.
plot(svp)

#The sample session starts with a C classification task on the iris data, using the radial basis
#function kernel with fixed hyper-parameters C and gamma:


#splitting the data
set.seed(123)
length(iris$Species)
train_sample <- sample(150,100)
iris_train <- iris[train_sample, ]
iris_test <- iris[-train_sample, ]
iris_train

install.packages("e1071")
library(e1071)

model<-svm(Species~., data = iris_train,method = "C-classification", kernel = "radial",cost = 10, gamma = 0.1)
summary(model)

plot(model, iris_train, Petal.Width ~Petal.Length, slice = list(Sepal.Width = 3,Sepal.Length = 4))


pred <- predict(model, head(iris), decision.values = TRUE)
attr(pred, "decision.values")


tobj <- tune.svm(type ~ ., data = spam_train[1:300,],gamma = 10^(-6:-3),cost = 10^(1:2))

summary(tobj)

plot(tobj, transform.x = log10, xlab = expression(log[10](gamma)),ylab = "C")

bestGamma <- tobj$best.parameters[[1]]
bestC <- tobj$best.parameters[[2]]
model <- svm(type ~ ., data = spam_train,cost = bestC, gamma = bestGamma, cross = 10)
summary(model)

pred <- predict(model, spam_test)
acc <- table(pred, spam_test$type)
classAgreement(acc)

pred$diag
pred$kappa
pred$rand
pred$crand

install.packages("klaR")
library("klaR")

data("B3")
Bmod <- svmlight(PHASEN ~ ., data = B3,svm.options = "-c 10 -t 2 -g 0.1 -v 0")
predict(Bmod, B3[c(4, 9, 30, 60, 80, 120),-1])

pred$class
pred$posterior




































































