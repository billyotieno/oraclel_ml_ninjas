

getwd()
projo=read.csv("projo.csv",header=T)

edit(projo)

length(projo$SATISFIED)

set.seed(123)
train_sample <- sample(291,200)
project_train <- projo[train_sample, ]
project_test <- projo[-train_sample, ]
edit(project_train)


#SVM parameters tuning by giving their range in tune function as
list(epsilon = seq(0,10,0.05), cost = 2^(2:9)) 
tune.svm(model)

library(e1071)



model<-svm(SATISFIED~., data = project_train,method = "C-classification", 
           kernel = "radial",cost = 10, gamma = 0.1)
summary(model)
plot(model, project_train, Petal.Width ~Petal.Length, slice = 
       list(Sepal.Width = 3,Sepal.Length = 4))

