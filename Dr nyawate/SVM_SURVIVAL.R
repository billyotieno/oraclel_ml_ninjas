#Support vector machine for survival analysis

install.packages("survivalsvm")
library(survivalsvm)
library(survival)
data(veteran, package = "survival")

#First, we split the data into a training and a test data set
set.seed(123)
n <- nrow(veteran)
train.index <- sample(1:n, 0.7 * n, replace = FALSE)
test.index <- setdiff(1:n, train.index)

#and next fit a survival support vector regression model
survsvm.reg <- survival(Surv(diagtime,status)~.,subset=train.index,
data=veteran,type="regression",gamma.mu=1,opt.meth="quadprog",kernel="add_kernel")
??functions
































































