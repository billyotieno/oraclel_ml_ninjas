#LINEAR REGRESSION: 

#rocket launch data
launch <- read.csv("challenger.csv")

#Assume that our shuttle launch data is stored in a data frame named launch, the
#independent variable x is temperature, and the dependent variable y is distress_ct.
#We can then use R's cov() and var() functions to estimate b:

b <- cov(launch$temperature, launch$distress_ct) /var(launch$temperature)

#From here we can estimate a using the mean() function:
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a

#correlations
r<- cov(launch$temperature, launch$distress_ct) /(sd(launch$temperature) * sd(launch$distress_ct))
r
#alternatively
r=cor(launch$temperature, launch$distress_ct)

#A basic regression function
reg <- function(y, x) {
x <- as.matrix(x)
x <- cbind(Intercept = 1, x)
b <- solve(t(x) %*% x) %*% t(x) %*% y
colnames(b) <- "estimate"
print(b)
}

#apply the reg function to the data
reg(y = launch$distress_ct, x = launch[2]) #simple linear regression

#multiple linear regression
reg(y = launch$distress_ct, x = launch[2:4])



#PREDICTING MEDICAL EXPENSES USING LINEAR REGRESSION
#COLLECTING DATA
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
summary(insurance$expenses)
hist(insurance$expenses)
table(insurance$region)

#EXPLORING RELATIONSHIPS AMONG FEATURES – the correlation matrix
cor(insurance[c("age", "bmi", "children", "expenses")])

#Visualizing relationships among features – the scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "expenses")])
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])


#TRAINING A MODEL ON THE DATA
ins_model <- lm(expenses ~ age + children + bmi + sex +smoker + region, data = insurance)


#EVALUATING MODEL PERFORMANCE
summary(ins_model)

#IMPROVING MODEL PERFORMANCE
#Model specification – adding non-linear relationships
#To summarised the improvements we :
#Added a non-linear term for age
#Created an indicator for obesity
#Specified an interaction between obesity and smoking

insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)
summary(ins_model2)


































































































