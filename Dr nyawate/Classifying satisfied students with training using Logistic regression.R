
#=====================================================================
#LOGISTIC REGRESSION CLASSIFICATION

projo=read.csv("projo.csv",header=T)

str(projo)
table(projo$SATISFIED)

# Install and load caTools package 
install.packages("caTools")
library(caTools)

# Randomly split data
set.seed(88)
split = sample.split(projo$SATISFIED, SplitRatio = 0.75)
split


# Create training and testing sets
projoTrain = subset(projo, split == TRUE)
projoTest = subset(projo, split == FALSE)

nrow(projoTrain)
nrow(projoTest)

# Logistic Regression Model
ProjoLog = glm(SATISFIED ~ .,data=projoTrain, family=binomial)
summary(ProjoLog)

#Prediction
predictTrain = predict(ProjoLog, type="response")
summary(predictTrain)
tapply(predictTrain, projoTrain$SATISFIED, mean)

#Thresholding
#We can convert the probabilities to predictions using what's called a 
#threshold value, t. If the probability of YES is greater than this 
#threshold value, t, we predict SATISFACTION. But if the probability of
#SATISFACTION is less than the threshold value, t, then we predict NO SATISFACTION

# Confusion matrix for threshold of 0.5
table(projoTrain$SATISFIED, predictTrain > 0.5)

# Sensitivity= true positives/(true positive + false negative)

# Specificity=true negatives/(true negative + false positives)


# Confusion matrix for threshold of 0.7
table(projoTrain$SATISFIED, predictTrain > 0.7)

# Sensitivity= true positive/(true positive + false negative)

# Specificity=true negative/(true negative + false positives)



# Confusion matrix for threshold of 0.5
table(projoTrain$SATISFIED, predictTrain > 0.2)

# Sensitivity= true positives/(true positive + false negative)

# Specificity=true negatives/(true negative + false positives)


#We see that by increasing the threshold value, the model's sensitivity
#decreases and specificity increases while the reverse happens if the 
#threshold value is decreased. So how to choose the optimum threshold value.
#Picking a good threshold value is often challenging.A Receiver Operator 
#Characteristic curve, or ROC curve, can help us decide which value of the
#threshold is best.

# Install and load ROCR package
install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(predictTrain, projoTrain$SATISFIED)

#We now use the performance function which defines what we'd like to plot
#on the x and y-axes of our ROC curve.
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

#Now, we just need to plot the output of the performance function.
# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


#Prediction on Test Set

#In this particular example, we used a threshold value of 0.3 and we 
#obtain the following confusion matrix.

predictTest = predict(ProjoLog, type = "response", newdata = projoTest)

table( projoTest$SATISFIED,predictTest >= 0.3)

# Accuracy=sum(main diagnol values)/total sum


#Conclusion
#The model can accurately identify students satisfied with training.
#Test set accuracy being equal to % which is greater than our baseline
#model.


















































