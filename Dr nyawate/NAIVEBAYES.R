
setwd("C:/Users/hnyawate/Desktop/REPO")

getwd()

#NAIVE BAYES 
#Classification method that uses probability to estimate the likelihood that an observation falls into certain categories
#Classifiers based on Bayesian methods utilize training data to calculate an observed probability of each outcome based on the evidence provided by feature values. When
#the classifier is later applied to unlabeled data, it uses the observed probabilities to predict the most likely class for the new features.
#It's a simple idea, but it results in a method that often has results on par with more sophisticated algorithms

#DATA LOADING, INSTALL PACKAGES & DESCREPTIVE STATS
sms_raw=read.csv('sms_raw.csv',stringsAsFactors = FALSE)
sms_raw

library(tm)
#install.packages("tm")#text mining packages 
#install.packages("SnowballC")
#install.packages("gmodels")

as.character(sms_raw$sms)
str(sms_raw$text)
table(sms_raw$type)


#CLEANING AND STANDARDIZING THE DATA
#corpora (corpus 'singular') are collections of documents containing (natural language) text
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
sms_corpus
print(sms_corpus)
inspect(sms_corpus[1:2])

as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)

#Convert to lower case
sms_corpus_clean <- tm_map(sms_corpus,content_transformer(tolower))
as.character(sms_corpus_clean[[1]])

#Remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

#Remove stopwords
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords, stopwords())

#Remove punctuation
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

#Stem the document; returns the same vector of terms in its root form
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

#Strip whitespace
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)


#DATA PREPARATION splitting text documents into words

#1. Creating a DTM sparse matrix, given a tm corpus, involves a single command:
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm
#Ashortcut to the DTM
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(tolower = TRUE,removeNumbers = TRUE,stopwords = TRUE,removePunctuation = TRUE,stemming = TRUE))
sms_dtm2

#if the two sms_dtm are not similar then stopwords must be the problem thus replace it with the following function to correct it.
stopwords = function(x) { removeWords(x, stopwords()) }

#2. Creating training and test datasets
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]

sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

#To confirm that the subsets are representative of the complete set of SMS data, let's compare the proportion of spam in the training and test data frames:
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

#3. Visualizing text data  word clouds
# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud")
# word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("Wordcloud")
library("RColorBrewer")


wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

#split for separate visualisations
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))


#4. Creating indicator features for frequent words
findFreqTerms(sms_dtm_train, 5)

sms_freq_words <- findFreqTerms(sms_dtm_train, 5)

str(sms_freq_words)

#Create train and test foe freq words
sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

convert_counts <- function(x) {
x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,convert_counts)



#TRAINING A MODEL ON THE DATA
sms_classifier <- naiveBayes(sms_train, sms_train_labels)



#EVALUATING MODEL PERFORMANCE
#Prediction
sms_test_pred <- predict(sms_classifier, sms_test)

#Check the false posiutives and negatives
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,prop.chisq = FALSE, prop.t = FALSE,dnn = c('predicted', 'actual'))


#IMPROVING MODEL PERFORMANCE
#it does not mean that every message with this word should be classified as spam.
#We'll build a Naive Bayes model as done earlier, but this time set laplace = 1:

sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,laplace = 1)

sms_test_pred2 <- predict(sms_classifier2, sms_test)

CrossTable(sms_test_pred2, sms_test_labels,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,dnn = c('predicted', 'actual'))









