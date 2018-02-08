#sms spam filter 
#built on R 3.3.3

#import the data set
library(readr)
sms_raw <- read_csv("~/sms_spam.csv")

#convert to factor
sms_raw$type <- factor(sms_raw$type)

convert_counts <- function(x){
  x<- ifelse(x>0,"Yes","No")
}

library(tm)
#pre processing

#create corpus
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

#convert to character to view the data
# as.character(sms_corpus[[1]])
# lapply(sms_corpus[1:2], as.character)

#convert all to lower case
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

#remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

#remove stop words
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

#remove punctuations
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

library(SnowballC)

#stem document
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

#remove white spaces
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

#create document term frequency
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

#create train and test set
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]
sms_train_labels <- sms_raw[1:4169,]$type
sms_test_labels <- sms_raw[4170:5559,]$type

library(wordcloud)
#subset
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")

#visualize word cloud
wordcloud(spam$text, max.words = 100, random.order = F)
wordcloud(ham$text, max.words = 100, random.order = F)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = F)

#reducing number of features, finding requent words
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words]

#converting indicator to yes or no
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts) #margin 2 means coloumns
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

#training
library(e1071)
sms_classification <- naiveBayes(sms_train, sms_train_labels, laplace = 1)

#prediction
sms_test_prdeict <- predict(sms_classification, sms_test)

#evaluation
library(gmodels)
CrossTable(sms_test_prdeict, sms_test_labels, prop.chisq = F, prop.t = F, dnn = c('predicted','acutual'))

