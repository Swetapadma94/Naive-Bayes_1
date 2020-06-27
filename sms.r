sms<-read.csv(file.choose(),stringsAsFactors = F)
View(sms)
class(sms)
str(sms)
dim(sms)
sms$type<-factor(sms$type)
class(sms$type)
table(sms$type)
library(tm)
# Prepare corpuse for the text data 
## corpus all documents
sms_corpous<-VCorpus(VectorSource(sms$text))
View(sms_corpous)
str(sms_corpus)
# Cleaning data (removing unwanted symbols)
corpous_clean<-tm_map(sms_corpous,content_transformer(tolower))
corpous_clean<-tm_map(sms_corpous,tolower)
corpous_clean<-tm_map(corpous_clean,removeNumbers)
corpous_clean<-tm_map(corpus_clean,removeWords,stopwords())
corpous_clean<-tm_map(corpous_clean,removePunctuation)
corpous_clean<-tm_map(corpous_clean,stripWhitespace)
class(corpous_clean)
corpous_clean
# Do not run the plainTextDocument
corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
# create a document-term sparse matrix
#corpus_clean<-Corpus(VectorSource(corpus_clean))
sms_dtm<-DocumentTermMatrix(corpous_clean)
class(sms_dtm)
as.character(sms_dtm)
# creating training and test datasets
sms_raw_train<-sms[1:4169,]
sms_raw_test<-sms[4170:5559,]
sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]
sms_corpus_train <- corpous_clean[1:4169]
sms_corpus_test  <- corpous_clean[4170:5559]
# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# indicator features for frequent words
sms_dict<-findFreqTerms(sms_dtm_train, 5)
sms_train<-DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_train
sms_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))
sms_test
inspect(sms_corpus_train[1:100])
list(sms_dict[1:100])
#convert counts to a factor
convert_count<-function(x){
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}
# apply() convert_counts() to columns of train/test data
sms_train<-apply(sms_train, MARGIN = 2, convert_count)
sms_test  <- as.data.frame(apply(sms_test, MARGIN = 2, convert_count))
View(sms_train)
View(sms_test)
##  Training a model on the data ----
library(e1071)
model<-naiveBayes(sms_train,sms_raw_train$type)
model
##  Evaluating model performance ----
pred<-predict(model,sms_test)
pred
class(pred)
# Accuracy 
mean(pred==sms_raw_test$type)
##0.9784173
library(gmodels)
CrossTable(pred,sms_raw_test$type)
## Or##
CrossTable(pred,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,dnn=c('precicted','actual'))
