library(stringr)
library(tm)

my.data <- readLines('D:/sravan/LabelledData.txt')



my.data2<-gsub(",,,.*","",my.data) ## removes labels ,,,

n=sub('^.* ([[:alnum:]]+)$', '\\1', my.data)### retrives labels

type <- revalue(n,
c("what"="1", "who"="2", "when"="3", "affirmation"="4","unknown"="5")) ## assigning labels


raw_data=cbind(my.data2,type) ##combining labels column and text 
raw_data=as.data.frame(raw_data)
'



sms_corpus = Corpus(VectorSource(raw_data$my.data2)) ### assigning to corpus

inspect(sms_corpus[1:3])
##########cleaning text

corpus_clean = tm_map(sms_corpus, tolower)                    # convert to lower case
corpus_clean = tm_map(corpus_clean, removeNumbers)            # remove digits

corpus_clean = tm_map(corpus_clean, removePunctuation)        
corpus_clean = tm_map(corpus_clean, stripWhitespace)         
inspect(corpus_clean[1:3])
corpus_clean = tm_map(corpus_clean, PlainTextDocument)
dtm = DocumentTermMatrix(corpus_clean)




############ word cloud
freq1 <- colSums(as.matrix(dtm))
freq1 <- sort(colSums(as.matrix(dtm)), decreasing=TRUE) 


ord <- order(freq1,decreasing=TRUE)
freq1[head(ord)]


wordcloud(names(freq1),freq1)



################################# spliting data into train and test


DTM1 <- removeSparseTerms(dtm,0.97) ## removing sparse terms from document term matrix
DTM1 <- as.matrix(DTM1)
data <- cbind(DTM1,raw_data$type)          ### combining Document term matrix and response variable


colnames(data)[20]<-"Outcome"                 ##assigning name to response variable
set.seed(1)
split<-sample(nrow(data),floor(nrow(data)*0.7))  ### spliting data 70 and 30 % for train and test data sets

train_data<- data[split, ]
test_data<-data[-split, ]

train_data=as.data.frame(train_data)
test_data=as.data.frame(test_data)


Test<-test_data[-20]                  ## removing response variable
Train<-train_data[-20]                 ### removing response variable
typeof(Train)
d=as.list(Test)
c=as.data.frame(Test)
train_label=train_data$Outcome   ## assigning response variable for train data
Test_label=test_data$Outcome     ## assigning response variable for test data


### applying svm model
library(e1071)
SVM_model1=svm(factor(train_label) ~ .,Train,probability=T)

SVM_Pred1 = predict(SVM_model1,Test)
tab1=table(Test_label,SVM_Pred1)
Accuracy1=sum(diag(tab1))/sum(tab1)
Accuracy1


