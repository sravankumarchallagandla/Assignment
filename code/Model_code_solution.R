
my.data <- readLines('D:/sravan/LabelledData.txt')
string<- gsub("([A-Za-z]+).*", "\\1",my.data) ## retreving first words from text

labels=sub('^.* ([[:alnum:]]+)$', '\\1', my.data) ## retreving labels from text

merge=cbind(string,labels) ### merging both first words and labels

merge<-as.data.frame(merge)
###


table(merge$labels,merge$string) ###to get count of first words

###### we have 3 basic class when,what,who,the strings which doesnt belongs to this classes and affirimation where assinged to unknow 
merge$ad3<-sub("in|define|give|name|the|on|mccarren|cnn|woodrow|italy|hazma|unknownt","unknown",merge$string)
head(merge) 
ad=table(merge$labels,merge$ad3) ### table with orignal labels and created column

#### creating dummy variables

library(dummies)
df<-merge$ad3
data<-dummy(df)


data1 <- cbind(data,merge$labels) ### combining dummie variables and labels
fix(data1)

############spliting data into train and test for prediction

split<-sample(nrow(data1),floor(nrow(data1)*0.7))            ## spilting data into 70 and 30 percent for train and test
train_data<- data1[split, ]
test_data<-data1[-split, ]
#fix(data1)
train_data=as.data.frame(train_data)
head(train_data)
test_data=as.data.frame(test_data)
train_label=train_data$outcome            ## assigning response variable 
test_label=test_data$outcome            ## assigning response variable and assigning to new variable
train=train_data[,-23]                  ## Removing response variable from train set and assigning to new variable
test=test_data[,-23]                   ## Removing response variable from test set


#### Random Forest


model=randomForest(train_label~., data=train)
predicted <- predict(model, test, type="response");
tab1=table(test_label,round(predicted))
Accuracy1=sum(diag(tab1))/sum(tab1)
Accuracy1


