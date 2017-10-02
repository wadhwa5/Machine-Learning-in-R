#-------------------------------Library Used------------------------------------
install.packages(c("ggplot2","e1071","caret","irlba","quanteda","randomForest","doSNOW"))
library(ggplot2)
library(e1071)
library(caret)
library(irlba)
library(quanteda)
library(randomForest)
library(doSNOW)
#------------------------------Data loading and cleaning------------------------

spam_raw<-read.csv("spam.csv",stringsAsFactors = FALSE)
spam_raw1<-spam_raw[,1:2]
names(spam_raw1)<-c("label","sms")
length(which(!complete.cases(spam_raw1)))
str(spam_raw1)
# as we have binary classification only so I am converting Lables column to factor.
factor(spam_raw1$label)
spam_raw1$label<-as.factor(spam_raw1$label)
str(spam_raw1)

#-----------------------------EDA------------------------------------------------

#checking the distribution of label
prop.table(table(spam_raw1$label))
# from this we can see that data is not balence its 86.5:13.5


spam_raw1$smsLength<-nchar(spam_raw1$sms)
ggplot(spam_raw1,aes(x=smsLength,fill=label))+geom_histogram(binwidth = 20)+labs(y='Text Count',x='SMS lenght',title="Distribution of SMS lenght vs Class Labels")



#------------------------Data Partition------------------------------------------
set.seed(32984)
index<-createDataPartition(spam_raw1$label,times = 1,p=0.7,list = FALSE)
train<-spam_raw1[index,]
test<-spam_raw1[-index,]

#------------------------Pre-Processing------------------------------------------
#removeing punctitions, symbols etc
train.token<- tokens(train$sms,what = "word",remove_numbers = TRUE,remove_punct = TRUE,remove_symbols = TRUE,remove_hyphens = TRUE)
#converting to lower case
train.token<-tokens_tolower(train.token)
#removing stoping words
train.token<-tokens_select(train.token,stopwords(),selection = "remove")
#stemming the words
train.token<-tokens_wordstem(train.token,language = "english")

#Making document-feature matrix
train.dfmatrix<-dfm(train.token)
train.dfm<-as.matrix(dfm(train.token))

#--------------------------Model Building---------------------------------------

train.fm<-cbind(train$label,as.data.frame(train.dfm))
#as some of the column names are not syntactically correct we make them right using make.names
#example names(train.fm)[c(234,231,145,147)]
names(train.fm)<-make.names(names(train.fm))

#using startified CV using caret package
# for imbalence data do repeated cv 
set.seed(48734)
cv.folds<-createMultiFolds(train$label,k=10,times = 3)
cv.cntrl<-trainControl(method = "repeatedcv",number = 10,repeats = 3,index = cv.folds)

#for parallel processing we using doSNow package

start.time<-Sys.time()
cl<-makeCluster(3,type = "SOCK")
registerDoSNOW(cl)

rpart1<-train(train.label~.,data = train.fm,method = "rpart",trControl=cv.cntrl,tuneLength=7)

stopCluster(cl)

rpart1



#creating TF-IDF function

TF<-function(row){row/sum(row)}
IDF<-function(col){
        corpus.size<-length(col)
        doc.count<-length(which(col !=0))
        log10(corpus.size/doc.count)
}

TF.IDF<-function(tf,idf){tf*idf}

train.df<-apply(train.dfmatrix,1,TF)
train.idf<-apply(train.dfmatrix,2,IDF)
train.tf.idf<-apply(train.df,2,TF.IDF,idf=train.idf)


train.tf.idf<-t(train.tf.idf)

#checking for incomplete cases
incomplete.cases<-which(!complete.cases(train.tf.idf))
train$sms[incomplete.cases]

#fixing incomplete cases

train.tf.idf[incomplete.cases,]<-rep(0.0,ncol(train.tf.idf))
sum(which(!complete.cases(train.tf.idf)))


#correcting the names again
train.new<-cbind(train$label,as.data.frame(train.tf.idf))
names(train.new)<-make.names(names(train.new))

#running model with new data of tf-idf
start.time<-Sys.time()
cl<-makeCluster(3,type = "SOCK")
registerDoSNOW(cl)

rpart2<-train(train.label~.,data = train.new,method = "rpart",trControl=cv.cntrl,tuneLength=7)

stopCluster(cl)

rpart2



#-------------------------------------Ngrams 
#--------------------------N Grams---------------------------------------------
