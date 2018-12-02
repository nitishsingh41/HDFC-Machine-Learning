#75209

adata=read.csv('AggregateData_Train.csv')
library(data.table)
tdata=fread('TransactionData_Train.csv')
testtdata=fread('TransactionData_Test.csv')
test=read.csv('AggregateData_Test.csv')
set.seed(100)

library("randomForest", lib.loc="~/R/win-library/3.3"
        library(data.table)
        library(Matrix)
        library(dplyr)
        library(MLmetrics)
        library(lightgbm)
        library(xgboost)
        dtrain <- xgb.DMatrix(label = y_label, data = as.matrix(adata))
y_label=adata[,'bad_flag']
adata[,'bad_flag']=NULL
adata[,'UID']=NULL
adata[,'V2']=NULL
adata[ adata == "?" ] <- NA
adata[] <- lapply(adata, function(x) as.numeric(as.factor(x)))
#cv 
cv.res <- xgb.cv(data = dtrain, label = y_label, nfold = 5,nrounds = 100, objective = "binary:logistic",early.stop.round = 10,metrics='auc')

params=list(eval_metric='auc')
model <- xgboost(data = dtrain, nrounds = 3000, objective = "binary:logistic",params=params)
names <- dimnames(data.matrix(adata[,-1]))[[2]]
importance_matrix <- xgb.importance(names, model = model)
xgb.plot.importance(importance_matrix)
test=read.csv('AggregateData_Test.csv')
uid=test[,'UID']
test[,'UID']=NULL
test[,'V2']=NULL
test[ test == "?" ] <- NA
test[] <- lapply(test, function(x) as.numeric(as.factor(x)))
y_test <- predict(model, data.matrix(test))
result=data.frame(UID=uid,bad_flag=y_test)
write.csv(result,file='sub with 50.csv',row.names = FALSE,col.names = TRUE)

#submission format error on c/d
test=read.csv('AggregateData_Test.csv')

uid=as.data.frame(test$UID)
names(uid)[1]<-"UID"
result=join(uid,result,by='UID')


#rf

df=na.roughfix(adata[,-52])
adata$bad_flag=as.factor(adata$bad_flag)
df['bad_flag']=adata['bad_flag']

model1 <- randomForest(bad_flag ~ ., data = df, importance = TRUE)

#remove columns
adata['V39']=NULL
adata['V9']=NULL
adata['V40']=NULL

adata['V37']=NULL
adata['V41']=NULL
adata['V10']=NULL
#impute
adata['bad_flag']=NA
train=rbind(adata,test)
require(mice)
train$UID=NULL
train$V2=NULL
train[ train == "?" ] <- NA
train[] <- lapply(train, function(x) as.numeric(as.factor(x)))

0.006499
0.006461


#cv 
require(caTools)
set.seed(101)
sample = sample.split(adata$bad_flag, SplitRatio = .80)
train=subset(adata, sample == TRUE)
test=subset(adata, sample == FALSE)
y_train=train[,'bad_flag']
y_test=test[,'bad_flag']
train[,'bad_flag']=NULL
test[,'bad_flag']=NULL
train[,'UID']=NULL
test[,'UID']=NULL
train[,'V2']=NULL
test[,'V2']=NULL
require(xgboost)
train[ train == "?" ] <- NA
train[] <- lapply(train, function(x) as.numeric(as.factor(x)))
test[ test == "?" ] <- NA
test[] <- lapply(test, function(x) as.numeric(as.factor(x)))
dtrain <- xgb.DMatrix(label = y_train, data = as.matrix(train))
dtest <- xgb.DMatrix(label = y_test, data = as.matrix(test))

#feature
require(plyr)
k=count(tdata,c('C2','C5','C10'))
names(k)[1]<-"V2"
c=subset(k,C5=='C')
c$C5=NULL
names(c)[2]<-"credit"
adata=join(adata,c,by='V2')
d=subset(k,C5=='D')
d$C5=NULL
names(d)[2]<-"debit"
adata=join(adata,d,by='V2')
adata['c/d']=adata$credit/(adata$debit+1)

##for test

k=count(testtdata,c('C2','C5'))
names(k)[1]<-"V2"
c=subset(k,C5=='C')
c$C5=NULL
names(c)[2]<-"credit"
test=join(test,c,by='V2')
d=subset(k,C5=='D')
d$C5=NULL
names(d)[2]<-"debit"
test=join(test,d,by='V2')
test['c/d']=test$credit/(test$debit+1)


data=rbind(adata,test)
data$mean=rowMeans(data,na.rm=TRUE)
require(matrixStats)
x=rowSds(as.matrix(data),na.rm=TRUE)

##date features

tdata$C8=gsub('-','',tdata$C8)
data=scale(data)


#for mlr
colnames(train)=gsub('_','',colnames(train))
colnames(train)=gsub(' ','',colnames(train))
colnames(test)=gsub('_','',colnames(test))
colnames(test)=gsub(' ','',colnames(test))


adata=adata[colSums(!is.na(adata))>0]
require(caTools)
sample=sample.split(adata,SplitRatio = 0.7)
train=subset(adata,sample==TRUE)
test=subset(adata,sample==FALSE)
traintask=makeClassifTask(data=adata,target='badflag')

require(HotDeckImputation)
data=impute.mean(DATA=as.matrix(data))

require(janitor)
train=remove_empty_cols(train)
test=remove_empty_cols(test)

library(parallelMap)
library(parallel)
parallelStartSocket(cpus = detectCores())

traintask=makeClassifTask(data=adata[1:20000,],target='bad_flag')
testtask=makeClassifTask(data=adata[20001:26775,],target='bad_flag')
rf<- makeLearner("classif.randomForest",predict.type ="prob")
rf$par.vals=list(ntree=5,importance=TRUE)
mod=train(rf,traintask)
prediction=predict(mod,task = testtask)
calculateConfusionMatrix(prediction)



#regex
"([0-9]{2})("[A-Z]{3})([0-9]{4})"
firstup <- function(x) {
   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
x
}

want_date=function(x){x=toString(x)
mon=formatC(match(firstup(tolower(substr(x,3,5))),month.abb), width = 2,flag = 0)
date=paste(substr(x,6,9),mon,substr(x,1,2),sep='-')
date
}


data$V1=lapply(data$V1,FUN=want_date)
data$V1=unlist(data$V1)
data$V1=as.factor(data$V1)

data=left_join(data,m,by=c('V2','V1'))



colnames(z)=paste('t',colnames(z),sep = '_')