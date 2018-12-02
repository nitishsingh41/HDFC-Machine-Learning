#feature engineering
money=group_by(tdata,C2,C5)
m=summarize(money,amount=sum(C12))
d=subset(m,C5=='D')
names(d)[1]<-"V2"
d$C5=NULL
names(d)[2]<-"debit"
adata=join(adata,d,by='V2')
c=subset(m,C5=='C')
names(c)[1]<-"V2"
c$C5=NULL
names(c)[2]<-"credit"
adata=join(adata,c,by='V2')
#no fe train-error:0.094332+0.001250	test-error:0.095089+0.004483
#train-error:0.094248+0.000464	test-error:0.094976+0.002422
#train-error:0.094444+0.000771	test-error:0.094977+0.002805
#train-error:0.091288+0.001912	test-error:0.095238+0.007095 with c6 count
#train-error:0.092586+0.000817	test-error:0.095126+0.002312 with c6 amount
#train-error:0.092428+0.001558	test-error:0.095649+0.005143 with c6,c10 count
#train-error:0.092157+0.000571	test-error:0.095201+0.003602 with c6,c10 amount
#[4]	train-error:0.091858+0.001400	test-error:0.095574+0.006353 with advanced fe
#train-error:0.090924+0.000838	test-error:0.095425+0.002132 with c2 c5 c10 with advanced fe
require(dplyr)
trans=rbind(tdata,testtdata)
k=tdata%>%group_by(C2,C6)%>%summarize(count_ttype=n())%>%arrange(C2)
library(reshape2)
z=dcast(k,C2~C6,value.var = 'count_ttype')
names(z)[1]<-"V2"
require(plyr)
adata=join(adata,z,by='V2')

l=trans%>%group_by(C2,C6,C10)%>%summarize(amt=mean(C12))%>%arrange(C2)
library(reshape2)
y=dcast(l,C2~C6+C10,value.var = 'amt')
names(y)[1]<-"V2"
require(plyr)
adata=join(adata,y,by='V2')

l=trans%>%group_by(C2,C6,C10)%>%summarize_at(vars(C12),funs(mean(C12),sd(C12),median(C12)))%>%arrange(C2)
#y=dcast(setDT(l),C2~C6+C10,value.var = c('mean','sd','median')) memory issue

l=trans%>%group_by(C2,C6,C10)%>%summarize_at(vars(C12),funs(mean(C12),sd(C12)))%>%arrange(C2)
y=dcast(setDT(l),C2~C6+C10,value.var = c('mean','sd'))
names(y)[1]<-"V2"


k=trans%>%group_by(C2,C6)%>%summarize(count_ttype=n())%>%arrange(C2)
library(reshape2)
z=dcast(k,C2~C6,value.var = 'count_ttype')
names(z)[1]<-"V2"
require(plyr)
adata=join(adata,z,by='V2')

m=trans%>%group_by(C2,C5,C10)%>%summarize(amt=mean(C12))%>%arrange(C2)
x=dcast(m,C2~C5+C10,value.var = 'amt')
names(x)[1]<-"V2"

m=trans%>%group_by(C2,C5,C6,C10)%>%summarize(amt=mean(C12))%>%arrange(C2)
x=dcast(m,C2~C5+C6+C10,value.var = 'amt')
names(x)[1]<-"V2"

#datetime features
trans$C13=as.Date(trans$C8)
trans$C9=as.Date(trans$C9, format = "%d%b%Y") 

detach("package:plyr", unload=TRUE)
l=trans%>%group_by(C2,C5,C8,C9)%>%summarize(amt=mean(C12))%>%arrange(C2)
y=dcast(l,C2~C5+C8+C9,value.var = 'amt')#0.99955
names(y)[1]<-"V2"

m=trans%>%group_by(C2,C6,C10)%>%summarize(sdamt=sd(C12))%>%arrange(C2)
m$C6=paste('sd',m$C6,sep="_")
x=dcast(m,C2~C6+C10,value.var = 'sdamt')
names(x)[1]<-"V2"
require(plyr)
adata=join(adata,x,by='V2')

k=trans%>%group_by(C2,C5,C6)%>%summarize(count_ttype=n())%>%arrange(C2)
library(reshape2)
z=dcast(k,C2~C5+C6,value.var = 'count_ttype')
names(z)[1]<-"V2"
require(plyr)
adata=join(adata,z,by='V2')#train-error:0.000336, improved my lb score 975 , ensemb with 2 times this and 965 lead to 977

k=trans%>%group_by(C2,C5,C6,C10)%>%summarize(count_ttype=n())%>%arrange(C2)
library(reshape2)
z=dcast(k,C2~C5+C6+C10,value.var = 'count_ttype')
names(z)[1]<-"V2"
require(plyr)
adata=join(adata,z,by='V2')#train-error:0.000336, on lb 967

m=trans%>%group_by(C2,C5)%>%summarize(amt=sum(C12))%>%arrange(C2)
x=dcast(m,C2~C5,value.var = 'amt')
names(x)[1]<-"V2"#train-error:0.000336, on lb 950

m=tdata%>%group_by(C2,C5)%>%summarize(count_ttype=n())%>%arrange(C2)
x=dcast(m,C2~C5,value.var = 'count_ttype')
names(x)[1]<-"V2"
adata=join(adata,x,by='V2')

n=tdata%>%group_by(C2,C6)%>%summarize(count_ttype=n())%>%arrange(C2)
w=dcast(n,C2~C6,value.var = 'count_ttype')
names(w)[1]<-"V2"
adata=join(adata,w,by='V2')#train-error:0.000187, on lb 0.51513

o=tdata%>%group_by(C2,C10)%>%summarize(count_ttype=n())%>%arrange(C2)
v=dcast(o,C2~C10,value.var = 'count_ttype')
names(v)[1]<-"V2"
adata=join(adata,v,by='V2')


#date
m=tdata%>%group_by(C2,C8)%>%summarize(count_ttype=n())%>%arrange(C2)
#library(reshape2)
#z=dcast(k,C2~C8,value.var = 'count_ttype')
names(m)[1]<-"V2"
names(m)[2]="V1"
names(m)[3]="cout_time"
require(plyr)
adata=join(adata,m,by=c('V2','V1'))#train-error:0.091316+0.001095	test-error:0.095649+0.003828,  train 187, lb=99852 

imp=xgb.importance(colnames(adata),model=model)
ifeat=imp$Feature[1:200]
#adata=adata[,ifeat]
adata=adata[,c('UID','bad_flag','V2',ifeat)]

colnames(v)=paste('',colnames(v),sep="_")
#train-error:0.000336
#train-error:0.000299 

#1000 rounds
#train-error:0.001494 
#train-error:0.001307 with 200 feat
#train-error:0.000934 100 feat
#train-error:0.000934 50 feat
#train-error:0.000560 +sd features


#train-error:0.001494
#train-error:0.000747

#train-error:0.000037 normalizing inputs in 3k lb=99982
