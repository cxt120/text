setwd("~/Desktop/PG")
###############
##pre-prosess##
##############
text<-read.delim("fitness_pre-processed_TEXT(2820).txt",header=F)
text<-as.matrix(text)
label<-read.delim("fitness_Label_DATA_nlp.txt",header=F)
label<-as.matrix(label)

##text clean##
require(stringr)
for(i in 1:2820){
  text[i]<-str_replace_all(text[i], "[[:punct:]]", " ")}
text<-tolower(text)

#combine data as a data frame
data<-data.frame(cbind(text,label))
names(data)<-c("text","label")

##devide train and test sets##
ind<-sample.int(2820,2250,replace=FALSE)
train<-data[ind,]
test<-data[-ind,]

##data explore
length<-str_length(text)
hist(length)

#make dictionary
require(RTextTools)
require(useful)
require(plyr)
doc_matrix.train<-create_matrix(train[,1],language="english",removeNumbers=T,removePunctuation=T,removeStopwords=T,stemWords=T)
doc_matrix.test<-create_matrix(test[,1],language="english",removeNumbers=T,removePunctuation=T,removeStopwords=T,stemWords=T)
doc_matrix.all<-create_matrix(data[,1],language="english",removeNumbers=T,removePunctuation=T,removeStopwords=T,stemWords=T)

train_in_test<-which(colnames(doc_matrix.train) %in% colnames(doc_matrix.test))
test_in_train<-which(colnames(doc_matrix.test) %in% colnames(doc_matrix.train))
text.train<-as.matrix(doc_matrix.train)
text.test<-as.matrix(doc_matrix.test)
text.train<-as.matrix(doc_matrix.train[,train_in_test])
text.test<-as.matrix(doc_matrix.test[,test_in_train])
text.X.train<-1*(text.train>0)
text.X.test<-1*(text.test>0)


##########################
##naive bayes classifier##
##########################
#build parameters 
a<-list(which(train$label==0),which(train$label==1))
n.c<-c(length(a[[1]]),length(a[[2]]))
theta.c<-n.c/2250
n.jc<-matrix(0,nrow=2,ncol=6153)
for (i in 1:2){
  n.jc[i,]<-colSums(text.X.train[a[[i]],])
}
#naive bayes algorithem
naive.Bayes<-function(alpha,theta.c,n.jc,n.c,X){
  theta.jc<-matrix(0,nrow=2,ncol=dim(X)[2])
  for ( i in 1:2){
    theta.jc[i,]<-(n.jc[i,]+alpha)/(n.c[i]+alpha/length(X[,1]))
  }
  weight<-apply(theta.jc,2,function(x) log(x[2]/(1-x[2]))-log(x[1]/(1-x[1])))
  prob<-apply(X,1,function(x) x%*%weight+log(theta.c[2]/theta.c[1]))
  
  return(prob)
}

#get prediction on testing set
label.nb<-naive.Bayes(alpha=1,theta.c=theta.c,n.jc=n.jc,n.c=n.c,X=text.X.test)
pred.nb<-rep(1,570)
pred.nb[which(label.nb<=0)]<-0
test.label<-as.numeric(as.matrix(test[,2]))
table(test.label,pred.nb)
prob.nb<-as.numeric(label.nb )

#compute accuracy
acu.nb<-sum(pred.nb==test.label)/570
acu.nb

#write out results
finalresult<-cbind(test,pred.nb,prob.nb)
write.csv(finalresult,file="testoutput.csv",row.names=F)
