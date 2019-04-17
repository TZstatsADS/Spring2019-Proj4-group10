library(e1071)
source('../lib/ifCleanToken.R')
load('../output/bigram.RData')
load('../output/datamatrix_new.RData')
file_name_vec <- list.files("../data/ground_truth") #100 files in total
#split the dataset into training and test data 
set.seed(1)
head(datamatrix)
s<-sample(1:2,length(file_name_vec),prob=c(0.8,0.2),replace=T)
train.index<-which(s==1)
test.index<-which(s==2)
datamatrix<-as.data.frame(datamatrix)
datamatrix$ifwordcorrect<-factor(datamatrix$ifwordcorrect)
trainset<-as.data.frame(datamatrix[datamatrix$j %in% train.index,c(-1,-2,-3,-4,-27)])
testset<-as.data.frame(datamatrix[datamatrix$j %in% test.index,c(-1,-2,-3,-4,-27)])

#try to train svm model using caret package, but running time is too long, so I gave up.
#library(kernlab)
#library(caret)
#ctrl = trainControl(method = "cv", number =  5, summaryFunction = twoClassSummary,classProbs = TRUE, savePredictions = TRUE)
#svm.fit<-train(ifwordcorrect~.,data=train_mat,method="svmRadial",metric="ROC",trControl=ctrl) 

#choose best parameters for svm with RBF Kernel
#the running time for svm is too long, so we do not perform cv, just split the data into 80%traning set and 20% test set

#C_arr_len<-3
#C_arr<-c(0.1,1,2)
#G_arr_len<-3
#G_arr<-c(0.05,0.1,1)
#err_rbf<-matrix(NA,nrow=3,ncol=3)
#for(i in 1:3){
#  for(j in 1:3){
#    cat("zaipao",i,j)
#    trainset<-train_mat[s==1,]
#    testset<-train_mat[s==2,]
#    svmfit<-svm(ifwordcorrect~.,data=trainset,cost=C_arr[i],gamma=G_arr[j],scale=T,kernel="radial")
#    err_rbf[i,j]<-mean(predict(svmfit,testset[,-1])!=testset[,1])
#  }
#}

#best model is with C=1, gamma=1
svm.best<-svm(ifwordcorrect~.,data=trainset,cost=1,gamma=1,scale=T,kernel="radial")
save(svm.best, file = "../output/svm.best.rda")
predvalue<-predict(svm.best, testset[,-1])
save(predvalue, file = "../output/predvalue.rda")

