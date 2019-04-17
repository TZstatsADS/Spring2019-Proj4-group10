# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("genefilter", version = "3.8")
library(genefilter)
file_name_vec <- list.files("../data/ground_truth") #100 files in total
#split the dataset into training and test data 
set.seed(1)
s <- sample(1:2,length(file_name_vec),prob=c(0.8,0.2),replace=T)
train.index <- which(s==1)
test.index <- which(s==2)
source('../lib/ifCleanToken.R')
# load('../output/bigram.RData')
library(tidyr)
truth <- c()
for(i in train.index){
  inputtxt <- readLines(paste("../data/ground_truth/",file_name_vec[i],sep=""), warn=FALSE, encoding = 'UTF-8')
  truth <- c(truth, inputtxt)
}
#head(truth,15)
ground_truth_vec <- unlist(strsplit(tolower(truth), " "))
#length(ground_truth_vec)
load('../output/datamatrix_new.RData')
load('../output/tes_split_list.RData')
#tesseract_vec with detected error
load("../output/predvalue.rda")
#see correction.R
datamatrix <- as.data.frame(datamatrix)
test_df <- datamatrix[datamatrix$j %in% test.index,]
test_vec <- tes_split_list[datamatrix$j %in% test.index]
test_if_clean <- predvalue
#length(tesseract_if_clean)
test_if_error <- ifelse(test_if_clean==1,F,T)
#extract error terms
test_error_vec <- test_vec[test_if_error]
#length(test_error_vec)
error <- test_error_vec
error <- tolower(error)

#word clean
a <- na.omit(strsplit(unlist(error), "[^a-z]+"))
c = 0
for (i in a) {
  c = c+1
  if(length(i)>=2){
    temp <- i[1]
    for (j in 2:length(i)) {
      temp <- paste0(temp,i[j])
    }
    a[c] <- temp
  }
}
error <- unlist(a)
error1 <- c()
error.index<-c()
for (i in 1:length(error)) {
  if(error[i] != ""){
    error1 <- c(error1,error[i])
    error.index<-c(error.index,i)
  }
}
ground <- ground_truth_vec
b <- na.omit(strsplit(unlist(ground), "[^a-z]+"))
c = 0
for (i in b) {
  c = c+1
  if(length(i)>=2){
    temp <- i[1]
    for (j in 2:length(i)) {
      temp <- paste0(temp,i[j])
    }
    b[c] <- temp
  }
}
ground <- unlist(b)
ground1 <- c()
for (i in ground) {
  if(i != ""){
    ground1 <- c(ground1,i)
  }
}

#devide word by length
#max(nchar(ground1))
l = sort(unique(nchar(ground1)))

word_by_len = list()
for (i in ground1) {
  for (j in l) {
    if (nchar(i) == j){
      word_by_len[j][[1]] <- append(i,word_by_len[j][[1]])
    }
  }
}

#built matrix for each length of words
l <- l[-1]
for (i in l) {
  for (k in 1:(i-1)){
    for(j in (k+1):i) {
      name <- paste("D",i, k, j, sep = "_")
      df <- data.frame(matrix(c(rep(0,26*26)),26,26))
      colnames(df) <- c(letters)
      rownames(df) <- c(letters)
      assign(name,df)
    }
  }
}

#assign each character pair to matrix.

for (i in 2:length(word_by_len)) {
  for (j in word_by_len[[i]]) {
    for (k in 1:(i-1)) {
      for (a in (k+1):i) {
        temp <- paste("D",i, k, a, sep = "_")
        char1 <- substr(j,k,k)
        char2 <- substr(j,a,a)
        
        tempD <- get(temp)
        #print()
        tempD[char1,char2] <- 1
        assign(temp,tempD)
      }
    }
  }
}

###

library(modeest)

#detect the incorrect character
detect_pos <- rep(0,length(error1))
dete_error <- data.frame(error1,detect_pos)

c <- 1
for (i in error1) {
  len <- nchar(i)
  err <- 0
  if (len!=1){
    for (j in 1:(len-1)) {
      for (k in (j+1):len) {
        temp <- paste("D",len, j, k, sep = "_")
        tempDD <- get(temp)
        char1 <- substr(i,j,j)
        char2 <- substr(i,k,k)
        if(tempDD[char1,char2]==0) {
          err <- c(err,j,k)
        }
      }
    }
  }
  err <- err[-1]
  mode <- mfv(err)
  if(length(mode)==1){
    dete_error[c,2] <- mode
  }
  if(dete_error[c,2]== "NaN"){
    dete_error[c,2] <- 0
  }
  c <- c+1
}

#find the correct character
cunt <- 0
dete_error$real <- rep(0,length(error1))

for (i in error1) {
  cunt <- cunt+1
  len <- nchar(i)
  pos <- dete_error[cunt,2]
  if(pos==0 || len==1) {
    next
  }
  poss_pos <- c()
  possible_pos <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)
  for (j in 1:len) {
    char1 <- substr(i,j,j) 
    if (j==pos) {
      next
    }
    if (j<pos) {
      temp <- paste("D", len, j, pos,sep = "_")
      tempM <- get(temp)
      for (k in possible_pos) {
        if(tempM[char1,k]==1) {
          poss_pos <- c(poss_pos,k)
        }
      }
    }
    if (j>pos) {
      temp <- paste("D", len, pos, j,sep = "_")
      tempM <- get(temp)
      for (k in possible_pos) {
        if(tempM[k,char1]==1) {
          poss_pos <- c(poss_pos,k)
        }
      }
    }
    possible_pos <- poss_pos
    poss_pos <- c()
  }
  if(length(possible_pos)==1) {
    dete_error[cunt,3] <- possible_pos
  }
}

#show the correct word
#intToUtf8(97+i)
count <- 0
dete_error$correct <- dete_error$error1
dete_error[,4] <- as.character(dete_error[,4])
for (i in error1) {
  count <- count+1
  if (dete_error[count,3]!=0) {
    j <- dete_error[count,3]
    substr(dete_error[count,4],dete_error[count,2],dete_error[count,2]) <- intToUtf8(96+j)
  }
}


dete_error
save(dete_error, file = '../output/error_correction1.RData')

correct1 <- rep(NA,length(error))
correct1[error.index] <- dete_error$correct
correct1[-error.index] <- error[-error.index]

test_vec_correct1 <- tolower(test_vec)
test_vec_correct1[test_if_error] <- correct1
test_df_correct1 <- cbind(tolower(test_vec),tolower(test_vec_correct1))
colnames(test_df_correct1) <- c("original_tesseract","after_correction1")
save(test_df_correct1, file = '../output/aftercorrection1.RData')