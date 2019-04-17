# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("genefilter", version = "3.8")
library(genefilter)
file_name_vec <- list.files("../data/ground_truth") #100 files in total
#split the datmaset into training and test data 
set.seed(1)
s<-sample(1:2,length(file_name_vec),prob=c(0.8,0.2),replace=T)
train.index<-which(s==1)
test.index<-which(s==2)
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
datamatrix<-as.data.frame(datamatrix)
test_df<-datamatrix[datamatrix$j %in% test.index,]
test_vec <- tes_split_list[datamatrix$j %in% test.index]
test_if_clean<-predvalue
#length(tesseract_if_clean)
test_if_error<-ifelse(test_if_clean==1,F,T)
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


#deal with two errors
detect_pos <- rep(0,length(error1))
dete_error <- data.frame(error1,detect_pos)
dete_error$pos1 <- rep(0,length(error1))
dete_error$pos2 <- rep(0,length(error1))
cc <- 1
for (i in error1) {
  len <- nchar(i)
  err2 <- c()
  if (len==1) {
    cc <- cc+1
    next
  }
  if (len!=1){
    for (j in 1:(len-1)) {
      for (k in (j+1):len) {
        temp <- paste("D",len, j, k, sep = "_")
        tempDD <- get(temp)
        char1 <- substr(i,j,j)
        char2 <- substr(i,k,k)
        if(tempDD[char1,char2]==0) {
          err2 <- c(err2,j,k)
        }
      }
    }
  }
  mode1 <- mfv(err2)
  if (length(mode1)==2) {
    if (nchar(i)==2){
      dete_error[cc,3] <- mode1[1]
    }
    else{dete_error[cc,3] <- mode1[1]
    dete_error[cc,4] <- mode1[2]}
  }
  if (is.na(mode1)) {
    cc <- cc+1
    next
  }
  if (length(mode1)==1) {
    t <- table(err2)
    num <- t[names(t)==mode1][[1]]
    
    if (num == length(err2)/2) {
      dete_error[cc,3] <- mode1
    }
    else {
      dete_error[cc,3] <- mode1
      err2 <- err2[-which(err2 == mode1)]
      mode2 <- mfv(err2)
      if (length(mode2)==1) {
        dete_error[cc,4] <- mode2
      }
    }
  }
  #print(i)
  cc <- cc+1
}

#correct two errors
#find the correct character
ccun <- 0
dete_error$corr <- dete_error[,1]
dete_error[,5] <- as.character(dete_error[,5])
for (l in error1) {
  ccun <- ccun+1
  if (dete_error[ccun,4]!=0) {
    for (i in letters) {
      for (j in letters) {
        err1 <- c()
        substr(dete_error[ccun,5],dete_error[ccun,3],dete_error[ccun,3]) <- i
        substr(dete_error[ccun,5],dete_error[ccun,4],dete_error[ccun,4]) <- j
        temp_word <- dete_error[ccun,5]
        # print(temp_word)
        len <- nchar(temp_word)
        for (a in 1:(len-1)) {
          for (b in (a+1):len) {
            temp <- paste("D",len, a, b, sep = "_")
            tempDD <- get(temp)
            char1 <- substr(temp_word,a,a)
            char2 <- substr(temp_word,b,b)
            if(tempDD[char1,char2]==0) {
              err1 <- c(err1,a,b)
            }
          }
        }
        if (is.na(err1) || length(err1)==0) {
          dete_error[ccun,5] <- temp_word
          break
        }
      }
      if (is.na(err1) || length(err1)==0) {
        dete_error[ccun,5] <- temp_word
        break
      }
    }
  } else if (dete_error[ccun,3]!=0) {
    for (i in letters) {
      for (j in letters) {
        err1 <- c()
        substr(dete_error[ccun,5],dete_error[ccun,3],dete_error[ccun,3]) <- i
        temp_word <- dete_error[ccun,5]
        len <- nchar(temp_word)
        for (a in 1:(len-1)) {
          for (b in (a+1):len) {
            temp <- paste("D",len, a, b, sep = "_")
            tempDD <- get(temp)
            char1 <- substr(temp_word,a,a)
            char2 <- substr(temp_word,b,b)
            if(tempDD[char1,char2]==0) {
              err1 <- c(err1,a,b)
            }
          }
        }
        if (is.na(err1) || length(err1)==0) {
          dete_error[ccun,5] <- temp_word
          break}
      }
      if (is.na(err1) || length(err1)==0) {
        dete_error[ccun,5] <- temp_word
        break}
    }
  } else {next}
}

dete_error
save(dete_error, file = '../output/error_correction2.RData')

correct2 <- rep(NA,length(error))
correct2[error.index] <- dete_error$corr
correct2[-error.index] <- error[-error.index]

test_vec_correct2 <- tolower(test_vec)
test_vec_correct2[test_if_error] <- correct2
test_df_correct2 <- cbind(tolower(test_vec),tolower(test_vec_correct2))
colnames(test_df_correct2) <- c("original_tesseract","after_correction2")
save(test_df_correct2, file = '../output/aftercorrection2.RData')
