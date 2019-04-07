if (!require("stringr")) {
  install.packages("stringr")
}

library(stringr)

feature1 <- function(word) {
  return(str_length(word))
}


feature2 <- function(word) {
  vowels <- c("a", "e", "i", "o", "u")
  
  word_lower <- tolower(strsplit(word, "")[[1]])
  word_freq <- word_lower[word_lower %in% letters]
  word_table <- table(word_freq)  
  
  print(word_table)
  l <- sum(word_table)
  v <- sum(word_table[vowels], na.rm=TRUE)
  c <- l - v
  
  q1 <- v / l
  q2 <- c / l
  q3 <- ifelse(c != 0, v/c, 0)
  
  return(c(v, c, q1, q2, q3))
}

feature3 <- function(word) {
  s<-length(grep("[^[:alnum:]=\\.]", strsplit(word,"")[[1]]))
  l<-str_length(word)
  return(c(s,s/l))
}

feature4 <- function(word){
  d<-length(grep("[0-9]", strsplit(word,"")[[1]]))
  l<-str_length(word)
  return(c(d,d/l)) 
}

feature5 <- function(word){
  low<-length(grep("[a-z]", strsplit(word,"")[[1]]))
  upp<-length(grep("[A-Z]", strsplit(word,"")[[1]]))
  l<-str_length(word)
  return(c(low,upp,low/l,upp/l)) 
}

feature6 <- function(word){
  l<-str_length(word)
  cur_count <- c()
  i<-1
  while (TRUE) {
    cur_letter <- substr(word,i,i)
    cur_count[i] <- 1
    j <- i + 1
    if(j==l+1){
      break
    }
    while (substr(word,j,j)==cur_letter) {
      cur_count[i] <- cur_count[i] + 1
      j <- j + 1
    }
    i<-j
    if(i==l+1){
      break
    }
  }
  count <- max(cur_count,na.rm = T)
  if(count<=3){
    return(0)
  }else{
    return(count/l)
  }
}

feature7<- function(word){
  l_alpha<-length(grep("[[:alnum:]=\\.]", strsplit(word,"")[[1]]))
  l<-nchar(word)
  k<-l-l_alpha
  return(ifelse(k>l_alpha,1,0))
}

feature8<- function(word){
  consec_con<-str_count(word,"[bcdfghjklmnpqrstvxzwyBCDFGHJKLMNPQRSTVXZWY]{6,}")
  return(ifelse(consec_con>=1,1,0))
}

feature9<- function(word){
  l<-nchar(word)
  trim_word <- substr(word,2,(l-1))
  non_alnum<-str_count(trim_word,"[^[:alnum:]]")
  return(ifelse(non_alnum>=2,1,0))
}

most_freq_sym <- function(word) {
  word_split <- strsplit(word, "")[[1]]
  word_table <- table(word_split)  
  max_sym <- word_table[which.max(word_table)]
  
  feature <- 0
  if (max_sym >= 3) {
    feature <- max_sym / str_length(word)
  }
  
  return(feature);
}

non_alpha_sym <- function(word) {
  l_alpha <- length(grep("[[:alnum:]=\\.]", strsplit(word,"")[[1]]))
  l <- str_length(word)
  l_nonalpha <- l - l_alpha
  
  return(ifelse(l_alpha == 0, 0, l_nonalpha / l_alpha))
}
