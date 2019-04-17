if (!require("stringr")) {
  install.packages("stringr")
}
if (!require("quanteda")) {
  install.packages("quanteda")
}

library(stringr)
library(quanteda)

features <- function(word){
  #feature1
  l <- str_length(word)
  
  #feature2
  word_lower <- tolower(strsplit(word, "")[[1]])
  vowels <- '[aeiou]'
  consonants <- '[bcdfghjklmnpqrstvxzwy]'
  v <- sum(str_count(word_lower, vowels))
  c <- sum(str_count(word_lower, consonants))
  q1 <- v / l
  q2 <- c / l
  q3 <- ifelse(c != 0, v/c, 2*l)

  #feature3 
  s <- length(grep("[^[:alnum:]]", strsplit(word,"")[[1]]))

  #feature4
  d<-length(grep("[0-9]", strsplit(word,"")[[1]]))

  #feature5
  low<-length(grep("[a-z]", strsplit(word,"")[[1]]))
  upp<-length(grep("[A-Z]", strsplit(word,"")[[1]]))

  #feature6
  maxletter <- max(unlist(rle(strsplit(word,"")[[1]])[1]))
  f6 <- ifelse(maxletter>2, maxletter/l, 0)

  #feature7 
  l_alpha<-length(grep("[[:alnum:]=\\.]", strsplit(word,"")[[1]]))
  k<-l-l_alpha
  f7 <- ifelse(k>l_alpha,1,0)

  #feature8
  consec_con<-str_count(word,"[bcdfghjklmnpqrstvxzwyBCDFGHJKLMNPQRSTVXZWY]{6,}")
  f8 <- ifelse(consec_con>=1,1,0)

  #feature9
  trim_word <- substr(word,2,(l-1))
  non_alnum<-str_count(trim_word,"[^[:alnum:]]")
  f9 <- ifelse(non_alnum>=2,1,0)

  #feature10bigram
  bigramletters <- tolower(char_ngrams(strsplit(word,"")[[1]],2,concatenator = ""))
  f10 <- ifelse(l>=2, sum(bigramtable[bigramletters], na.rm = TRUE)/length(bigramletters)/10000, 0)

  #feature11MFS
  split <- strsplit(word, "")[[1]]
  i <- max(table(split))
  f11 <- ifelse(i>=3, i/l, 0)

  #feature12NAS
  l1 <- length(grep("[[:alpha:]]", strsplit(word,"")[[1]]))
  l2 <- l-l1
  f12 <- ifelse(l1 != 0, l2/l1, l*2)

return(c(l=l,v=v, c=c,
         v_l = q1, c_l = q2, v_c = q3,
         s=s, s_l = s/l,
         d=d, d_l = d/l,
         low = low, upp = upp, low_l = low/l, upp_l = upp/l,
         f6=f6, f7=f7, f8=f8, f9=f9, f10=f10, f11=f11, f12=f12))
}
