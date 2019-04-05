if (!require("stringr")) {
  install.packages("stringr")
}

feature1 <- function(word) {
  return(str_length(input));
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
  return(length(grep("[^[:alnum:]=\\.]", strsplit(word,"")[[1]])));
}



