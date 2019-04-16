replace_func <- function(word){
  index <- which(unique_error$error1 == word)
  word <- ifelse(length(index)==0, word, unique_error[index,]$correct)
  return(word)
}
