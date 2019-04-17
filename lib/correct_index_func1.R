file_name_vec <- list.files("../data/ground_truth") #100 files in total
#datamatrix[datamatrix[,'j'] %in% test.index,'j']==4

#test.index
new_correct_index <- function(num_of_file_name){
  
  current_file_name <- sub(".txt","",file_name_vec[num_of_file_name])
  ## read the ground truth text
  current_ground_truth_txt <- readLines(paste("../data/ground_truth/",current_file_name,".txt",sep=""), warn=FALSE)
  
  #split ground truth text and get row index
  truth_split <- str_split(current_ground_truth_txt, " ")
  tru_split <- tolower(unlist(truth_split))
  # truth_lengthperrow <- sapply(truth_split, length)
  # truth_rowindex <- rep(1:length(current_ground_truth_txt), truth_lengthperrow)
  
  #read the text after correction
  #tes_split <- test_vec[datamatrix[datamatrix[,'j'] %in% test.index,'j']==num_of_file_name]
  tes_split <- tes_split_list[datamatrix[,'j'] ==num_of_file_name]
  
  ###############
  #get the correct position of corresponding word in ground truth text
  #for each word, we find all it's matches in ground truth text, but there are some problems
  #for example, 'and' may have several indexes in ground truth text. We want to find the correct one. 
  #for example, the third word only matches the last few words in the ground truth text, obviously it's not the right index. 
  ###############
  
  res = c()
  pos = rep(list(0), length(tes_split))
  truthpos <- c(0)
  
  for (i in 1:length(tes_split)){
    #if the word in OCR output appears in ground truth text
    res[i] <- ifelse(tes_split[i] %in% tru_split,1,0)
    #get all the matches for each appeared word
    pos[[i]] <- if(res[i]==1) which(tru_split==tes_split[i]) else 0
  }
  tempindex <- cumsum(res)
  
  #for each selected word in the loop, we have the index of it's previous selected word, 
  #if the previous selected word is not an appeared word, the index will be equal to the last appeared word.
  #delete indexes which are smaller than the index of the previous selected word.
  #and delete indexes which are greater than the smallest possible index of next two words
  
  for (i in 1:length(tes_split)){
    #get the index of next appeared word
    nextnonzero1 <- min(which(tempindex == tempindex[i]+1))
    
    #get the index of next second appeared word
    nextnonzero2 <- min(which(tempindex == tempindex[i]+2))
    
    a <- pos[[nextnonzero1]]
    b <- pos[[nextnonzero2]]
    
    #get a list of the right index of each word
    #for those words which are not appeared in ground truth text, the index will equal to the previous appeared word. 
    #so the list include increasing numbers
    
    truthpos[i+1] <- ifelse(sum((pos[[i]]>truthpos[i]) &
                                  pos[[i]]<= min(a[a>truthpos[i]]) & pos[[i]]<= min(b[b>truthpos[i]])
    )>0,
    pos[[i]][min(which(pos[[i]]>truthpos[i]))],
    truthpos[i])
  }
  truthpos <- truthpos[-1]
  #let the indexes of words which are not appeared in ground truth text be 0
  truthpos[c(0, diff(truthpos))==0] <- 0
  
  
  #finally get the list with correct indexes.    1: correct, 0: wrong
  ifwordcorrect <- rep(1,length(tes_split))
  ifwordcorrect[truthpos==0] <- 0
  
  #add row index in ground truth text 
  #truth_row <- unlist(sapply(1:length(tes_split), function(x) ifelse(truthpos[x]==0,0,truth_rowindex[truthpos[x]])))
  
  #create matrix
  index <- 1:length(tes_split)
  ifcorrectmatrix <- cbind(index, truthpos, ifwordcorrect)
  rownames(ifcorrectmatrix) <- tes_split
  
  total_correct_words <- sum(ifcorrectmatrix[,'ifwordcorrect']==1)
  total_words <- nrow(ifcorrectmatrix)
  
  return(list(total_correct_words=total_correct_words, 
              total_words=total_words,
              total_correct_words/total_words))
}
