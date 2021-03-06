source('../lib/features.R')
datamatrix = c()
tes_split_list = c()
for(j in c(1:length(file_name_vec))){
# for(j in c(1:2)){
  current_file_name <- sub(".txt","",file_name_vec[j])
  ## read the ground truth text
  current_ground_truth_txt <- readLines(paste("../data/ground_truth/",current_file_name,".txt",sep=""), warn=FALSE)
  ## read the tesseract text
  current_tesseract_txt <- readLines(paste("../data/tesseract/",current_file_name,".txt",sep=""), warn=FALSE)
  
  #split ground truth text and get row index
  truth_split <- str_split(current_ground_truth_txt, " ")
  tru_split <- unlist(truth_split)
  truth_lengthperrow <- sapply(truth_split, length)
  truth_rowindex <- rep(1:length(current_ground_truth_txt), truth_lengthperrow)
  
  #split tesseract text and get row index
  tesseract_split <- str_split(current_tesseract_txt, " ")
  tes_split <- unlist(tesseract_split)
  tes_lengthperrow <- sapply(tesseract_split, length)
  tes_row <- rep(1:length(current_tesseract_txt), tes_lengthperrow)
  
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
  #if the previous selected word is not an appeared word in the ground truth text, the index will be equal to the last appeared word.
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
  truth_row <- unlist(sapply(1:length(tes_split), function(x) ifelse(truthpos[x]==0,0,truth_rowindex[truthpos[x]])))
  
  #create matrix
  index <- 1:length(truth_row)
  ifcorrectmatrix <- cbind(index, tes_row, truthpos, truth_row, ifwordcorrect)
  #rownames(ifcorrectmatrix) <- tes_split
  
  #add features to this matrix
  feat <- do.call(rbind, lapply(tes_split, features))
  tempmatrix <- cbind(ifcorrectmatrix, feat, j)
  datamatrix <- rbind(datamatrix, tempmatrix)
  tes_split_list <- c(tes_split_list, tes_split)
}

# for (p in 1:100){
#   print(p)
#   print(row.names(datamatrix[datamatrix[,'j']==p,])[1:3])
# }

save(datamatrix, file = '../output/datamatrix_new.RData')
save(tes_split_list, file = '../output/tes_split_list.RData')
