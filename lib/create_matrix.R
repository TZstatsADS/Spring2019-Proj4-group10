#create matrix

truth_split <- str_split(current_ground_truth_txt, " ")
lengthperrow1 <- sapply(truth_split, length)
truth_rowindex <- rep(1:length(current_ground_truth_txt), lengthperrow1)
truth_table <- cbind(word = unlist(truth_split), truth_rowindex)
nrow(truth_table) #1751

tesseract_split <- str_split(current_tesseract_txt, " ")
lengthperrow2 <- sapply(tesseract_split, length)
tes_row <- rep(1:length(current_tesseract_txt), lengthperrow2)
tesseract_table <- cbind(word = unlist(tesseract_split), tes_row)
nrow(tesseract_table) #1756

res = c()
pos = rep(list(0), nrow(tesseract_table))
for (i in 1:nrow(tesseract_table)){
  res[i] <- ifelse(tesseract_table[i,1] %in% truth_table[,1],1,0)
  pos[[i]] <- if(res[i]==1) which(truth_table[,1]==tesseract_table[i,1]) else 0
}


truthpos = c(0) #position plus one
for (i in 1:nrow(tesseract_table)){
  truthpos[i+1] <- ifelse(((pos[[i]]>truthpos[i])&(pos[[i]] <= (i+5))),
                        pos[[i]][min(which(pos[[i]]>truthpos[i]))],
                        truthpos[i])
}
truthpos <- truthpos[-1]
truthpos[c(0, diff(truthpos))==0] <- 0 
truth_row <- unlist(sapply(1:nrow(tesseract_table), function(x) ifelse(truthpos[x]==0,0,truth_rowindex[x])))
#1: correct, 0: wrong
ifwordcorrect <- rep(1,nrow(tesseract_table))
ifwordcorrect[truthpos==0] <- 0

ifcorrectmatrix <- cbind(tesseract_table, truthpos, truth_row, ifwordcorrect)


#pos[[987]]   = 983


