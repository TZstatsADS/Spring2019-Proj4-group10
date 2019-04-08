library(tidyr)
truth <- c()
for(i in c(1:length(file_name_vec))){
  inputtxt <- readLines(paste("../data/ground_truth/",file_name_vec[i],sep=""), warn=FALSE, encoding = 'UTF-8')
  truth <- c(truth, inputtxt)
}
#head(truth,15)
splittruth <- unlist(strsplit(tolower(truth), " "))
#head(splittruth,30)
bigramfreq <- sapply(strsplit(splittruth,""), function(x) char_ngrams(x, 2,concatenator = ""))
bigramtable <- table(unlist(bigramfreq))
save(bigramtable,  file="../output/bigram.RData")
