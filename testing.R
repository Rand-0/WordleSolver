words = read.csv("words/wordsENG.txt", header = F)

test = prepareWordsList(words)

cand = calcEntropy(words)

words_new = apply(test[,2:6], 1, doesMatchPattern, c(0,2,0,0,0), c("w", "i", "g", "h", "t"))

words = words[which(words_new == 1),]

words = as.data.frame(na.omit(words))
