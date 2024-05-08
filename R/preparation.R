prepareWordsList <- function(words)
{
  words_check = apply(words, 1, nchar)

  if(length(unique(words_check)) != 1)
  {stop("All words should be the same length!")}

  chars = sapply(words[,1], stringr::str_split_1, "")

  words_trans = cbind(words, t(chars))

  words_cols = c("word")

  for(i in 1:nchar(words[1,1]))
  {
    words_cols = c(words_cols, paste0("Char", i))
  }

  colnames(words_trans) = words_cols

  words_trans
}

doesMatchPattern <- function(word, pattern, chars)
{
  if(length(word) != length(pattern))
  {stop("Error 1")}

  if(length(word) != length(chars))
  {stop("Error 2")}

  for(i in 1:length(pattern))
  {
    if(pattern[i] == 0)
    {
      for(j in word)
      {
        if(chars[i] == j) {return(0)}
      }

    } else if(pattern[i] == 1)
    {
      if(word[i] == chars[i]) {return(0)}

      str = FALSE

      for(j in 1:length(word))
      {
        if(word[j] == chars[i] & j != i) {str = TRUE}
      }

      if(!str) {return(0)}

    } else
    {
      if(chars[i] != word[i]) {return(0)}

    }
  }
  return(1)
}

calcEntropy_word <- function(word, words)
{
  patterns <- expand.grid(replicate(5, c(0,1,2), simplify = FALSE))

  n_words = nrow(words)
  res = c()

  for(i in 1:nrow(patterns))
  {
    res_i = apply(words, 1, doesMatchPattern, patterns[i,], word)

    p_i = sum(res_i)/n_words

    if(p_i == 0)
    {
      Ent_i = 0
    } else
    {Ent_i = p_i * log(1/p_i, 2)}

    res = c(res, Ent_i)
  }

  return(sum(res))
}

calcEntropy <- function(words)
{
  words_tran = prepareWordsList(words)

  words_chars = words_tran[,2:ncol(words_tran)]

  entropy = apply(words_chars, 1, calcEntropy_word, words_chars)

  return(entropy)
}
