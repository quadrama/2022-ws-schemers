# https://stackoverflow.com/questions/22116938/twitter-sentiment-analysis-w-r-using-german-language-set-sentiws

readAndflattenSentiWS <- function(filename) { 
  lines = readLines(filename, encoding="UTF-8")
  words <- sub("\\|[A-Z]+\t[0-9.-]+\t?", ",", lines)
  partofspeech <- regmatches(lines, regexec("\\|[A-Z]+", lines))
  weights <- regmatches(lines, regexec("[0-9.-]+", lines))
  wordweights <- Map(c, words, weights, partofspeech)
  weights <- unlist(lapply(wordweights, 
                function(x) {rep(x[[2]], times = lengths(strsplit(x[[1]], ",")))}), 
            use.names = FALSE)
  partofspeech <- unlist(lapply(wordweights, 
                           function(x) {rep(x[[3]], times = lengths(strsplit(x[[1]], ",")))}), 
                    use.names = FALSE)
  weights <- as.numeric(weights)
  words <- unlist(strsplit(words, ","))
  wordweights.df <- data.frame(paste0(words, partofspeech), weights)
  names(wordweights.df) <- c("word", "weight")
  return(wordweights.df)
}

score.sentiment <- function(words, pos.words, neg.words) {
  require(plyr)
  require(stringr)
  scores <- Reduce(rbind, lapply(words, function(words, pos.words, neg.words) 
  {
    # compare our words to the dictionaries of positive & negative terms
    pos.matches <- match(words, pos.words$word)
    neg.matches <- match(words, neg.words$word)
    # match() returns the position of the matched term or NA
    # we want the weight:
    pos.weights <- pos.words[pos.matches,]$weight
    neg.weights <- neg.words[neg.matches,]$weight
    pos.score <- sum(pos.weights, na.rm = TRUE)
    neg.score <- sum(neg.weights, na.rm = TRUE)
    data.frame(pos.score=pos.score, neg.score=neg.score)
  }, 
  pos.words, neg.words))
  scores.df <- data.frame(pos.score=scores$pos.score, neg.score=scores$neg.score, text=words)
  return(scores.df)
}
