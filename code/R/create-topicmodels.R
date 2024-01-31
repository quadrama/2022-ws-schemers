library(DramaAnalysis)
library(topicmodels)
library(slam)
library(tm)
library(data.table)

frequencytable <- function(drama, 
                           acceptedPOS = postags$de$words,
                           bigram=FALSE,
                           column="Token.lemma", 
                           byCharacter=FALSE, 
                           sep="|",
                           sepBigram=" ",
                           normalize=FALSE, 
                           sortResult=FALSE, 
                           segment=c("Drama", "Act", "Scene")) {
  stopifnot(inherits(drama, "QDDrama"))
  
  segment <- match.arg(segment)
  
  ft <- switch(segment,
               Drama=drama$text,
               Act=segment(drama$text, drama$segments),
               Scene=segment(drama$text, drama$segments))
  
  
  if (length(acceptedPOS) > 0)
    ft <- ft[as.character(ft$Token.pos) %in% acceptedPOS,]
  
  if (bigram == TRUE) {
    ft$nextToken <- data.table::shift(ft[,get(column)], n=-1)
    ft$ngram <- paste(ft[,get(column)], ft$nextToken, sep = sepBigram)
  } else {
    ft$ngram <- ft[,get(column)]
  }
  
  segment <- match.arg(segment)
  switch(segment,
         Drama = { 
           if (byCharacter == FALSE) { xt <- stats::xtabs(~drama + ft$ngram, data=ft) }
           else { xt <- stats::xtabs(~ paste(drama,Speaker.figure_id,sep=sep) + ~ft$ngram, data=ft) }
         },
         Act = {
           if (byCharacter == FALSE) { xt <- stats::xtabs(~ paste(drama,Number.Act,sep=sep) + ~ft$ngram, data=ft) }
           else { xt <- stats::xtabs(~ paste(drama,Number.Act,Speaker.figure_id,sep=sep) + ~ft$ngram, data=ft) }
         },
         Scene = {
           if (byCharacter == FALSE) { xt <- stats::xtabs(~ paste(drama,Number.Act,Number.Scene,sep=sep) + ~ft$ngram, data=ft) }
           else { xt <- stats::xtabs(~ paste(drama,Number.Act,Number.Scene,Speaker.figure_id,sep=sep) + ~ft$ngram, data=ft) }
         },
         stop("Please enter valid string-value for argument 'segment' (default = 'Drama', 'Act' or 'Scene').")
  )
  
  r <- as.matrix(stats::ftable(xt, row.vars = c(), col.vars = c()))
  
  if (normalize == TRUE) {
    r <- t(apply(r,1,function(x) { x / sum(x)}))
  } 
  
  if (sortResult == TRUE) {
    r <- r[,order(colSums(r),decreasing = TRUE),drop = FALSE]
  } else {
    r <- r[, order(colnames(r)) ,drop = FALSE]
  }
  
  class(r) <- append(class(r), switch(segment, 
                                      Drama = "QDByDrama",
                                      Act   = "QDByAct",
                                      Scene ="QDByScene"))
  
  r
}

if (!file.exists("data/ldaModels.RData")) {
  
  stopwords <- stopwords(kind = "german")
  stopwords <- c(stopwords, ".", "-", "–", "––", ",", ";", ":", "?", "!", "'", 
                 "_", "--", "---", "«", "»", "[", "]", "|")
  
  ldaModels <- list()
  
  ids <- loadAllInstalledIds()
  ids <- ids[startsWith(ids, "qd:")]
  all_dramas <- loadDrama(ids, defaultCollection = "qd")
  
  # Remove stopwords
  all_dramas$text <- all_dramas$text[!which(tolower(all_dramas$text$Token.surface) %in% stopwords)]
  all_dramas$text <- all_dramas$text[!which(tolower(all_dramas$text$Token.lemma) %in% stopwords)]
  pos.allowed <- postags$de$words[postags$de$words %in% c("ADJA", "ADJD", "NN", "VVINF", "VVIZU",
                                                          "VVPP", "VVIMP", "VVFIN", "ADV")]
  # Create frequency table of bigrams
  ft2 <- as.simple_triplet_matrix(as.matrix(frequencytable(all_dramas, bigram = TRUE, acceptedPOS = pos.allowed, column = "Token.lemma", normalize = FALSE)))
  # Split text into chunks of 1,000 tokens
  all_dramas$text$drama <- rep(1:((nrow(all_dramas$text)/1000)+1), times=1, each=1000, length.out=nrow(all_dramas$text))
  # Create frequency matrix
  ft <- as.simple_triplet_matrix(as.matrix(frequencytable(all_dramas, acceptedPOS = pos.allowed, column = "Token.lemma", normalize = FALSE)))
  # Train LDA models
  ldaModels$lda_10 <- LDA(ft, k=10, method="Gibbs")
  ldaModels$lda_5 <- LDA(ft, k=5, method="Gibbs")
  ldaModels$lda_20 <- LDA(ft, k=20, method="Gibbs")
  ldaModels$lda_50 <- LDA(ft, k=50, method="Gibbs")
  ldaModels$lda_100 <- LDA(ft, k=100, method="Gibbs")
  ldaModels$lda_10_bigram <- LDA(ft2, k=10, method="Gibbs")
  ldaModels$lda_5_bigram <- LDA(ft2, k=5, method="Gibbs")
  ldaModels$lda_20_bigram <- LDA(ft2, k=20, method="Gibbs")
  ldaModels$lda_50_bigram <- LDA(ft2, k=50, method="Gibbs")
  ldaModels$lda_100_bigram <- LDA(ft2, k=100, method="Gibbs")
  
  save(ldaModels, file = "data/ldaModels.RData")
} else {
  load("data/ldaModels.RData")
}

# Create list of lemmas per topic
for (model in names(ldaModels)) {
  terms <- terms(ldaModels[[model]], k = 1000000)
  write.csv(terms, file = paste0("data/topics_",model,".csv"), quote = FALSE)
}
