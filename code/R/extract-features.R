source("code/R/load.R")
source("code/R/functions.R")
source("code/R/sentiment.R")

library(topicmodels)
library(plyr)
library(dplyr)
library(tidytext)
library(tidyr)

ttr <- function(x,w=1000) {
  if (length(x) < w) {
    return(0)
  }
  ds <- split(x, ceiling(seq_along(x)/w))
  ttr <- 0
  for (d in ds) {
    ttr <- ttr + length(levels(factor(d))) / length(d)
  }
  ttr/length(ds)
}

frequencytableSd <- function(drama, 
                           acceptedPOS = postags$de$words,
                           column="Token.lemma", 
                           byCharacter=FALSE, 
                           sep="|", 
                           normalize=FALSE, 
                           sortResult=FALSE, 
                           segment=c("Drama", "Act", "Scene")) {
  stopifnot(inherits(drama, "QDDrama"))
  
  segment <- match.arg(segment)
  
  ft <- switch(segment,
               Drama=drama$stageDirections,
               Act=segment(drama$stageDirections, drama$segments),
               Scene=segment(drama$stageDirections, drama$segments))
  
  
  if (length(acceptedPOS) > 0)
    ft <- ft[as.character(ft$Token.pos) %in% acceptedPOS,]
  
  segment <- match.arg(segment)
  switch(segment,
         Drama = { 
           if (byCharacter == FALSE) { xt <- stats::xtabs(~drama + ft[,get(column)], data=ft) }
           else { xt <- stats::xtabs(~ paste(drama,Speaker.figure_id,sep=sep) + ~ft[,get(column)], data=ft) }
         },
         Act = {
           if (byCharacter == FALSE) { xt <- stats::xtabs(~ paste(drama,Number.Act,sep=sep) + ~ft[,get(column)], data=ft) }
           else { xt <- stats::xtabs(~ paste(drama,Number.Act,Speaker.figure_id,sep=sep) + ~ft[,get(column)], data=ft) }
         },
         Scene = {
           if (byCharacter == FALSE) { xt <- stats::xtabs(~ paste(drama,Number.Act,Number.Scene,sep=sep) + ~ft[,get(column)], data=ft) }
           else { xt <- stats::xtabs(~ paste(drama,Number.Act,Number.Scene,Speaker.figure_id,sep=sep) + ~ft[,get(column)], data=ft) }
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

get_highest_freq <- function(text, number = 10, lemmas_to_remove = NULL) {
  text %>% dplyr::count(Token.lemma, sort = TRUE) %>% top_n(n = number, wt = n)
}

load("data/ldaModels.RData")

# load SentiWS
pos.words <- readAndflattenSentiWS("data/SentiWS_v2.0_Positive.txt") # https://wortschatz.uni-leipzig.de/de/download
neg.words <- readAndflattenSentiWS("data/SentiWS_v2.0_Negative.txt")

postags.verbs <- postags$de$words[startsWith(postags$de$words, "V")]

wf <- loadFields(fieldnames=c("Liebe","Familie","Krieg","Ratio","Religion","Politik","Wirtschaft"), baseurl = "data/fields_enriched/")
prose <- scan("data/prose.txt", character(), quote = "")

ids.all <- ids

remove <- c("ger000497")
ids.all <- ids.all[!ids.all %in% remove]

# Get the verbs with the overall highest tfidf"
lemmas_to_remove_utt <- c("weiß", "–")
lemmas_to_remove_sd <- c()
max_freq <- 10
verbs.utt <- dramas$text[startsWith(as.character(Token.pos), "VV"), ]
verbs.utt <- verbs.utt[!verbs.utt$Token.lemma %in% lemmas_to_remove_utt,]
verbs.utt <- get_highest_freq(verbs.utt, n = max_freq)
verbs.sd <- dramas$stageDirections[startsWith(as.character(Token.pos), "VV"), ]
verbs.sd <- verbs.sd[!verbs.sd$Token.lemma %in% lemmas_to_remove_sd,]
verbs.sd <- get_highest_freq(verbs.sd, n = max_freq)

# Extract features
f <- Reduce(rbind, lapply(ids.all, function(x) {
  print(x)
  drama <- lapply(dramas, function(i) {i[i$drama==x,]})
  class(drama) <- append("QDDrama", class(drama))
  meta <- drama$meta
  char <- unique(drama$text[,c("corpus","drama","Speaker.figure_surface","Speaker.figure_id")])
  nfig <- nrow(char)
  pres <- presence(drama, passiveOnlyWhenNotActive = TRUE)
  chstat <- characterStatistics(drama)
  chstat$TTR <- sapply(chstat$character, 
                       function(x) (ttr(drama$text[Speaker.figure_id==x,]$Token.surface, w = 200)))
  ft <- frequencytable(drama, column="Token.lemma",normalize = FALSE, byCharacter = TRUE)
  conf <- configuration(drama, segment = "Scene", onlyPresence = TRUE)
  copr <- as.matrix(conf) %*% t(as.matrix(conf))
  rownames(copr) <- conf$character
  colnames(copr) <- conf$character

  verbsUtt <- as.data.frame(frequencytable(drama, byCharacter = TRUE, acceptedPOS = postags.verbs))
  verbsUtt <- setDT(verbsUtt, keep.rownames = TRUE)[]
  setnames(verbsUtt, "rn", "character")
  verbsUtt$corpus <- unique(drama$text$corpus)
  verbsUtt$drama <- sapply(strsplit(verbsUtt$character, split='|', fixed=TRUE), function(x) (x[1]))
  verbsUtt$character <- sapply(strsplit(verbsUtt$character, split='|', fixed=TRUE), function(x) (x[2]))
  verbsUtt <- verbsUtt[,names(verbsUtt) %in% c("corpus", "drama", "character", verbs.utt$Token.lemma), with = F]
  verbsUtt.meta <- verbsUtt[, c("corpus", "drama", "character")]
  verbsUtt <- merge(chstat[, c("corpus", "drama", "character", "tokens")], verbsUtt)
  verbsUtt <- verbsUtt[, 5:length(verbsUtt)] / verbsUtt$tokens
  missing_verbs <- verbs.utt$Token.lemma[!verbs.utt$Token.lemma %in% 
                                          names(verbsUtt)]
  for (v in missing_verbs) {
    verbsUtt[, v] <- 0
  }
  names(verbsUtt) <- paste0("utt.", names(verbsUtt)) 
  verbsUtt <- cbind(verbsUtt.meta, verbsUtt)
  
  verbsSd <- as.data.frame(frequencytableSd(drama, byCharacter = TRUE, acceptedPOS = postags.verbs))
  verbsSd <- setDT(verbsSd, keep.rownames = TRUE)[]
  setnames(verbsSd, "rn", "character")
  verbsSd$corpus <- unique(drama$text$corpus)
  verbsSd$drama <- sapply(strsplit(verbsSd$character, split='|', fixed=TRUE), function(x) (x[1]))
  verbsSd$character <- sapply(strsplit(verbsSd$character, split='|', fixed=TRUE), function(x) (x[2]))
  verbsSd <- verbsSd[,names(verbsSd) %in% c("corpus", "drama", "character", verbs.sd$Token.lemma), with = F]
  verbsSd.meta <- verbsSd[, c("corpus", "drama", "character")]
  verbsSd <- merge(chstat[, c("corpus", "drama", "character", "tokens")], verbsSd)
  missing_verbs <- verbs.sd$Token.lemma[!verbs.sd$Token.lemma %in% 
                                          names(verbsSd)]
  if (nrow(verbsSd) == 0) {
    verbsSd <- verbsSd.meta
    verbsSd$tokens <- 1
  }
  for (v in missing_verbs) {
    verbsSd[, v] <- 0
  }
  names(verbsSd)[5:length(verbsSd)] <- paste0("sd.", names(verbsSd)[5:length(verbsSd)]) 
  verbsSd <- verbsSd[, 5:length(verbsSd)] / verbsSd$tokens
  verbsSd <- cbind(verbsSd.meta, verbsSd)
  
  confAct <- configuration(drama, segment = "Act", onlyPresence = TRUE)
  lastAct <- cbind(confAct$drama, character = as.character(confAct$character), lastAct = as.numeric(as.matrix(confAct)[,ncol(as.matrix(confAct))]))
  
  sentiment.tokens <- drama$text[,c("corpus","drama","Speaker.figure_id","Token.surface","Token.pos")]
  # Bring PoS into format of SentiWS
  sentiment.tokens$Token.pos <- gsub("ADJA", "ADJX", sentiment.tokens$Token.pos, fixed = TRUE)
  sentiment.tokens$Token.pos <- gsub("ADJD", "ADJX", sentiment.tokens$Token.pos, fixed = TRUE)
  sentiment.tokens$Token.pos <- gsub("VVFIN", "VVINF", sentiment.tokens$Token.pos, fixed = TRUE)
  sentiment.tokens$Token.pos <- gsub("VVIZU", "VVINF", sentiment.tokens$Token.pos, fixed = TRUE)
  sentiment.tokens$Token.pos <- gsub("VVIMP", "VVINF", sentiment.tokens$Token.pos, fixed = TRUE)
  sentiment.tokens$Token.pos <- gsub("VVPP", "VVINF", sentiment.tokens$Token.pos, fixed = TRUE)
  # Classify tokens for sentiment
  sentiment <- score.sentiment(paste(sentiment.tokens$Token.surface, 
                                     sentiment.tokens$Token.pos, 
                                     sep = "|"),
                               pos.words, neg.words)
  sentiment.tokens$sentiment.pos <- sentiment$pos.score
  sentiment.tokens$sentiment.neg <- sentiment$neg.score
  sentiments <- sentiment.tokens %>% 
    dplyr::group_by(corpus, drama, Speaker.figure_id) %>% 
    dplyr::summarize(pos = sum(sentiment.pos), neg = sum(sentiment.neg),
                     .groups="keep")
  
  g <- graph_from_adjacency_matrix(copr, mode="upper", diag=FALSE, weighted = TRUE)
  dg <- degree(g, normalized = TRUE)
  wdg <- strength(g)
  bt <- betweenness(g, normalized = TRUE)
  cn <- closeness(g, normalized = TRUE)
  ev <- eigen_centrality(g)
  post <- posterior(ldaModels$lda_20, ft)$topics
  post.df <- data.frame(character = unlist(sapply(strsplit(rownames(post), "|", fixed = TRUE), '[', 2)), post)
  dstat <- dictionaryStatistics(drama, fields = wf, normalizeByCharacter = TRUE, normalizeByField = TRUE)
  features <- merge(pres, char, 
                by.x=c("corpus","drama","character"),
                by.y=c("corpus","drama","Speaker.figure_id"))
  features <- merge(features, sentiments,
                by.x=c("corpus","drama","character"),
                by.y=c("corpus","drama","Speaker.figure_id"))
  features$nfig <- rep(nfig, nrow(features))
  features$length <- unique(drama$text$length)
  features <- merge(features, meta, by = c("corpus", "drama"))
  features <- merge(features, lastAct)
  features <- merge(features, chstat)
  features <- merge(features, dstat)
  features <- merge(features, post.df, by=c("character"))
  features <- merge(features, data.frame(degree=dg,character=names(dg)))
  features <- merge(features, data.frame(wdegree=wdg,character=names(wdg)))
  features <- merge(features, data.frame(between=bt,character=names(bt)))
  features <- merge(features, data.frame(close=cn,character=names(cn)))
  features <- merge(features, data.frame(eigen=ev$vector,character=names(ev$vector)))
  features <- merge(features, verbsUtt, by = c("corpus", "drama", "character"))
  features <- merge(features, verbsSd, by = c("corpus", "drama", "character"), all.x = TRUE)
  features[is.na(features)] <- 0
  features
}))
f <- merge(f, types_schemer, by = c("drama", "character"), all.x = TRUE)
f$type <- ifelse(is.na(f$type), "Non-Schemer", "Schemer")
f$V1 <- NULL
f$posRatio <- f$pos/f$tokens
f$negRatio <- f$neg/f$tokens

f$prose <- ifelse(f$drama %in% prose, 1, 0)

# Manually add missing date information
f[f$drama=="qkmq.0",]$Date.Printed <- "1785"
f[f$drama=="qkmq.0",]$Date.Premiere <- "1785"
f[f$drama=="qkn4.0",]$Date.Printed <- "1796"
f[f$drama=="qkn4.0",]$Date.Premiere <- "1796"
f[f$drama=="r134.0",]$Date.Written <- "1775"
f[f$drama=="r134.0",]$Date.Printed <- "1776"
f[f$drama=="rfxf.0",]$Date.Printed <- "1776"
f[f$drama=="rfxf.0",]$Date.Premiere <- "1776"
f[f$drama=="rksp.0",]$Date.Printed <- "1772"
f[f$drama=="rksp.0",]$Date.Premiere <- "1772"

f$Date <- pmin(na_if(f$Date.Premiere, 0), na_if(f$Date.Printed, 0), 
              na_if(f$Date.Translation, 0), na_if(f$Date.Written, 0), 
              na.rm = TRUE)
f$Date <- as.numeric(f$Date)
f$Decade <- as.numeric(sapply(f$Date, function(x) {gsub("(.{3}).", "\\10", x)}))

f <- f[!is.na(f$type), ]
f[is.na(f)] <- 0
levels(f$type) <- unique(f$type)
f$type <- as.factor(f$type)
f$drama <- as.factor(f$drama)

names(f) <- gsub("X", "T", names(f), fixed = TRUE)

# Self-made unit test
if (length(unique(f$drama)) != 38) {
  print(length(unique(f$drama)))
  print(setdiff(unique(types_schemer$drama), unique(f$drama)))
  stop("Something went wrong, some plays are missing.")
}
if (nrow(f[f$type=="Schemer",]) != 50) {
  print(nrow(f[f$type=="Schemer",]))
  print(setdiff(unique(paste(types_schemer$drama, types_schemer$character)), 
                unique(paste(f$drama, f$character))))
  stop("Something went wrong, some schemers are missing.")
}
if (length(unique(paste0(f$drama,f$character))) != 848) {
  print(length(unique(paste0(f$drama,f$character))))
  print(setdiff(unique(paste(dramas$characters$drama, dramas$characters$figure_id)), 
                unique(paste(f$drama, f$character))))
  stop("Something went wrong, some characters are missing.")
}

save(f, file="data/f_schemer_new.RData")
