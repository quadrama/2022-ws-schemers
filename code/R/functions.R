getLabels <- function(x,b,limit=100) {
  rl <- apply(x, 2, function(xx) {
    xx <- as.double(xx)
    cs <- c(0,head(cumsum(xx),-1))
    r <- cs + xx/2
    r[xx<=limit] <- NA
    r  
  })
  rl
}

loadFullText <- function(ids) {
  texts <- data.table::data.table(loadSegmentedText(ids, defaultCollection = "gdc"))
  mentions <- data.table::data.table(loadAnnotations(ids, type=atypes$FigureMention, coveredType = NULL, defaultCollection = "gdc"))
  data.table::setkey(texts, "corpus", "drama", "begin", "end")
  data.table::setkey(mentions, "corpus", "drama", "begin", "end")
  
  mtext <- data.table::foverlaps(mentions,
                                 texts, type="within",
                                 by.x=c("corpus", "drama", "begin", "end"), 
                                 by.y=c("corpus", "drama", "begin", "end"))
  mtext
}

removeCorpusPrefix <- function(str, prefix = "tg") {
  sapply(strsplit(str,paste0(prefix,":"), fixed = T), function(x) (if (is.na(x[2])) {x} else {x[2]}))
}

# The iml package does not make it easy to round the feature values for plotting
roundFeatures <- function(featureVector, digits) {
  features <- lapply(strsplit(featureVector, "="), function(x) x[1])
  values <- round(as.numeric(lapply(strsplit(featureVector, "="), function(x) x[2])), digits = digits)
  paste0(features,"=",values)
}

# Calc Matthew's Correlation Coefficient
MCC <- function(TN, FP, FN, TP) {
  (TP*TN - FP*FN)/(sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
}
