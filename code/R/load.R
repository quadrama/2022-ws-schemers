library(DramaAnalysis)
library(igraph)
library(data.table)
source("code/R/functions.R")
source("code/R/load-types.R")

ids <- unique(c(types_schemer$drama))

# Removed because no protagonists or other issues
negatives <- c()

ids <- setdiff(ids, negatives)

if (file.exists("data/d.RData") & file.exists("data/t.RData")) {
  load("data/d.RData")
  load("data/t.RData")
} else {
  dramas <- loadDrama(ids, defaultCollection = "gdc")
  dramas$text <- dramas$text[!is.na(dramas$text$Speaker.figure_id), ]
  
  texts <- dramas$text
  # we remove rows with unknown speakers
  texts <- texts[!is.na(texts$Speaker.figure_surface)]
  save(dramas, file="data/d.RData")
  save(texts, file="data/t.RData")
}