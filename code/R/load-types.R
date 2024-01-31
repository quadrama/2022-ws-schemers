library(data.table)

read_types <- function(file) {
  types <- data.table(read.csv(file, header = FALSE))
  types <- melt(types, id.vars = c("V1", "V2"), na.rm = TRUE)
  types <- types[value != "",]
  colnames(types) <- c("drama", "character", "variable", "type")
  types <- types[with(types, order(drama, character)), ]
  types$variable <- NULL
  return(types)
}
types_schemer <- read_types("data/Schemer_new.csv")
