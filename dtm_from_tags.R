library(tm)
myReader <- readTabular(mapping=list(content="label", id="id"))

makeCorpus <- function(df) {
  #df$labelClean <- gsub("[^[:alnum:]///' ]", "", df$label)
  tm <- VCorpus(DataframeSource(df), readerControl=list(reader=myReader))
}

getData <- function(df) {
  labels <- gsub("[^[:alnum:]///' ]", "", df$label)
  ids <- df$id
  dfNew <- data.frame("label" = labels, "id" = ids)
}

dfListTags <- head(dfTags)
testDfList <- lapply(dfListTags, getData)
testCorpusList <- lapply(testDfList, makeCorpus)
allCorpi <- do.call(function(...) c(..., recursive = TRUE), testCorpusList)
testDtm <- DocumentTermMatrix(allCorpi)