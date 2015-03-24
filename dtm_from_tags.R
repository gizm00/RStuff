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

#the above works, but what about when we have documents repeated?
#this works better!
# collapse into DF
allTags <- ldply(dfTags)

#find unique doc ids
uTags <- unique(allTags)

#profit!
uCorpus <- makeCorpus(uTags)
testDtm <- DocumentTermMatrix(uCorpus)