library(tm)
library(plyr)
library(skmeans)
library(SnowballC)
library(Matrix)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
numWords <- function(x) sapply(gregexpr("\\W+", trim(x)), length) + 1
skipWords <- function(x) removeWords(x, c(stopwords("english"), "can", "the", "but", "from", "was", "really", "wait"))

tfidfTest <- function(testAds, stem=FALSE) {
  vecItemBodies <- as.vector(testAds)
  corpusItemBodies <- Corpus(VectorSource(vecItemBodies))
  terms <- getTerms(corpusItemBodies, stem)
  terms
  
}

tfidf2 <- function(df = read.csv('allads.csv'), withStem = FALSE, sparse=0.9) {
  df$bodyClean <- gsub("[^[:alnum:]///' ]", "", df$body)
  vecItemBodies <- as.vector(df$bodyClean)
  vecItemIds <- as.vector(df$id)
  corpusItemBodies <- Corpus(VectorSource(vecItemBodies))
  terms <- getTerms(corpusItemBodies, withStem, sparse)
  list("terms" = terms, "ids" = vecItemIds)
  
}

getTerms <- function(corpusItemBodies, withStem=FALSE, sparse=0.9) {
  # clean corpus (enbalming?)
  # cleaning functions - evaluated RIGHT to LEFT by tm_map ! so save stripwhitespace till end (left most)
  funcs <- list(stripWhitespace, removeNumbers, skipWords, removePunctuation, content_transformer(tolower))
  
  if (withStem) {
    funcs <- list(stemDocument, stripWhitespace, removeNumbers, skipWords, removePunctuation, content_transformer(tolower))
    #corpusItemBodies <- tm_map(corpusItemBodies, stemDocument)
  }
  corpusItemBodies <- tm_map(corpusItemBodies, FUN=tm_reduce, tmFuns = funcs)
  print(class(corpusItemBodies))
  #print(as.data.frame(as.matrix(dtmCount)))
  
  # create DTM and label with category
  dtm = DocumentTermMatrix(corpusItemBodies,
                           control = list(weighting = function(x) 
                             weightTfIdf(x, normalize = FALSE)))
  
  # remove sparse terms
  gc()  #garbage collection
  terms = removeSparseTerms(dtm, sparse)
  terms = as.data.frame(as.matrix(terms))
  terms
}

findwss <- function(tfidf) {
  wss <- (nrow(tfidf)-1)*sum(apply(tfidf,2,var))
  cnt = 0;
  for (i in seq(10, 200, by=10)) {
    wss[cnt] <- sum(kmeans(tfidf, centers=i)$withinss)
    cnt = cnt + 1
  }
  wss
  
  # plot function
  # plot(1:15, wss, type="b", xlab="Number of Clusters",
  # ylab="Within groups sum of squares")
}

computeKmeans <- function(tfidf) {
  #wss <- (nrow(tfidf)-1)*sum(apply(tfidf,2,var))
  cluster <- list()
  
  cnt = 1;
  for (i in 5:15) {
    cluster[[cnt]] <- computeSingleKmeans(tfidf, i)
    
    cnt = cnt + 1
  }
  cluster
  
}

computeSingleKmeans <- function(tfidf, c=1) {
  kmeans(tfidf, centers=c)
}

computeSingleSKmeans <- function(tfidf, c=1) {
  tfidf <- tfidf[apply(tfidf[,-1], 1, function(x) !all(x==0)),]
  skmeans(tfidf, k=c, method='pclust')
}

#terms <- tfidf2(withStem = TRUE)
#kmeansNormStem100 <- computeSingleKmeans(terms, 100)
#save(terms, kmeansNormStem100, file='kmeansNormStemKeq100.RData')
