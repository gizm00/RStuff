library(tm)
library(e1071)

# enhanced stopword list
skipWords <- function(x) removeWords(x, c(stopwords("english"), "can", "the", "but", "from", "was", "really", "wait"))

# bagit - generate a labeled DTM for a body of texts
# input - dataframe with fields $item_body (text description of ad)
#                                and $category_major (label)
# output - dataframe including the DTM and labels in $cat
bagit <- function(df) {
  # create corpus
  vecItemBodies <- as.vector(df$item_body)
  corpusItemBodies <- Corpus(VectorSource(vecItemBodies))
  
  # clean corpus (enbalming?)
  # cleaning functions - evaluated RIGHT to LEFT by tm_map ! so save stripwhitespace till end (left most)
  funcs <- list(stripWhitespace, removeNumbers, skipWords, removePunctuation, content_transformer(tolower))
  corpusItemBodies <- tm_map(corpusItemBodies, FUN=tm_reduce, tmFuns = funcs)
  
  # create DTM and label with category
  dtm = DocumentTermMatrix(corpusItemBodies)
  labeledTerms = as.data.frame(as.matrix(dtm))
  labeledTerms$cat = df$category_major
  
  # return labeled DTM
  labeledTerms
}
  
# modelit - runs NB model on labeled DTM
# input - df DTM with labels in $cat
# output - NB model
modelit <- function(df) {

  m <- naiveBayes(df, df$cat)
  m
  
}

# runAnalysis - pre process text, create DTM, generate training
# and test sets, create a model based on the training set and
# predict labels of the test set
# input - dataframe with items_body text and $category_major labels
# output - table of predicted categories (rows)
# vs labeled categories (columns)
runAnalysis <- function(df) {
  # create bag of words from ads in dataframe
  adsBag <- bagit(df)
  
  # setup training and test sets
  set.seed(1)
  smp_size <- floor(0.7 * nrow(adsBag))
  train_ind <-sample(seq_len(nrow(adsBag)), size = smp_size)
  train <- adsBag[train_ind,]
  test <- adsBag[-train_ind,]
  
  # create model 
  mod <- modelit(train)
  
  # predict!
  table(predict(mod, test), test$cat)
}