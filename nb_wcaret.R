library("klaR")
library("caret")
library("tm")

# enhanced stopword list
skipWords <- function(x) removeWords(x, c(stopwords("english"), "can", "the", "but", "from", "was", "really", "wait"))

# bagit - generate a labeled DTM for a body of texts
# input - dataframe with fields $item_body (text description of ad)
#                                and $category_major (label)
# output - dataframe including the DTM and labels in $cat
bagit <- function(df, booleanify=FALSE) {
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
  
  if (booleanify) {
    # convert all 0s to false and all 1 or > 1 to true
    labeledTerms[,] <- ifelse(labeledTerms>=1,1,0)
  }
  
  labeledTerms$cat_ = df$category_major
  print (head(labeledTerms))
  
  # return labeled DTM
  labeledTerms
}

trainit <- function() {
  df <- read.csv('ads_02_09_to_02_16_annotated_500.csv')
  ads <- df[df$category_major == 'news' | df$category_major == 'sports',]
  
  # refactor to avoid extraneous levels
  ads$category_major <- factor(ads$category_major)
  print(c("ads factors:", levels(ads$category_major)))
  
  # create bag of words. category output in ads_bag$cat_
  ads_bag <- bagit(ads, TRUE)
  print("bagged words")
  
  # create training and testing partitions
  trainIndex <- createDataPartition(ads_bag$cat_, p=0.8, list=FALSE)
  data_train <- ads_bag[ trainIndex,]
  data_test <- ads_bag[-trainIndex,]
  
  # assume y variable (cat_) is the last column
  y_train <- as.factor(data_train[,ncol(data_train)])
  x_train <- data_train[,-ncol(data_train)]
  
  #generate model
  print("model generation")
  model <- train(x_train, y_train, method="nb")
  
  #predict based on test set
  print("prediction")
  predictions <- predict(model, data_test[,-ncol(data_test)])
  
  #calculate confusion matrix
  #confusionMatrix <- confusionMatrix(predictions, data_test$cat_)
  
  # return list of items
  list("train" = data_train, "test" = data_test, "model" = model, "pred" = predictions)
}