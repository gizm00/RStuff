library(tm)
library(plyr)

numWords <- function(x) sapply(gregexpr("\\W+", trim(x)), length) + 1

run <- function() {
  df <- read.csv('ads_02_09_to_02_16_annotated_500.csv')
  
  # get the news ads that have > 10 words
  df <- df[df$category_major != "",]
  news_ads <- df[df$category_major == 'news',]
  news_ads_with_text <- news_ads[numWords(news_ads$item_body) > 10,]
  
  # get ad bodies and ad ids
  news_ad_ids <- as.list(news_ads_with_text[['item_id']])
  news_ad_bodies <- as.list(news_ads_with_text[['item_body']])
  
  # convert to corpus
  list_news_corpora <- lapply(1:length(news_ad_bodies), function(i) Corpus(VectorSource(news_ad_bodies[[i]])))
  
  # clean text
  skipWords <- function(x) removeWords(x, c(stopwords("english"), "can", "the", "but", "from", "was", "really", "wait"))
  funcs <- list(stripWhitespace, removeNumbers, skipWords, removePunctuation, content_transformer(tolower))
  list_news_corpora1 <- lapply(1:length(list_news_corpora), function(i) tm_map(list_news_corpora[[i]], FUN = tm_reduce, tmFuns = funcs))
  
  #convert to TDM
  list_news_tdms <- lapply(1:length(list_news_corpora1), function(i) TermDocumentMatrix(list_news_corpora1[[i]], control=list(wordLengths = c(3,10))))
  
  # get 10 most frequently occuring words
  top_words <- lapply(1:length(list_news_tdms), function(i) sort(rowSums(as.matrix(list_news_tdms[[i]])), decreasing=TRUE))
  top_words_df <- t(ldply(1:length(top_words), function(i)  head(names(top_words[[i]]),10)))
  
  # danger ! assumes that the ordering has been preserved - labeling with the news_ad_ids
  # corresponding to the index of the top word lists
  colnames(top_words_df) <- lapply(1:length(list_news_tdms), function(i) news_ad_ids[[i]])
  top_words_df
}
