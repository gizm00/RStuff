library(tm)

'%nin%' <- Negate('%in%')
stopw <- c(stopwords(), "can")
getFeatures <- function(dfText) {
  # extract stopwords
  dfRmStop <- apply(dfText, 1, function(x) {
    clean <- removePunctuation(tolower(x))
    cleaner <- gsub('\n', " ", clean)
    ret <- unlist(strsplit(cleaner, " "))
    tab <-table(sapply(ret[ret %nin% stopw], "["))

    y <- data.frame(tab)
    ordered <- y[order(-y['Freq']),]
    ordered[1:10, 'Var1']
    
    
    
    # count words
    
  } )
  
  # count words
  dfRmStop
  
}
