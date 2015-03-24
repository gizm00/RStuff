library(caret)
library(plyr)
library(dplyr)
library(tm)
library(randomForest)

source('../tfidf_script.R')

actions <- read.table('actions60k.csv')
towerdata <- read.csv("towerdata_results.csv")
towerdata <- towerdata[,c(1:14)] # Match to columns of unclicking users
towerdata <- rbind(towerdata, read.csv("towerdata_unclicking_users.csv"))

test_users <- c(-1,
                802656650,
                1392700475,
                333078593,
                1489685196,
                402766677,
                399496632,
                1613170246,
                1917899305,
                110002647,
                2051322685,
                1098598605,
                1709319647,
                1647294775,
                2137965355,
                1398191645,
                1419606473,
                1607762513,
                1260676593,
                601921483,
                497502960,
                756975310,
                1305243554,
                2003726590,
                1252548512,
                1582301942,
                1840944244,
                376647365,
                865479479,
                1539796513,
                1446169574,
                2082908033,
                1771155806,
                816809459,
                14689899,
                922945981,
                535623332,
                523239511,
                31260123,
                268821667,
                383659977,
                1564166284,
                145583762)

getTfidf <- function() {
  dfAllAdTitles <- read.csv('../allAdTitles.csv')
  dfunique <- subset(dfAllAdTitles, !duplicated(dfAllAdTitles$id))
  dfunique <- dplyr::rename(dfunique, body = title)
  tfidfResS99 <- tfidf2(dfunique, withStem = FALSE, sparse=0.99)
}

setUsers <- function(dfclusters) {
  actions$cluster <- dfclusters[as.character(actions$item_id),]
  users <- filter(actions, !user_id %in% test_users, user_id %in% towerdata$userid) %>%
    group_by(user_id) %>%
    select(action, cluster) %>%
    summarize(
      loads.c1 = sum(ifelse(action == 'load' & cluster == 1, 1 ,0)),
      loads.c2 = sum(ifelse(action == 'load' & cluster == 2, 1 ,0)),
      loads.c3 = sum(ifelse(action == 'load' & cluster == 3, 1 ,0)),
      loads.c4 = sum(ifelse(action == 'load' & cluster == 4, 1 ,0)),
      loads.c5 = sum(ifelse(action == 'load' & cluster == 5, 1 ,0)),
      loads.c6 = sum(ifelse(action == 'load' & cluster == 6, 1 ,0)),
      loads.c7 = sum(ifelse(action == 'load' & cluster == 7, 1 ,0)),
      loads.c8 = sum(ifelse(action == 'load' & cluster == 8, 1 ,0)),
      loads.c9 = sum(ifelse(action == 'load' & cluster == 9, 1 ,0)),
      loads.c10 = sum(ifelse(action == 'load' & cluster == 10, 1 ,0)),
      loads.c11 = sum(ifelse(action == 'load' & cluster == 11, 1 ,0)),
      loads.c12 = sum(ifelse(action == 'load' & cluster == 12, 1 ,0)),
      loads.c13 = sum(ifelse(action == 'load' & cluster == 13, 1 ,0)),
      loads.c14 = sum(ifelse(action == 'load' & cluster == 14, 1 ,0)),
      loads.c15 = sum(ifelse(action == 'load' & cluster == 15, 1 ,0)),
      loads.c16 = sum(ifelse(action == 'load' & cluster == 16, 1 ,0)),
      loads.c17 = sum(ifelse(action == 'load' & cluster == 17, 1 ,0)),
      loads.c18 = sum(ifelse(action == 'load' & cluster == 18, 1 ,0)),
      loads.c19 = sum(ifelse(action == 'load' & cluster == 19, 1 ,0)),
      loads.c20 = sum(ifelse(action == 'load' & cluster == 20, 1 ,0)),
      loads.c21 = sum(ifelse(action == 'load' & cluster == 21, 1 ,0)),
      loads.c22 = sum(ifelse(action == 'load' & cluster == 22, 1 ,0)),
      loads.c23 = sum(ifelse(action == 'load' & cluster == 23, 1 ,0)),
      loads.c24 = sum(ifelse(action == 'load' & cluster == 24, 1 ,0)),
      loads.c25 = sum(ifelse(action == 'load' & cluster == 25, 1 ,0)),
      clicks.c1 = sum(ifelse(action == 'click' & cluster == 1, 1, 0)),
      clicks.c2 = sum(ifelse(action == 'click' & cluster == 2, 1, 0)),
      clicks.c3 = sum(ifelse(action == 'click' & cluster == 3, 1, 0)),
      clicks.c4 = sum(ifelse(action == 'click' & cluster == 4, 1, 0)),
      clicks.c5 = sum(ifelse(action == 'click' & cluster == 5, 1, 0)),
      clicks.c6 = sum(ifelse(action == 'click' & cluster == 6, 1, 0)),
      clicks.c7 = sum(ifelse(action == 'click' & cluster == 7, 1, 0)),
      clicks.c8 = sum(ifelse(action == 'click' & cluster == 8, 1, 0)),
      clicks.c9 = sum(ifelse(action == 'click' & cluster == 9, 1, 0)),
      clicks.c10 = sum(ifelse(action == 'click' & cluster == 10, 1, 0)),
      clicks.c11 = sum(ifelse(action == 'click' & cluster == 11, 1, 0)),
      clicks.c12 = sum(ifelse(action == 'click' & cluster == 12, 1, 0)),
      clicks.c13 = sum(ifelse(action == 'click' & cluster == 13, 1, 0)),
      clicks.c14 = sum(ifelse(action == 'click' & cluster == 14, 1, 0)),
      clicks.c15 = sum(ifelse(action == 'click' & cluster == 15, 1, 0)),
      clicks.c16 = sum(ifelse(action == 'click' & cluster == 16, 1, 0)),
      clicks.c17 = sum(ifelse(action == 'click' & cluster == 17, 1, 0)),
      clicks.c18 = sum(ifelse(action == 'click' & cluster == 18, 1, 0)),
      clicks.c19 = sum(ifelse(action == 'click' & cluster == 19, 1, 0)),
      clicks.c20 = sum(ifelse(action == 'click' & cluster == 20, 1, 0)),
      clicks.c21 = sum(ifelse(action == 'click' & cluster == 21, 1, 0)),
      clicks.c22 = sum(ifelse(action == 'click' & cluster == 22, 1, 0)),
      clicks.c23 = sum(ifelse(action == 'click' & cluster == 23, 1, 0)),
      clicks.c24 = sum(ifelse(action == 'click' & cluster == 24, 1, 0)),
      clicks.c25 = sum(ifelse(action == 'click' & cluster == 25, 1, 0)),
      clicks = sum(ifelse(action == 'click', 1, 0)),
      loads = sum(ifelse(action == 'load', 1, 0)),
      ctr = clicks / loads) %>%
    rename(userid = user_id)
    users <- filter(users, is.finite(users$ctr))
}


createTowerSet <- function(users, dfkmeans) {
  tower.set <- merge(users, towerdata)
  tower.set.attrs <- c("ctr", "Age", "Gender", "Household.Income")
  
  #find max cluster position
  catchAll <- which(dfkmeans$size == max(dfkmeans$size))
  print(dfkmeans$size)
  tower.set.cluster <- ""
  for (i in 1:length(dfkmeans$size)) {
    if (i != catchAll)
      tower.set.cluster <- c(tower.set.cluster, paste0("clicks.c", i), paste0("loads.c", i))
  }
  tower.set.attrs <- c(tower.set.attrs, tower.set.cluster)
  print(tower.set.attrs)
  
  tower.set <- tower.set[names(tower.set) %in% tower.set.attrs]
  tower.set[tower.set$Gender=="",]$Gender=NA
  tower.set[tower.set$Age=="",]$Age=NA
  tower.set <- tower.set[complete.cases(tower.set),]
}

runRandomForests <- function(tower.set, seedVal) {
  set.seed(seedVal)
  frac <- 0.7
  
  inTrain <- createDataPartition(y=tower.set$ctr, p=frac, list=FALSE)
  tower.train <- tower.set[inTrain,]
  tower.test <- tower.set[-inTrain,]
  system.time(
    #model.tower <- train(ctr ~ ., data = tower.train, 'parRF', na.action=na.omit))
    model.tower <- randomForest(ctr ~ ., data = tower.train))
  tower.test$predicted <- predict(model.tower, tower.test, na.action = na.omit)
  print(seedVal)
  sqrt(mean((tower.test$predicted - tower.test$ctr) ^ 2, na.rm = TRUE))
}

runAnalysis <- function(kstart, kend, kstep) {
  dfreturn <- data.frame("k" = seq(kstart,kend,kstep), "rsme_9" = 0*seq(kstart,kend,kstep),
                         "rsme_4" = 0*seq(kstart,kend,kstep))
  tfidfRun = getTfidf()

  for (i in seq(kstart,kend,kstep)) {
    
    #compute kmeans
    dfkmeans <- computeSingleKmeans(tfidfRun$terms, i)
    print("got kmeans")
    dfcluster <- data.frame("cluster" = dfkmeans$cluster)
    rownames(dfcluster) <- tfidfRun$ids
    gc()
    
    # setup user data with cluster info
    users <- setUsers(dfcluster)
    print("got users")
    tower.set <- createTowerSet(users, dfkmeans)
    print("got tower.set")
    gc()
    
    # run random forests on 2 seeds
    dfreturn[dfreturn$k == i, 'rsme_9'] = runRandomForests(tower.set, 9)
    dfreturn[dfreturn$k == i, 'rsme_4'] = runRandomForests(tower.set, 4)
    print(dfreturn[,c("rsme_9", "rsme_4")])
  }
  save(dfreturn, file='../kmeans_rf_run_3_24.RData')
}

runAnalysis(5,25,5)