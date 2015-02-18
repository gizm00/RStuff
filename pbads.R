library(RMySQL)

dftop <- data.frame()

# open connection to aws db
open <- function() {
  con <- dbConnect(RMySQL::MySQL(), 
                   user = "readonly", 
                   password = "2bguxmYsYDCevEUEP33EjYWVmQyGAF", 
                   host = "recommender-snapshot-replica.c7bp7cmon09v.us-east-1.rds.amazonaws.com", 
                   dbname = "recommender")
  
  query <- function(...) dbGetQuery(con, ...)

  # reporting functions
  top10users <- function() {
    dftop10users <- data.frame(query("select user_id, count(id) 
                                     from user_action 
                                     where date_of > '2015-02-03' AND action = 'click' 
                                     group by user_id order by count(id) desc limit 10;"))
    write.csv(dftop10users, 'top10users.csv')
    print(dftop10users)
    
  }
  
  top10ads <- function() {
    dftop10ads <- data.frame(query("select item_id, count(id) 
                                   from user_action 
                                   where date_of > '2015-02-09' AND action = 'click' 
                                   group by item_id 
                                   order by count(id) desc limit 10;"))
    #write.csv(dftop10ads, 'top10ads.csv')
    print(dftop10ads)
    dftop <- dftop10ads
  }
  
  nonorganic <- function() {
    df <- data.frame(query("select user_action.item_id as 'item_id',  
                                      nonorganic_items.body as 'item_body',
                                      count(case when user_action.action = 'click' then 1 else null end) as 'click',  
                                      count(case when user_action.action = 'load' then 1 else null end) as 'load' 
                                      from user_action
                                      inner join ( 
                                        select item.id as 'id', item.body as 'body' 
                                        from item 
                                          inner join source 
                                            on item.source_id = source.id 
                                        where source.source_type not like 'organic' 
                                      ) nonorganic_items on user_action.item_id = nonorganic_items.id 
                                      where user_action.date_of > '2015-02-09'  group by user_action.item_id;"))
    print
    df['ctr'] = df['click'] / df['load']
    write.csv(df, '2_9to2_17_nonorganic.csv')

  }
  
  nonorganic_wusers <- function() {
    df <- data.frame(query("select user_action.user_id as 'user_id', user_action.item_id as 'item_id',  
                                      nonorganic_items.body as 'item_body',
                                      count(case when user_action.action = 'click' then 1 else null end) as 'click',  
                                      count(case when user_action.action = 'load' then 1 else null end) as 'load' 
                                      from user_action
                                      inner join ( 
                                        select item.id as 'id', item.body as 'body' 
                                        from item 
                                          inner join source 
                                            on item.source_id = source.id 
                                        where source.source_type not like 'organic' 
                                      ) nonorganic_items on user_action.item_id = nonorganic_items.id 
                                      where user_action.date_of > '2015-02-09' 
                                      and user_action.date_of < '2015-02-17'
                                      group by user_id, item_id;"))
    df['ctr'] = df['click'] / df['load']
    write.csv(df, '2_9to2_17_nonorganic_usersitems.csv')
  }
  
  # close db connection
  disConn <- function() {
    dbDisconnect(con)
  }
  
  nonorganic_wusers()
  #top10ads()
  disConn()
  
}

open()