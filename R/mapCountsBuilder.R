## This will at least create a map_counts table
## This function should be added wherever map_counts is needed.
buildMapCounts <- function(db, countFields){
  sql<-"  CREATE TABLE map_counts (
      map_name VARCHAR(80) PRIMARY KEY,
      count INTEGER NOT NULL
    );"
  dbSendQuery(db, sql)
  for(i in seq_len(dim(countFields)[1])){
    sql <- paste("INSERT INTO map_counts SELECT '",
                 countFields[i,1],"', count(DISTINCT ",
                 countFields[i,2],") FROM ",
                 countFields[i,3],";", sep="")
    dbSendQuery(db, sql)
  }
}
