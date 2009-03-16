\name{#PREFIX#_dbconn}
\alias{#PREFIX#_dbconn}
\alias{#PREFIX#_dbfile}
\alias{#PREFIX#_dbschema}
\alias{#PREFIX#_dbInfo}
\title{Collect information about the package annotation DB}
\description{  
  Some convenience functions for getting a connection object to (or collecting 
  information about) the package annotation DB. 
}
\usage{
  #PREFIX#_dbconn()
  #PREFIX#_dbfile()
  #PREFIX#_dbschema(file="", show.indices=FALSE)
  #PREFIX#_dbInfo()
}
\arguments{ 
  \item{file}{A connection, or a character string naming the file to print to 
    (see the file argument of the cat function for the details). }
  \item{show.indices}{The CREATE INDEX statements are not shown by default. Use 
    show.indices=TRUE to get them. }
}
\details{
  \code{#PREFIX#_dbconn} returns a connection object to the package annotation DB. 
  IMPORTANT: Don't call \code{\link[DBI]{dbDisconnect}} on the connection object 
  returned by \code{#PREFIX#_dbconn} or you will break all the 
  \code{\link[AnnotationDbi]{AnnDbObj}} objects defined in this package! 

  \code{#PREFIX#_dbfile} returns the path (character string) to the package
  annotation DB (this is an SQLite file). 

  \code{#PREFIX#_dbschema} prints the schema definition of the package annotation
  DB. 

  \code{#PREFIX#_dbInfo} prints other information about the package annotation
  DB. 
}
\examples{
  ## Show the first three rows.
  dbGetQuery(#PREFIX#_dbconn(), "select * from geneint limit 3")

  ## The connection object returned by #PREFIX#_dbconn() was created with:
  dbConnect(SQLite(), dbname=#PREFIX#_dbfile(), cache_size=64000, synchronous=0)

  #PREFIX#_dbschema()

  #PREFIX#_dbInfo()
}
