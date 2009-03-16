\name{#PREFIX#_dbconn}
\alias{#PREFIX#_dbconn}
\alias{#PREFIX#_dbfile}
\alias{#PREFIX#_dbInfo}
\title{Collect information about the package annotation DB}
\description{  
  Some convenience functions for getting a connection object to (or collecting 
  information about) the package annotation DB. 
}
\usage{
  #PREFIX#_dbconn()
  #PREFIX#_dbfile()
  #PREFIX#_dbInfo()
}
\arguments{ 
}
\details{
  \code{#PREFIX#_dbconn} returns a connection object to the package annotation DB. 
  IMPORTANT: Don't call \code{\link[DBI]{dbDisconnect}} on the connection object 
  returned by \code{#PREFIX#_dbconn} or you will break all the 
  \code{\link[AnnotationDbi]{AnnDbObj}} objects defined in this package! 

  \code{#PREFIX#_dbfile} returns the path (character string) to the package
  annotation DB (this is an SQLite file). 
 
  \code{#PREFIX#_dbInfo} prints other information about the package annotation
  DB. 
}
\examples{
  ## The connection object returned by #PREFIX#_dbconn() was created with:
  dbConnect(SQLite(), dbname=#PREFIX#_dbfile(), cache_size=64000, synchronous=0)

  #PREFIX#_dbInfo()
}
