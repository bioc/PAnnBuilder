\name{#PKGNAME#}
\alias{#PKGNAME#}
\title{annotation data package}
\description{  
  Welcome to the #PKGNAME# annotation Package. 
  The annotation package was built using a downloadable R package -
  PAnnBuilder (download and build your own). The purpose is to provide detailed 
  annotation for unknown proteins bases on sequence similarity: 
  #PSEQSOURCEURL#
  #PSEQSOURCEDATE#

  Each of these objects has their own manual page detailing where relevant data 
  was obtained along with examples of how to use it. Many of these objects also 
  have a reverse map available. When this is true, expect to usually find relevant 
  information on the same manual page as the forward map. 
}
\examples{
  # You can learn what data this package supports with the following command:   
  xx=dbListTables(#PREFIX#_dbconn())
  dbGetQuery(#PREFIX#_dbconn(), paste("select * from",xx[1],"limit 3") )
}
