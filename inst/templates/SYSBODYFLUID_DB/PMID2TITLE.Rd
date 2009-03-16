\name{#PREFIX#PMID2TITLE}
\alias{#PREFIX#PMID2TITLE}
\title{Map published paper to search engine}
\description{
  #PREFIX#PMID2TITLE is an R environment that provides mappings between
  public peer-reviewed papers collected in Sys-BodyFluid and their titles. 
}
\details{
  Mappings were based on data provided by: Sys-BodyFluid (\url{#PAPERSOURCEURL#})
  on #PAPERSOURCEDATE#  
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#PMID2TITLE)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){         
         # Get the first one
         xx[[1]]
     }
}

