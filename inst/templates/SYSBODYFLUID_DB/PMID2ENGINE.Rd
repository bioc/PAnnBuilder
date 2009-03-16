\name{#PREFIX#PMID2ENGINE}
\alias{#PREFIX#PMID2ENGINE}
\title{Map published paper to search engine}
\description{
  #PREFIX#PMID2ENGINE is an R environment that provides mappings between
  public peer-reviewed papers collected in Sys-BodyFluid and protein search engines. 
}
\details{
  Frequentlly used search engines include SEQUEST, Mascot, and so on.
  
  Mappings were based on data provided by: Sys-BodyFluid (\url{#PAPERSOURCEURL#})
  on #PAPERSOURCEDATE#  
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#PMID2ENGINE)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){         
         # Get the first one
         xx[[1]]
     }
}

