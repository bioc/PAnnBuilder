\name{#PREFIX#PMID2SAMPLE}
\alias{#PREFIX#PMID2SAMPLE}
\title{Map published paper to experimental sample}
\description{
  #PREFIX#PMID2SAMPLE is an R environment that provides mappings between
  public peer-reviewed papers collected in Sys-BodyFluid and experimental sample. 
}
\details{
  Mappings were based on data provided by: Sys-BodyFluid (\url{#PAPERSOURCEURL#})
  on #PAPERSOURCEDATE#  
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#PMID2SAMPLE)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){         
         # Get the first one
         xx[[1]]
     }
}

