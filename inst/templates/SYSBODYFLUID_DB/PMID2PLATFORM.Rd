\name{#PREFIX#PMID2PLATFORM}
\alias{#PREFIX#PMID2PLATFORM}
\title{Map published paper to experimental platform}
\description{
  #PREFIX#PMID2PLATFORM is an R environment that provides mappings between
  public peer-reviewed papers collected in Sys-BodyFluid and mass spectrometry 
  experimental platforms. 
}
\details{    
  Mappings were based on data provided by: Sys-BodyFluid (\url{#PAPERSOURCEURL#})
  on #PAPERSOURCEDATE#  
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#PMID2PLATFORM)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){         
         # Get the first one
         xx[[1]]
     }
}

