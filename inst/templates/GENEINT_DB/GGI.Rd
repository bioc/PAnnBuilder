\name{#PREFIX#GGI}
\alias{#PREFIX#GGI}
\title{Gene-Gene interaction}
\description{
  #PREFIX#GGI gives gene-gene interactions from NCBI database.
}
\details{
  Mappings were based on data provided by: NCBI (\url{#GENEINTSOURCEURL#}) on
  #GENEINTSOURCEDATE# 
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#GGI)     
     if(length(xx) > 0){
         # Gets the first five interaction list
         xx[1:5]
     }
}
