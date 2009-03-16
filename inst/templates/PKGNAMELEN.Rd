\name{#PKGNAME#LEN}
\alias{#PKGNAME#LEN}
\title{Map Protein Identifiers to the Length of Protein Sequence}
\description{
  #PKGNAME#LEN is an R environment that provides mappings between
  protein identifiers and length of protein sequence. 
}
\details{
  Each protein identifier is mapped to the length for the
  protein sequence.
  
  Mappings were based on data provided by: 
  #SOURCE#
  
  Package built #DATE#
}
\examples{
    # Convert the environment to a list
    xx <- as.list(#PKGNAME#LEN)
    if(length(xx) > 0){
        # The length for the first two elements of XX
        xx[1:2]
        # Get the first one
        xx[[1]]
    }
}

\keyword{datasets}

