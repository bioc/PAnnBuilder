\name{#PKGNAME#PFAM2SP}
\alias{#PKGNAME#PFAM2SP}
\title{Map Pfam identifier to SwissProt Protein}
\description{
  #PKGNAME#PFAM2SP is an R environment that mapps Pfam identifiers to SwissProt
  protein.
}
\details{
  Each Pfam identifier is mapped to a matrix. Each row is a associated 
  protein, and the name of columns are: SwissProt protein acession number, 
  SwissProt protein identifier, Pfam domain name, 
  Pfam domain description, start position, end position.      
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#  
}
\references{
  \url{http://pfam.janelia.org/}
}
\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#PFAM2SP)
    # Remove all the NAs
    xx <- xx[!is.na(xx)]
    if(length(xx) > 0){
    	xx[1]
    }
}
\keyword{datasets}

