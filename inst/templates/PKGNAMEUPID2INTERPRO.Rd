\name{#PKGNAME#UPID2INTERPRO}
\alias{#PKGNAME#UPID2INTERPRO}
\title{Map UniProt Protein Identifier to InterPro identifier}
\description{
  #PKGNAME#UPID2INTERPRO is an R environment that mapps UniProt protein identifier
  to the associated InterPro domains.
}
\details{
  Each protein identifier is mapped to a matrix. Each row is a associated 
  InterPro domain, and the name of columns are: InterPro identifier, 
  InterPro domain description, InterPro domain sourcce, start position, 
  end position.    
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#  
}
\references{
  \url{http://www.ebi.ac.uk/interpro}
}
\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#UPID2INTERPRO)
    # Remove all the NAs
    xx <- xx[!is.na(xx)]
    if(length(xx) > 0){
    	xx[1:3]
    }
}
\keyword{datasets}

