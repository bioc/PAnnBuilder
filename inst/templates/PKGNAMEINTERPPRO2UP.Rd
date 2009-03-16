\name{#PKGNAME#INTERPRO2UP}
\alias{#PKGNAME#INTERPRO2UP}
\title{Map InterPro identifier to UniProt Protein}
\description{
  #PKGNAME#INTERPRO2UP is an R environment that mapps InterPro identifiers to 
  UniProt protein.
}
\details{
  Each InterPro identifier is mapped to a matrix. Each row is a associated 
  protein, and the name of columns are: UniProt protein identifier, 
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
    xx <- as.list(#PKGNAME#INTERPRO2UP)
    # Remove all the NAs
    xx <- xx[!is.na(xx)]
    if(length(xx) > 0){
    	xx[1:3]
    }
}
\keyword{datasets}

