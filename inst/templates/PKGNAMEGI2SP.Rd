\name{#PKGNAME#GI2SP}
\alias{#PKGNAME#GI2SP}
\title{An annotation data file that maps GI of Refseq proteins to
  Swiss-Prot protein identifiers.} 
\description{
  #PKGNAME#GI2SP maps GI of Refseq proteins to Swiss-Prot protein identifiers.
}
\details{
  This is an environment object containing key and value pairs. Keys is
  GI protein identifier and value is a vector of Swiss-Prot protein identifier.

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#GI2SP)
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
		# Get the values for a few keys
		if(length(xx) >= 3){
			xx[1:3]
		}
	}
}
\keyword{datasets}

