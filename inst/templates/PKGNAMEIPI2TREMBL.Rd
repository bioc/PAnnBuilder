\name{#PKGNAME#IPI2TREBML}
\alias{#PKGNAME#IPI2TREBML}
\title{An annotation data file that maps IPI protein identifiers to TrEMBL
  protein identifiers.} 
\description{
  #PKGNAME#IPI2TREBML maps IPI protein identifiers to TrEMBL protein identifiers.
}
\details{
  This is an environment object containing key and value pairs. Keys is
  IPI protein identifier and value is a vector of TrEMBL protein identifier.

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#IPI2TREBML)
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

