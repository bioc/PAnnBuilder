\name{#PKGNAME#TREBML2IPI}
\alias{#PKGNAME#TREBML2IPI}
\title{An annotation data file that maps TrEMBL protein identifiers to
   IPI protein identifiers.} 
\description{
  #PKGNAME#TREBML2IPI maps TrEMBL protein identifiers to IPI protein 
  identifiers.
}
\details{
  This is an environment object containing key and value pairs. Keys is
  TrEMBL protein identifier and value is a vector of IPI protein identifier.

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#TREBML2IPI)
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

