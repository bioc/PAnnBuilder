\name{#PKGNAME#IPI2GI}
\alias{#PKGNAME#IPI2GI}
\title{An annotation data file that maps IPI protein identifiers to GI
  of Refseq proteins.} 
\description{
  #PKGNAME#IPI2GI maps IPI protein identifiers to GI of Refseq proteins.
}
\details{
  This is an environment object containing key and value pairs. Keys is
  IPI protein identifier and value is a vector of protein GIs.

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#IPI2GI)
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

