\name{#PKGNAME#GI2TREMBL}
\alias{#PKGNAME#GI2TREMBL}
\title{An annotation data file that maps GI of Refseq proteins to
  TrEMBL protein identifiers.} 
\description{
  #PKGNAME#GI2TREMBL maps GI of Refseq proteins to TrEMBL protein identifiers.
}
\details{
  This is an environment object containing key and value pairs. Keys is
  GI protein identifier and value is a vector of TrEMBL protein identifier.

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#GI2TREMBL)
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

