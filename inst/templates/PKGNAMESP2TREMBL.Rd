\name{#PKGNAME#SP2TREMBL}
\alias{#PKGNAME#SP2TREMBL}
\title{An annotation data file that maps Swiss-Prot protein identifiers to
  TrEMBL protein identifiers.} 
\description{
  #PKGNAME#SP2TREMBL maps Swiss-Prot protein identifiers to TrEMBL protein 
  identifiers.
}
\details{
  This is an environment object containing key and value pairs. Keys is
  Swiss-Prot protein identifier and value is a vector of TrEMBL protein 
  identifier.

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#SP2TREMBL)
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

