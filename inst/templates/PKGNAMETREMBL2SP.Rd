\name{#PKGNAME#TREBML2SP}
\alias{#PKGNAME#TREBML2SP}
\title{An annotation data file that maps TrEMBL protein identifiers to
   Swiss-Prot protein identifiers.} 
\description{
  #PKGNAME#TREBML2SP maps TrEMBL protein identifiers to Swiss-Prot protein 
  identifiers.
}
\details{
  This is an environment object containing key and value pairs. Keys is
  TrEMBL protein identifier and value is a vector of Swiss-Prot protein 
  identifier.

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#TREBML2SP)
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

