\name{#PKGNAME#SP2GI}
\alias{#PKGNAME#SP2GI}
\title{An annotation data file that maps Swiss-Prot protein identifiers to GI
  of Refseq proteins.} 
\description{
  #PKGNAME#SP2GI maps Swiss-Prot protein identifiers to GI
  of Refseq proteins.
}
\details{
  This is an environment object containing key and value pairs. Keys is
  Swiss-Prot protein identifier and value is a vector of GI protein identifier.

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#SP2GI)
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

