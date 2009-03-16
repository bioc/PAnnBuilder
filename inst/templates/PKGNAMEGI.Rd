\name{#PKGNAME#GI}
\alias{#PKGNAME#GI}
\title{An annotation data file that maps Protein identifiers to
  NCBI GI identifiers} 
\description{
  #PKGNAME#GI maps protein identifiers to NCBI GI identifiers
}
\details{
  This is an environment object containing key and value pairs. Keys are
  protein identifiers and values are NCBI GI identifiers.

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#GI)
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

