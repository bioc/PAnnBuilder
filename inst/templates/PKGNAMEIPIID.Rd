\name{#PKGNAME#IPIID}
\alias{#PKGNAME#IPIID}
\title{An annotation data file that maps Protein identifiers to
  IPI identifiers} 
\description{
  #PKGNAME#IPIID maps protein identifiers to IPI identifiers
}
\details{
  This is an environment object containing key and value pairs. Keys are
  protein identifiers and values are IPI identifiers (eg: IPI00000045.1).

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#IPIID)
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

