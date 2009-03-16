\name{#PKGNAME#UPID}
\alias{#PKGNAME#UPID}
\title{An annotation data file that maps Protein identifiers to
  UniProt identifiers} 
\description{
  #PKGNAME#UPID maps protein identifiers to UniProt identifiers
}
\details{
  This is an environment object containing key and value pairs. Keys are
  protein identifiers and values are UniProt identifiers (eg: P53\_HUMAN).

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#UPID)
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

