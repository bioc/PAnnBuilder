\name{#PKGNAME#SPID}
\alias{#PKGNAME#SPID}
\title{An annotation data file that maps Protein identifiers to
 SwissProt identifiers} 
\description{
  #PKGNAME#SPID maps protein identifiers to SwissProt identifiers
}
\details{
  This is an environment object containing key and value pairs. Keys are
  protein identifiers and values are SwissProt identifiers (eg: P53\_HUMAN).

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#SPID)
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

