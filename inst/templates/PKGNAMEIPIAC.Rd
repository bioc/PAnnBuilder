\name{#PKGNAME#IPIAC}
\alias{#PKGNAME#IPIAC}
\title{An annotation data file that maps Protein identifiers to
  IPI Primary Accession Number} 
\description{
  #PKGNAME#IPIAC maps protein identifiers to IPI primary accession number
}
\details{
  This is an environment object containing key and value pairs. Keys are
  protein identifiers and values are IPI primary accession number (eg: IPI00000045).

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#IPIAC)
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

