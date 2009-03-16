\name{#PKGNAME#SPAC}
\alias{#PKGNAME#SPAC}
\title{An annotation data file that maps Protein identifiers to
 SwissProt Primary Accession Number} 
\description{
  #PKGNAME#SPAC maps protein identifiers to SwissProt primary accession number
}
\details{
  This is an environment object containing key and value pairs. Keys are
  protein identifiers and values are SwissProt primary accession number (eg: P04637).

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#SPAC)
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

