\name{#PKGNAME#TREBML2GI}
\alias{#PKGNAME#TREBML2GI}
\title{An annotation data file that maps TrEMBL protein identifiers to
   GI of Refseq proteins.} 
\description{
  #PKGNAME#TREBML2GI maps TrEMBL protein identifiers to GI of Refseq proteins.
}
\details{
  This is an environment object containing key and value pairs. Keys is
  TrEMBL protein identifier and value is a vector of GI protein identifiers. 

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#TREBML2GI)
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

