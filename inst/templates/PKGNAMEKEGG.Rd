\name{#PKGNAME#KEGG}
\alias{#PKGNAME#KEGG}
\title{An annotation data file that maps Protein identifiers to
 KEGG Gene identifiers} 
\description{
 #PKGNAME#KEGG maps protein identifiers to KEGG Gene identifiers
}
\details{
  This is an environment object containing key and value pairs. Keys are
  protein identifiers and values are KEGG Gene identifiers.

  Mappings were derived from data provided by:
  #SOURCE#
  #KEGG#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#KEGG)
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

