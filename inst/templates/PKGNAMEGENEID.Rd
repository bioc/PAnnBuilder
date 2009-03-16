\name{#PKGNAME#GENEID}
\alias{#PKGNAME#GENEID}
\title{An annotation data file that maps Protein identifiers to Entrez Gene
 identifiers} 
\description{
  #PKGNAME#GENEID maps protein identifiers to Entrez Gene identifiers
}
\details{
  This is an environment object containing key and value pairs. Keys are
  PubMed identifier and values are Entrez Gene identifier.

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}

\examples{
	xx <- as.list(#PKGNAME#GENEID)
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

