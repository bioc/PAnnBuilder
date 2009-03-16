\name{#PKGNAME#DE}
\alias{#PKGNAME#DE}
\title{An annotation data file that maps protein
 identifiers to textural descriptions}
\description{
  #PKGNAME#DE maps protein identifiers to descriptive information
}
\details{
  This is an environment object containing key and value pairs. Keys are
  protein identifiers and values are the corresponding descriptions.
  protein identifiers that can not be mapped to
  descriptive information are assigned a value NA.
  
  Annotation based on data provided by:
  #SOURCE#

  Package built: #DATE#
}

\examples{
	xx <- as.list(#PKGNAME#DE)
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

