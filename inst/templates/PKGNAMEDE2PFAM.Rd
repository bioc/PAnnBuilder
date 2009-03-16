\name{#PKGNAME#DE2PFAM}
\alias{#PKGNAME#DE2PFAM}
\title{Map textural descriptions to Pfam ID}
\description{
  #PKGNAME#DE2PFAM maps domain textural description to their Pfam ID.
}
\details{  
  Annotation based on data provided by:
  #SOURCE#

  Package built: #DATE#
}

\examples{
	xx <- as.list(#PKGNAME#DE2PFAM)
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

