\name{#PKGNAME#IPIACs}
\alias{#PKGNAME#IPIACs}
\title{An annotation data file that maps Protein identifiers to
  all IPI Accession Numbers} 
\description{
  #PKGNAME#IPIACs maps protein identifiers to IPI accession numbers
}
\details{
  This is an environment object containing key and value pairs. Keys are
  protein identifiers and values are IPI accession numbers (eg: IPI00000045, IPI00004985).

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#IPIACs)
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

