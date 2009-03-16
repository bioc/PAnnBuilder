\name{#PKGNAME#SPACs}
\alias{#PKGNAME#SPACs}
\title{An annotation data file that maps Protein identifiers to
 all SwissProt Accession Numbers} 
\description{
  #PKGNAME#SPACs maps protein identifiers to SwissProt accession numbers
}
\details{
  This is an environment object containing key and value pairs. Keys are
  protein identifiers and values are SwissProt accession numbers (eg: P04637, Q15086).
  #PKGNAME#SPACs includes all accession numbers in all versions of SwissProt,
  while #PKGNAME#SPAC is the accession number in current version.

  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}
\examples{
	xx <- as.list(#PKGNAME#SPACs)
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

