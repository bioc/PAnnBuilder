\name{#PKGNAME#PATH2GI}
\alias{#PKGNAME#PATH2GI}
\title{An annotation data file that maps KEGG pathway identifiers to NCBI 
  Protein GI identifiers.} 
\description{
  #PKGNAME#PATH2GI maps KEGG pathway identifiers to NCBI protein GI identifiers.
}
\details{
  This is an environment object containing key and value pairs. Keys are
  KEGG pathway identifiers and values are protein identifiers. Values are
  vectors of length 1 or greater depending on whether a pathway identifier can
  be maapped to one or more protein identifiers. NAs are assigned to KEGG 
  pathway identifiers that can not be mapped to any protein identifiers. 

  KEGG pathway identifiers are the identifiers used by KEGG for various
  pathways.

  Mappings were based on data provided by:
  #SOURCE#
  #PATH#
  #KEGG#

  Package built: #DATE#
}
\references{
\url{ftp://ftp.genome.ad.jp/pub/kegg/pathways}
}
\examples{
	xx <- as.list(#PKGNAME#PATH2GI)
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
		# Get the values for multiget for a few keys
		if(length(xx) >= 3){
			xx[1:3]
		}
	}
}
\keyword{datasets}

