\name{#PKGNAME#END}
\alias{#PKGNAME#END}
\title{An annotation data file for transciption ending locations of genes
on chromosome}
\description{
  #PKGNAME#END maps protein identifiers to the transciption ending
  locations of corresponding genes on chromosome
}
\details{
   This is an environment object containing key and value pairs. Keys
   are protein identifiers and values are the transciption ending location for
   genes. The ending positions for genes on both the sense and antisense
   strand are number of base pairs measured from the p (5' end of the
   sense strand) to q (3' end of the sense strand) arms. 

   Mappings were derived from the following public data sources:
   #SOURCE#
   #Gene2refseq#

   Package built: #DATE#
}
\references{
\url{http://www.genome.ucsc.edu/goldenPath/hg16/database/}
}
\examples{
	xx <- as.list(#PKGNAME#END)
	if(length(xx) > 0){
		# Get the value of the first key
		xx[1]
		# Get the values for a few keys
		if(length(xx) >= 3){
		    xx[1:3]
		}
	}
}
\keyword{datasets}

