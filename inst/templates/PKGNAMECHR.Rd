\name{#PKGNAME#CHR}
\alias{#PKGNAME#CHR}
\title{Map protein IDs to Chromosomes}
\description{
 #PKGNAME#CHR is an R environment that provides mappings between a
 protein identifier and the chromosome that contains the corresponding gene. 
}
\details{
	If the chromosomal location is unknown, the vector will contain an \code{NA}.
  
  Mappings were based on data provided by:
  #SOURCE#
  #GeneInfo#
  
  Package built #DATE#
}

\examples{
        # Convert to a list
        xx <- as.list(#PKGNAME#CHR)
        # Remove protein identifiers that do not map to any CHRLOC
        xx <- xx[!is.na(xx)]
        if(length(xx) > 0){
            # Get the chromosome number for the first five proteins
            xx[1:5]
            # Get the first one
            xx[[1]]
        }
}
\keyword{datasets}

