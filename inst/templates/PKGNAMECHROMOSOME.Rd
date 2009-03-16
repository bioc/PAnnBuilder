\name{#PKGNAME#CHROMOSOME}
\alias{#PKGNAME#CHROMOSOME}
\title{Map protein identifiers to Chromosome and Position}
\description{
 #PKGNAME#CHROMOSOME is an R environment that provides mappings between a
 protein identifier and the chromosome information that contains the corresponding gene. 
}
\details{
  Each protein identifier is mapped to the chromosome information, including
  chromosome, start, end, strand. If the chromosomal location is unknown,
  the vector will contain an \code{NA}.
  
  Mappings were based on data provided by:  
  #SOURCE#
  
  Package built: #DATE#
}

\examples{
        # Convert to a list
        xx <- as.list(#PKGNAME#CHROMOSOME)
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

