\name{#PKGNAME#UNIGENE}
\alias{#PKGNAME#UNIGENE}
\title{Map Protein Identifiers to UniGene cluster identifiers}
\description{
   #PKGNAME#UNIGENE is an R environment that provides mappings between
   protein identifiers and UniGene identifiers.
}
\details{
  Each protein identifier is mapped to a UniGene identifier. An \code{NA} is
  reported if the manufacturer identifier cannot be mapped to UniGene at this time. 

  A UniGene identifier represents a cluster of sequences of a gene. Using
  UniGene identifiers one can query the UniGene database for information about
  the sequences or the Entrez Gene database for information about the
  genes.  
  
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert the environment to a list
    xx <- as.list(#PKGNAME#UNIGENE)
    # Remove protein identifiers that do no map to any UniGene id
    xx <- xx[!is.null(xx)]
    if(length(xx) > 0){
        # The UniGene identifiers for the first two elements of XX
        xx[1:2]
        # Get the first one
        xx[[1]]
    }
}

\keyword{datasets}

