\name{#PKGNAME#HOMOLOGENE}
\alias{#PKGNAME#HOMOLOGENE}
\title{Parse homologs among the annotated genes from HomoloGene}
\description{
  #PKGNAME#HOMOLOGENE is an R environment that stores homologs among the 
  annotated genes of several completely sequenced eukaryotic genomes.
}
\details{
  Each element in this R environment is a homolog group and expressed
  using a matrix.
  
  Mappings were based on data provided by:  
  #SOURCE#
  
  Package built #DATE#
}
\references{
  \url{http://www.ncbi.nlm.nih.gov/sites/entrez?db=homologene}
}
\examples{
    # Convert the environment to a list
    xx <- as.list(#PKGNAME#HOMOLOGENE)
    if(length(xx) > 0){
        # The symbols for the first two elements of XX
        xx[1:2]
        # Get the first one
        xx[[1]]
    }
}

\keyword{datasets}

