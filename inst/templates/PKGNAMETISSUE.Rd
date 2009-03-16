\name{#PKGNAME#TISSUE}
\alias{#PKGNAME#TISSUE}
\title{Map Protein IDs to the description of the tissue-specific expression of mRNA or protein}
\description{
  #PKGNAME#TISSUE is an R environment that provides mappings between a
 protein identifier and the description of the tissue-specific expression of mRNA or protein. 
}
\details{
  Each protein identifier maps to the description of the tissue-specific expression of mRNA or protein. 
  This information is extractd from "TISSUE SPECIFICITY" topic in SwissProt "comments" lines. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#TISSUE)
        # randomly display 10 proteins
        sample(xxx, 10)
}
\keyword{datasets}

