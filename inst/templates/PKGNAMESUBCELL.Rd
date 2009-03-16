\name{#PKGNAME#SUBCELL}
\alias{#PKGNAME#SUBCELL}
\title{Map Protein IDs to the description of the Subcellular Location of the 
chain/peptide/isoform}
\description{
 #PKGNAME#SUBCELL is an R environment that provides mappings between a
 protein identifier and the description of the Subcellular Location of the 
 chain/peptide/isoform. 
}
\details{
  Each protein identifier maps to the description of the Subcellular Location 
  of the chain/peptide/isoform. 
  This information is extractd from "SUBCELLULAR LOCATION" topic in SwissProt 
  "comments" lines. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#SUBCELL)
        # randomly display 10 proteins
        sample(xxx, 10)
}
\keyword{datasets}

