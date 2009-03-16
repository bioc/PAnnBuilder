\name{#PKGNAME#PTM2SITE}
\alias{#PKGNAME#PTM2SITE}
\title{Get all sites of each Post-translational Modification}
\description{
  #PKGNAME#PTM2SITE is an R environment that provides mappings between a
  Post-translational Modification and the information of modified proteins/sites. 
}
\details{
  Information about Post-translational Modifications contains : protein, 
  position, amino acid, and data source.
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
        # Convert to a list
        xx <- as.list(#PKGNAME#PTM2SITE)
        if(length(xx) > 0){
         # randomly display 1 proteins
         sample(xx, 1)
        }
}
