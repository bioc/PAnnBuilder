\name{#PKGNAME#ORIENT}
\alias{#PKGNAME#ORIENT}
\title{Map protein IDs to orientation}
\description{
 #PKGNAME#ORIENT is an R environment that provides mappings between a
 protein identifier and the orientation of corresponding gene. 
}
\details{	
	If the orientation is unknown, the vector will contain an \code{NA}.
  
  Mappings were based on data provided by: 
  #SOURCE#
  #GeneInfo#
  
  Package built: #DATE#
}

\examples{
        # Convert to a list
        xx <- as.list(#PKGNAME#ORIENT)        
        xx <- xx[!is.na(xx)]
        if(length(xx) > 0){
            # Get the orientation for the first five proteins
            xx[1:5]
            # Get the first one
            xx[[1]]
        }
}
\keyword{datasets}

