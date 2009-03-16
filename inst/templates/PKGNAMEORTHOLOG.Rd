\name{#PKGNAME#ORTHOLOG}
\alias{#PKGNAME#ORTHOLOG}
\title{Get Ortholog Groups }
\description{
  #PKGNAME#ORTHOLOG is an R environment that stores orthologs among the 
  two organisms.
}
\details{
  Each element in this R environment is a ortholog group and expressed
  using a matrix.
  
  Mappings were based on data provided by: 
  #SOURCE#
  
  Package built #DATE#
}
\references{
  \url{http://inparanoid.sbc.su.se/cgi-bin/index.cgi}
}
\examples{
    # Convert the environment to a list
    xx <- as.list(#PKGNAME#ORTHOLOG)
    if(length(xx) > 0){
        # The symbols for the first two elements of XX
        xx[1:2]
        # Get the first one
        xx[[1]]
    }
}

\keyword{datasets}

