\name{#PKGNAME#SPID2PFAM}
\alias{#PKGNAME#SPID2PFAM}
\title{Map SwissProt Protein Acession Number to Pfam}
\description{
  #PKGNAME#SPID2PFAM is an R environment that mapps SwissProt protein acession
  number to the associated Pfam domains.
}
\details{
  Each protein identifier is mapped to a matrix. Each row is a associated 
  pfam domain, and the name of columns are: Pfam identifier, Pfam domain name, 
  Pfam domain description, start position, end position.  
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#  
}
\references{
  \url{http://pfam.janelia.org/}
}
\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#SPID2PFAM)
    # Remove all the NAs
    xx <- xx[!is.na(xx)]
    if(length(xx) > 0){
    	xx[1:3]
    }
}
\keyword{datasets}

