\name{#PKGNAME#BF}
\alias{#PKGNAME#BF}
\title{Map Protein identifiers to Body Fluids}
\description{
  #PKGNAME#BF is an R environment that provides mappings between
  protein identifiers and involved body fluids. 
}
\details{
  The name of body fluids in use include:
  Amniotic fluid
  Bronchoalveolar lavage fluid
  Cerebrospinal fluid
  Milk
  Nipple aspiration fluid
  Plasma/Serum
  Saliva
  Seminal Fluid
  Synovial Fluid
  Tear
  Urine
  
  Mappings were based on data provided by:  
  #SOURCE#
  
  Package built #DATE#
}

\examples{
     # Convert to a list
     xx <- as.list(#PKGNAME#BF)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){         
         # Get the first one
         xx[[1]]
     }
}
\keyword{datasets}

