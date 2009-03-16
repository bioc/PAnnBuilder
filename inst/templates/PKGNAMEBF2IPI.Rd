\name{#PKGNAME#BF2IPI}
\alias{#PKGNAME#BF2IPI}
\title{Map Body Fluids to Protein IPI identifiers}
\description{
  #PKGNAME#BF2IPI is an R environment that provides mappings body fluid to
  protein IPI identifiers in this body fluids. 
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
     xx <- as.list(#PKGNAME#BF2IPI)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){         
         # Get the first one
         xx[[1]]
     }
}
\keyword{datasets}

