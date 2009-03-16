\name{#PREFIX#BF}
\alias{#PREFIX#BF}
\title{Map Protein identifiers to body fluids}
\description{
  #PREFIX#BF is an R environment that provides mappings between
  protein identifiers and involved body fluids. 
}
\details{
  The name of body fluids in use include:
  Amniotic fluid; Bronchoalveolar lavage fluid; Cerebrospinal fluid; Milk;
  Nipple aspiration fluid; Plasma/Serum; Saliva; Seminal Fluid; Synovial Fluid;
  Tear; Urine.
  
  Mappings were based on data provided by: Sys-BodyFluid (\url{#BFSOURCEURL#})
  on #BFSOURCEDATE#  
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#BF)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){         
         # Get the first one
         xx[[1]]
     }
}

