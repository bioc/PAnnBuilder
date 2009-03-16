\name{#PREFIX#BF2IPI}
\alias{#PREFIX#BF2IPI}
\title{Map body fluid to protein IPI identifiers}
\description{
  #PREFIX#BF2IPI is an R environment that provides mappings body fluid to
  protein IPI identifiers in this body fluids. 
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
     xx <- as.list(#PREFIX#BF2IPI)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){         
         # Get the first one
         xx[[1]]
     }
}

