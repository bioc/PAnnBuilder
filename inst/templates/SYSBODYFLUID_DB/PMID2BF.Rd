\name{#PREFIX#PMID2BF}
\alias{#PREFIX#PMID2BF}
\alias{#PREFIX#BF2PMID}
\title{Map published paper to body fluid}
\description{
  #PREFIX#PMID2BF is an R environment that provides mappings between
  public peer-reviewed papers collected in Sys-BodyFluid and involved body fluids. 
}
\details{
  The name of body fluids in use include:
  Amniotic fluid; Bronchoalveolar lavage fluid; Cerebrospinal fluid; Milk;
  Nipple aspiration fluid; Plasma/Serum; Saliva; Seminal Fluid; Synovial Fluid;
  Tear; Urine.
  
  Mappings were based on data provided by: Sys-BodyFluid (\url{#PAPERSOURCEURL#})
  on #PAPERSOURCEDATE#  
}

\examples{
     ## Convert to a list
     xx <- as.list(#PREFIX#PMID2BF)
     if(length(xx) > 0){         
         ## Get the first one
         xx[[1]]
     }
     
    ## For the reverse map #PREFIX#BF2PMID
     xx <- as.list(#PREFIX#BF2PMID)
     if(length(xx) > 0){         
         ## Get the first one
         xx[[1]]
     }    
}

