\name{#PREFIX#GN}
\alias{#PREFIX#GN}
\title{Map SysPTM protein identifier to Gene Name}
\description{
  #PREFIX#GN maps SysPTM protein
  identifier to the corresponding gene names.
}
\details{
  Each protein identifier maps to a vector containing the gene
  name or alias.

  Mappings were based on data provided by: SysPTM (\url{#SYSPTMSOURCEURL#}) on
  #SYSPTMSOURCEDATE#  
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#GN)
     # Remove protein that do not map to any GENENAME
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){
         # Gets the gene names for the first five probe identifiers
         xx[1:5]
         # Get the first one
         xx[[1]]
     }
}

