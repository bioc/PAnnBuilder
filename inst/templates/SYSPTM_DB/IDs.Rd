\name{#PREFIX#IDs}
\alias{#PREFIX#IDs}
\title{Map SysPTM protein identifiers to external identifiers}
\description{
  #PREFIX#IDs maps SysPTM protein identifiers to external protein identifiers.
}
\details{
  Each SysPTM protein identifier maps to a named vector containing protein
  identifiers from public databases, such as Swiss-Prot, IPI, NCBI RefSeq, and
  so on. The vector name corresponds to the public database.

  Mappings were based on data provided by: SysPTM (\url{#SYSPTMSOURCEURL#}) on
  #SYSPTMSOURCEDATE#  
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#IDs)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){
         xx[1:5]
     }
}
