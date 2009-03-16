\name{#PREFIX#ORGANISM}
\alias{#PREFIX#ORGANISM}
\title{Map SysPTM protein identifiers to organism}
\description{
  #PREFIX#ORGANISM map SysPTM protein identifiers to the name the organism.
}
\details{
  Mappings were based on data provided by: SysPTM (\url{#SYSPTMSOURCEURL#}) on
  #SYSPTMSOURCEDATE#  
}
\examples{
         # Convert to a list
        xxx <- as.list(#PREFIX#ORGANISM)
        # randomly display 10 proteins
        sample(xxx, 10)
}

