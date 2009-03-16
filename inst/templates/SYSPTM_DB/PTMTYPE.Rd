\name{#PREFIX#PTMTYPE}
\alias{#PREFIX#PTMTYPE}
\alias{#PREFIX#PTMTYPE2SITE}
\title{Map site to the type of posttranslational modifications}
\description{
 #PREFIX#PTMTYPE maps a site to the type of posttranslational modifications (PTMs).
 The site is consisted of SysPTM protein identifer, start position of PTM, 
 and end position of PTM.
}
\details{
  Mappings were based on data provided by: SysPTM (\url{#SYSPTMSOURCEURL#}) on
  #SYSPTMSOURCEDATE#  
}

\examples{
 # Convert to a list
 xx <- as.list(#PREFIX#PTMTYPE)
 # Get the values for a few keys
 if(length(xx) >= 3){
   xx[1:3]
 }
 
 ## For the reverse map #PREFIX#PTMTYPE2SITE
 xx <- as.list(#PREFIX#PTMTYPE2SITE)
 if(length(xx) > 0){         
     ## Get the first one
     xx[[1]]
 }   
}
