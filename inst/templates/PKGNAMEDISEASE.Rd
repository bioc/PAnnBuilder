\name{#PKGNAME#DISEASE}
\alias{#PKGNAME#DISEASE}
\title{Map Protein IDs to the description of the disease(s) associated with a deficiency of a protein}
\description{
  #PKGNAME#DISEASE is an R environment that provides mappings between a
 protein identifier and the description of the disease(s) associated with 
 a deficiency of a protein. 
}
\details{
  Each protein identifier maps to the description of the disease(s) 
  associated with a deficiency of a protein. This information is extractd 
  from "DISEASE" topic in SwissProt "comments" lines. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#DISEASE)
        # randomly display 10 proteins
        sample(xxx, 10)
}
\keyword{datasets}

