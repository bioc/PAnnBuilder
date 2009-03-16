\name{#PKGNAME#PTM}
\alias{#PKGNAME#PTM}
\title{Map Protein to the information of Posttranslational Modifications
 in Swiss-Prot database}
\description{
  #PKGNAME#PTM is an R environment that provides mappings between a
 protein and the information of Posttranslational Modifications in Swiss-Prot 
 database. 
}
\details{
  Each protein identifier maps to the information of Posttranslational 
  Modifications. This information is extractd from "MOD\_RES", "LIPID", "CARBOHYD",
   "DISULFID" and "CROSSLNK"  topic in SwissProt feature table (FT lines). 
   3 types of non-experimental qualifiers in comment (CC) lines and feature table 
   (FT lines) indicate that the information given is not based on experimentally 
   proven findings:
	Potential - It indicates that there is some logical or conclusive evidence 
	that the given annotation could apply. This non-experimental qualifier is often 
	used to present the results from protein sequence analysis tools, which are only 
	annotated, if the result makes sense in the context of a given protein.
	Probable - It is stronger than the qualifier 'Potential' and there must be at 
	least some experimental evidence, which indicates, that the given information 
	is expected to be found in the natural environment of a protein.
	By similarity - 'By similarity' is added to facts that were proven for a protein 
	or part of it, and which is then transferred to other protein family members 
	within a certain taxonomic range, dependent on the biological event or characteristic.
	Experimental - PTMs besides above three types.

  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#PTM)
        # randomly display 10 proteins
        sample(xxx, 10)
}
\keyword{datasets}

