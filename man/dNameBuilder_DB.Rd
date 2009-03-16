\name{dNameBuilder_DB}
\alias{dNameBuilder_DB}
\title{Build Data Packages for Mapping between ID and Name}
\description{
    Given the interested database, this function creates a data package to 
    get Name from ID or get ID from Name.
}
\usage{
dNameBuilder_DB(prefix, pkgPath, version, author)
}
\arguments{ 
  \item{prefix}{the prefix of the name of the data package to be built. (e.g. 
    "hsaSP"). The name of builded package is prefix+".db". }
  \item{pkgPath}{a character string for the full path of an existing
    directory where the built backage will be stored.}  
  \item{version}{a character string for the version number.}
  \item{author}{a list with named elements "authors" containing a character
    vector of author names and "maintainer" containing the complete
    character string for the maintainer field, for example, "Jane
    Doe <jdoe@doe.com>".}
}
\details{
  For given database, build a R package to map entry ID and Name. Supported 
  databases are:
  "GO": Gene Ontology,  \url{http://www.geneontology.org} ;
  "KEGG": KEGG Pathway Database,  \url{http://www.genome.ad.jp/kegg/pathway.html} ;
  "PFAM": \url{http://www.sanger.ac.uk/Software/Pfam} ;
  "INTERPRO": \url{http://www.ebi.ac.uk/interpro} ;
  "TAX": NCBI Taxonomy, \url{http://www.ncbi.nlm.nih.gov/sites/entrez?db=Taxonomy} ;

  \code{\link{dNameBuilder_DB}} employes functions 
    \code{\link{writeGOName_DB}}, \code{\link{writeKEGGName_DB}}
  \code{\link{writePFAMName_DB}}, \code{\link{writeINTERPROName_DB}} and 
    \code{\link{writeTAXName_DB}} 
  to parse and write data.
    
  Data files in the database will be automatically downloaded to the tmp
  directory, so enough space is needed for the data files. After
  downloading, files are parsed by perl, so perl must be installed.  It may 
  take a long time to parse database and build R package. Alternatively, we have 
  produced diverse R packages by PAnnBuilder, and you can download appropriate 
  package via \url{http://www.biosino.org/PAnnBuilder}.
}
\value{
  This function does not return any value.
}
\author{Hong Li}
\examples{
# Set path, version and author for the package.
pkgPath <- tempdir()
version <- "1.0.0"
author <- list()
author[["authors"]] <- "Hong Li"
author[["maintainer"]] <- "Hong Li <sysptm@gmail.com>"
  
## It may take a long time to parse database and build R package.
# Build ID-Name mapping packages "dName.db" for "GO", "KEGG", "PFAM", "INTERPRO", 
# and "TAX".
if(interactive()){
    dNameBuilder_DB(prefix = "dName", pkgPath, version, author)
}
}

