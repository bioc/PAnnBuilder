\name{intBuilder_DB}
\alias{intBuilder_DB}
\title{Build Data Package for Interaction}
\description{
    Given the URL to protein-protein or domain-domain interaction database, 
    this function creates a SQLite-based annotation data package.
}
\usage{
intBuilder_DB(src=c("geneint","intact","mppi","3DID","DOMINE"), 
           prefix, pkgPath, version, author) 
}
\arguments{ 
  \item{src}{a character string to indicate which protein-protein or 
    domain-domain interaction database will be used. }
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
  Build annotation data packages for protein-protein or domain-domain interaction. 
  Supported databases are:
  "geneint": \url{ftp://ftp.ncbi.nih.gov/gene/GeneRIF/interactions.gz} ;
  "intact": \url{http://www.ebi.ac.uk/intact} ;
  "mppi": \url{http://mips.gsf.de/proj/ppi} ;
  "3DID": interacting protein domains of known three-dimensional structure, 
          \url{http://3did.embl.de} ;
  "DOMINE": \url{http://domine.utdallas.edu/cgi-bin/Domine} ;

  \code{\link{intBuilder_DB}} employes functions 
    \code{\link{writeGENEINTData_DB}}, \code{\link{writeINTACTData_DB}}, 
    \code{\link{writeMPPIData_DB}}, \code{\link{write3DIDData_DB}} and 
    \code{\link{writeDOMINEData_DB}} to parse and write data.

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
if(interactive()){
    # Build annotation data package "int.geneint" for interaction data from NCBI.
    intBuilder_DB(src="geneint", 
    prefix="int.geneint", pkgPath, version, author) 
    
    # Build annotation data package "int.intact" for IntAct database.
    intBuilder_DB(src="intact", 
    prefix="int.intact", pkgPath, version, author) 
    
    # Build annotation data package "int.mppi" for interaction data from MIPS.
    intBuilder_DB(src="mppi", 
    prefix="int.mppi", pkgPath, version, author) 
    
    # Build annotation data package "int.did" for 3DID database.
    intBuilder_DB(src="3DID", 
    prefix="int.did", pkgPath, version, author) 
    
    # Build annotation data package "int.domine" for DOMINE database.
    intBuilder_DB(src="DOMINE", 
    prefix="int.domine", pkgPath, version, author) 
}
}


