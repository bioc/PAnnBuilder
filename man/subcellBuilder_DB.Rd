\name{subcellBuilder_DB}
\alias{subcellBuilder_DB}
\title{Build Data Package for Protein Subcellular Location}
\description{
    Given the URL to subcellular location database, this function creates a 
    SQLite-based annotation data package.    
}
\usage{
subcellBuilder_DB(src=c("BaCelLo", "DBSubLoc"),
              prefix, pkgPath, version, author)
}
\arguments{ 
  \item{src}{a character string that can be "BaCelLo" or "DBSubLoc" to 
    indicate which protein subcellular location database will be used. }
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
  Build annotation data packages for protein subcellular location. Supported 
  databases are:
  "BaCelLo": \url{http://gpcr.biocomp.unibo.it/bacello} ;
  "DBSubLoc": \url{http://www.bioinfo.tsinghua.edu.cn/dbsubloc.html} ;

  \code{\link{subcellBuilder_DB}} employes functions 
   \code{\link{writeBACELLOData_DB}} and \code{\link{writeDBSUBLOCData_DB}}, to  
   parse and write data.

  Data files in the database will be automatically downloaded to the tmp
  directory, so enough space is needed for the data files. After
  downloading, files are parsed by perl, so perl must be installed.  It may 
  take a long time to parse database and build R package. Alternatively, we have 
  produced diverse R packages by PAnnBuilder, and you can download appropriate 
  package via  \url{http://www.biosino.org/PAnnBuilder} .
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
author[["maintainer"]] <- "Hong Li <sysptm@com.cn>"

## It may take a long time to parse database and build R package.
if(interactive()){
    # Build annotation data packages "sc.bacello.db" for BaCelLo database.
    subcellBuilder_DB(src="BaCelLo",
                   prefix = "sc.bacello", pkgPath, version, author)
    
    # Build annotation data packages "sc.dbsubloc.db" for DBSubLoc database.
    subcellBuilder_DB(src="DBSubLoc",
                   prefix = "sc.dbsubloc", pkgPath, version, author)
}
}


