\name{scopBuilder_DB}
\alias{scopBuilder_DB}
\title{Build Data Package for Structural Classification of Proteins}
\description{
    Given the URL to SCOP database, this function creates a SQLite-based 
    annotation data package.
}
\usage{
scopBuilder_DB(prefix, pkgPath, version, author)
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
  Build annotation data packages for structural classification of proteins. 
  Data is from SCOP database: \url{http://scop.mrc-lmb.cam.ac.uk/scop} ;

  \code{\link{writeSCOPData_DB}} to parse and write data from SCOP database.

  Data files in the database will be automatically downloaded to the tmp
  directory, so enough space is needed for the data files. After
  downloading, files are parsed by perl, so perl must be installed.  It may 
  take a long time to parse database and build R package. Alternatively, we have 
  produced diverse R packages by PAnnBuilder, and you can download appropriate 
  package via  \url{http://www.biosino.org/PAnnBuilder}.
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
    # Build annotation data packages "scop.db" for SCOP database.
    scopBuilder_DB(prefix = "scop", pkgPath, version, author)
}
}


