\name{ptmBuilder_DB}
\alias{ptmBuilder_DB}
\title{Build Data Packages for Protein Post-Translational Modifications}
\description{
    Given the URL to Post-Translational Modifications data, this function 
    creates a SQLite-based annotation data package.
}
\usage{
ptmBuilder_DB(src="SysPTM",
          prefix, pkgPath, version, author)
}
\arguments{ 
  \item{src}{a character string that can be "SysPTM" to indicate
    which database will be used.
    "SysPTM": \url{http://www.biosino.org/SysPTM} }
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
  Build annotation data packages for Post-Translational Modifications, such 
  as phosphorylation, methylation, acetylation, glycosylation and so on.

  \code{\link{ptmBuilder_DB}} employes functions 
    \code{\link{writeSYSPTMData_DB}} to parse and write data.  

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
# Build annotation data packages "sysptm.db" for post-translational modifications.
if(interactive()){
    ptmBuilder_DB(src = "SysPTM",
               prefix= "sysptm", pkgPath, version, author)
}
}


