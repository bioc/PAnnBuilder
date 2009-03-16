\name{bfBuilder_DB}
\alias{bfBuilder_DB}
\title{Build Data Packages for Body Fluid Proteomes}
\description{
    Given the URL to body fluid protein data, this function creates a 
    SQLite-based annotation data package.
}
\usage{
bfBuilder_DB(src="SysBodyFluid",
          prefix, pkgPath, version, author)
}
\arguments{ 
  \item{src}{a character string that can be "SysBodyFluid" to indicate
    which body fluid database will be used.
    "SysBodyFluid": \url{http://www.biosino.org/bodyfluid} }
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
  Build annotation data packages for proteins in body fluids, such as
  plasma/serum, urine, cerebrospinal fluid, saliva, bronchoalveolar lavage 
  fluid, synovial fluid, nipple aspirate fluid, tear fluid, seminal fluid, 
  human milk, amniotic fluid, and so on.

  \code{\link{bfBuilder_DB}} employes functions 
  \code{\link{writeSYSBODYFLUIDData_DB}} to parse and write data.
    
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
# Build annotation data packages "org.Hs.bf.db" for body fluid proteomics.
if(interactive()){
    bfBuilder_DB(src = "SysBodyFluid",
              prefix= "org.Hs.bf", pkgPath, version, author)
}
}


