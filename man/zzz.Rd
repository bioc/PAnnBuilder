\name{.onLoad}
\alias{.onLoad}
\alias{sourceURLs}
\alias{descriptionInfo}
\alias{taxLists}
\title{Load packages and data}
\description{
    This functions load depended R packages and imports default data into 
    global "options".
}
\usage{
.onLoad(libname, pkgname)
descriptionInfo
}
\arguments{ 
  \item{libname}{a character string giving the library directory where the 
    package defining the namespace was found.}
  \item{pkgname}{a character string giving the name of the package, including 
    the version number if the package was installed with 
    --with-package-versions.}
}
\details{
    This function is from Bioconductor "AnnBuilder" package, and do some 
    modification by Hong Li, 2008.
}
\value{
  This function does not return any value.
}
\author{Hong Li}
