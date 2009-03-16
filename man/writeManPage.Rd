\name{writeManPage}
\alias{getRepList}
\alias{copyTemplates}
\alias{writeDescription}
\alias{writeDescription_DB}
\alias{writeManAnno}
\alias{writeManAnno_DB}
\alias{copyTemplates_DB}
\alias{writeManSQ}
\alias{writeManMerge}
\title{Write Help Documents}
\description{
    Given the R environment in the data directory, these functions write
    help documents based on templates and stored them in the man directory. 
}
\usage{ 
getRepList(organism="", type, srcUrls="", built="", pkgName)
copyTemplates(repList, pattern, pkgName, pkgPath, replaceBy = NULL)
copyTemplates_DB(repList, pkgName, pkgPath)
writeDescription(pkgName, pkgPath, version, author, 
                 dataSrc = "public data repositories", 
                 license = "The Artistic License, Version 2.0")  
writeDescription_DB(pkgName, pkgPath, version, author, organism,
                    dataSrc = "public data repositories",
                    license = "The Artistic License, Version 2.0") 
    
writeManAnno(pkgName, pkgPath, version, author, repList, pattern) 
writeManAnno_DB(pkgName, pkgPath, version, author, repList)
writeManSQ(pkgName, pkgPath, version, author, repList)
writeManMerge(pkgName, pkgPath, version, author, repList, pattern) 
}
\arguments{ 
  \item{organism}{a character string for the name of the organism of
    concern. (eg: "Homo sapiens")}
  \item{type}{a character string for the concerned database. Possible values of 
    type are : "sp", "trembl", "ipi", "refseq", "geneint", "intact", "mppi", 
    "3DID", "DOMINE", "BaCelLo", "DBSubLoc", "SCOP", "HomoloGene", "InParanoid", 
    "PeptideAtlas", "SysPTM", "SysBodyFluid", "GOA", "GO", "KEGGNAME", "PFAMNAME", 
    "INTERPRONAME", "TAXNAME", "dName", "cross", "pSeq". }
  \item{srcUrls}{a character string or a string vector for the urls of database.}
  \item{built}{a character string for the version/release information of database.}
  \item{repList}{a named list which will replace the symbols in template file.}
  \item{pattern}{a character string of the prefix of template *.Rd file }
  \item{replaceBy}{a character string for the prefix of new *.Rd file. 
    If replaceBy=NULL, pattern will be replaed by pkgName. Otherwise 
    pattern will be replaed by replaceBy }
  \item{dataSrc}{a character string describing the data source}
  \item{license}{a character string describing the license of R package. 
    It will appear in the description file.}
  \item{pkgName}{the name of the data package to be built. (e.g. "hsaSP") }
  \item{pkgPath}{a character string for the full path of an existing
    directory where the built backage will be stored.}  
  \item{version}{a character string for the version number.}
  \item{author}{a list with named elements "authors" containing a character
    vector of author names and "maintainer" containing the complete
    character string for the maintainer field, for example, "Jane
    Doe <jdoe@doe.com>".}  
}
\details{
  Write help document files for the package.
  \code{\link{getRepList}} return a list which will replace the symbols in 
    template file.

  If the name of "*.rda" file in the "data" subdirectory matchs the pattern, 
  \code{\link{copyTemplates}} will produce a "*.rd" file in the "man" 
    subdirectory. Related template files in the "inst/templates" subdirectory 
    are needed for this function. 
  
  Parameter "names" gives the name of the data objects; \code{\link{copyTemplates_DB}} 
  will produces relative "*.rd" files in the "man" subdirectory. Related template 
  files in the "inst/templates" subdirectory are needed for this function. 

  \code{\link{writeDescription}} write descrption file for the environment-based 
    annotation package.
    
  \code{\link{writeDescription_DB}} write descrption file for the SQLite-based  
    annotation package.    

  \code{\link{writeManAnno}} write help document files for the environment-based 
    annotation package.
  \code{\link{writeManAnno_DB}} write help document files for the SQLite-based 
    annotation package.

  \code{\link{writeManSQ}} write help document files for the sequence data 
    package.

  \code{\link{writeManMerge}}  write help document files for the merged package.
}

\author{Hong Li}
\references{ 
Zhang, J., Carey, V., Gentleman, R. (2003) An extensible application for 
assembling annotation for genomic data.Bioinformatics 19(1), 155-156. 
}

