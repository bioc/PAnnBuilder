\name{writeData_DB}
\alias{createEmptyDPkg}
\alias{writeMeta_DB}
\alias{writeData_DB}
\alias{writeSPData_DB}
\alias{writeIPIData_DB}
\alias{writeREFSEQData_DB}
\alias{writeSYSBODYFLUIDData_DB}
\alias{writeGOAData_DB}
\alias{writeHomoloGeneData_DB}
\alias{writeInParanoidData_DB}
\alias{writeGENEINTData_DB}
\alias{writeINTACTData_DB}
\alias{writeMPPIData_DB}
\alias{write3DIDData_DB}
\alias{writeDOMINEData_DB}
\alias{writePeptideAtlasData_DB}
\alias{writeSYSPTMData_DB}
\alias{writeSCOPData_DB}
\alias{writeBACELLOData_DB}
\alias{writeDBSUBLOCData_DB}
\alias{writeName_DB}
\alias{writeGOName_DB}
\alias{writeKEGGName_DB}
\alias{writePFAMName_DB}
\alias{writeINTERPROName_DB}
\alias{writeTAXName_DB}
\title{Parse and Write Data into R Data Packages}
\description{
    These functions store data tables in the "*.sqlite" file in SQLite-based 
    annotation package. 
}
\usage{
createEmptyDPkg(pkgName, pkgPath,folders = c("man", "R", "data"), force = TRUE)

writeMeta_DB(db, repList)

writeData_DB(type, srcUrls, db, organism="")
writeSPData_DB(srcUrls, db, organism)
writeIPIData_DB(srcUrls, db, organism)
writeREFSEQData_DB(srcUrls, db, organism)
writeSYSBODYFLUIDData_DB(srcUrls, db)
writeGOAData_DB(srcUrls, db)
writeHomoloGeneData_DB(srcUrls, db)
writeInParanoidData_DB(srcUrls, db)
writeGENEINTData_DB(srcUrls, db)
writeINTACTData_DB(srcUrls, db)
writeMPPIData_DB(srcUrls, db)
write3DIDData_DB(srcUrls, db)
writeDOMINEData_DB(srcUrls, db)
writePeptideAtlasData_DB(srcUrls, db)
writeSYSPTMData_DB(srcUrls, db)
writeSCOPData_DB(srcUrls, db)
writeBACELLOData_DB(srcUrls, db)
writeDBSUBLOCData_DB(srcUrls, db)

writeName_DB(type, srcUrls, db)
writeGOName_DB(srcUrls, db) 
writeKEGGName_DB(srcUrls, db)
writePFAMName_DB(srcUrls, db)
writeINTERPROName_DB(srcUrls, db)
writeTAXName_DB(srcUrls, db)
}
\arguments{
  \item{pkgName}{the name of the data package to be built. (e.g. "hsaSP") }
  \item{pkgPath}{a character string for the full path of an existing
    directory where the built backage will be stored.}   
  \item{folders}{a string vector for the file folder of establish R package.}
  \item{force}{a boolean to indicate whether the existed homonymic folder 
    will be removed.}
  \item{db}{a object that extends DBIConnection.}
  \item{repList}{a named list which will replace the symbols in template file.}
  \item{type}{character string, giving the name of concerned database.}
  \item{srcUrls}{a character string, giving the url of the source file.}  
  \item{organism}{a character string for the name of the organism of
    concern. (eg: "Homo sapiens")}
}
\details{
  These functions download and parse data from diverse databases. The result
  files are stored as tables in the "*.sqlite" file.
  
  \code{\link{writeMeta_DB}} writes information from "repList" into a table 
    called "metadata".
  
  \code{\link{writeData_DB}} download and parse source file, and write the result
    files into tables.  
}
\author{Hong Li}

