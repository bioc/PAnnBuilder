\name{getSrcUrl}
\alias{speciesNorganism}
\alias{organism2species}
\alias{organism2alias}
\alias{getShortSciName}
\alias{organism2taxID}
\alias{taxID2organism}
\alias{.srcUrls}
\alias{getSrcUrl}
\alias{getALLUrl}
\alias{getSrcBuilt}
\alias{getALLBuilt}
\alias{getSPUrl}
\alias{getTREMBLUrl}
\alias{getIPIUrl}
\alias{getREFSEQUrl}
\alias{getGOUrl}
\alias{getGOAUrl}
\alias{getGENEINTUrl}
\alias{getINTACTUrl}
\alias{getMPPIUrl}
\alias{get3DIDUrl}
\alias{getDOMINEUrl}
\alias{getDBSUBLOCUrl}
\alias{getBACELLOUrl}
\alias{getINTERPROUrl}
\alias{getPFAMUrl}
\alias{getSCOPUrl}
\alias{getHOMOLOGENEUrl}
\alias{getINPARANOIDUrl}
\alias{getPEPTIDEATLASUrl}
\alias{getSYSPTMUrl}
\alias{getSYSBODYFLUIDUrl}
\alias{getKEGGUrl}
\alias{getSPBuilt}
\alias{getTREMBLBuilt}
\alias{getIPIBuilt}
\alias{getREFSEQBuilt}
\alias{getGOABuilt}
\alias{getGENEINTBuilt}
\alias{getINTACTBuilt}
\alias{getMPPIBuilt}
\alias{get3DIDBuilt}
\alias{getDOMINEBuilt}
\alias{getDBSUBLOCBuilt}
\alias{getBACELLOBuilt}
\alias{getINTERPROBuilt}
\alias{getPFAMBuilt}
\alias{getSCOPBuilt}
\alias{getHOMOLOGENEBuilt}
\alias{getINPARANOIDBuilt}
\alias{getPEPTIDEATLASBuilt}
\alias{getSYSPTMBuilt}
\alias{getSYSBODYFLUIDBuilt}
\alias{getSrcSQUrl}
\alias{getSrcSQBuilt}
\alias{getIPISQUrl}
\alias{getPROSITENAMEUrl}
\alias{getPROSITENAMEBuilt}
\alias{getREFSEQSQUrl}
\title{Get the Url and Release information for Diverse Databases}
\description{
    Given the abbreviation of databases, these functions gets
    the url of data file, and get the version/release of the database.
}
\usage{
speciesNorganism()
organism2species(organism) 
organism2alias(organism)
getShortSciName(organism)
organism2taxID(organism)
taxID2organism(taxID)

.srcUrls(name)

getSrcUrl(src, organism = "")
getALLUrl(organism)
getSrcBuilt(src, organism = "")
getALLBuilt(organism)
getSrcSQUrl(src,organism)
getSrcSQBuilt(src,organism)

getSPUrl()
getTREMBLUrl()
getIPIUrl(organism)
getREFSEQUrl(organism)
getGOUrl()
getGOAUrl(organism)
getGENEINTUrl()
getINTACTUrl()
getMPPIUrl()
get3DIDUrl()
getDOMINEUrl()
getDBSUBLOCUrl()
getBACELLOUrl()
getINTERPROUrl()
getPFAMUrl()
getSCOPUrl()
getHOMOLOGENEUrl()
getINPARANOIDUrl(organism)
getPEPTIDEATLASUrl(organism)
getSYSPTMUrl()
getSYSBODYFLUIDUrl()
getSPSQUrl()
getTREMBLSQUrl()
getIPISQUrl(organism)
getREFSEQSQUrl(organism)

getSPBuilt()
getTREMBLBuilt()
getIPIBuilt(organism)
getREFSEQBuilt(organism)
getGOABuilt(organism)
getGENEINTBuilt()
getINTACTBuilt()
getMPPIBuilt()
get3DIDBuilt()
getDOMINEBuilt()
getDBSUBLOCBuilt()
getBACELLOBuilt()
getINTERPROBuilt()
getPFAMBuilt()
getSCOPBuilt()
getHOMOLOGENEBuilt()
getINPARANOIDBuilt()
getPEPTIDEATLASBuilt(organism)
getSYSPTMBuilt()
getSYSBODYFLUIDBuilt()
}
\arguments{  
  \item{src}{a character string to indicate which database will be used.
    Possible values are: sp, trembl, ipi, refseq, go, goa, geneint, intact, 
    mppi, 3did, domain, dbsubloc, bacello, interpro, pfam, scop, 
    homologene, inparanoid, peptideatlas, sysptm, sysbodyfulid, prosite or ALL.
  }
  \item{organism}{a character string for the name of the organism of
    concern. (eg: "Homo sapiens")}
  \item{taxID}{a integer for the taxonomy identifier. (eg: 1906)}
  \item{name}{a character string to indicate data source.
    Possible values are: SP, IPI, REFSEQ, KEGG, Gene, GO, GOA, geneint, intact, 
    MPPI, 3DID, DOMINE, DBsubloc, Bacello, Interpro, Pfam, SCOP,
    Homologene, Inparanoid, PeptideAtlas, SysPTM, Sys-BodyFulid or TAX.
  }
}
\details{
    These functions are the results of an effort to get url and release/version 
    for diverse databases. Some functions are improved from previous package 
    "AnnBuilder".
    
    Functions developped to convert multiple names for organism:
    \code{\link{speciesNorganism}} return a three-column matrix. 
     organism2species() and organism2alias() employ speciesNorganism()
     to get organism and alias used in IPI database.
    \code{\link{organism2species}} use speciesNorganism() to convert organism to
     their species name.
    \code{\link{organism2alias}} use speciesNorganism() to convert organism to 
     their alias name.
    \code{\link{getShortSciName}} get 3-character short name, eg: "Hsa".
    \code{\link{organism2taxID}} convert organism to taxID, eg: 9606 for 
     "Homo sapiens".
    \code{\link{taxID2organism}} convert taxID to organism, eg: "Homo sapiens" 
     for 9606.
    
    Functions developped to get url and release information for data file:
    \code{\link{.srcUrls}} get basic web address for the given database.
    \code{\link{getSrcUrl}} get url for the data file based on .srcUrls().
    \code{\link{getALLUrl}} get all available urls in this package.
    \code{\link{getSrcBuilt}} get version/release information based on 
     \code{\link{.srcUrls}}.
    \code{\link{getALLBuilt}} get release information for databases in 
     \code{\link{getALLUrl}}.
    \code{\link{getSrcSQUrl}} get url for protein sequence files.
    \code{\link{getSrcSQBuilt}} get release information for 
     protein sequence files.
}
\value{
    \code{\link{getSrcUrl}} return a character string to indicate the url of 
    data file.
  
    \code{\link{getSrcBuilt}} return a character string to indicate the release/
     version of the database.
}
\author{Hong Li}
\references{ 
Zhang, J., Carey, V., Gentleman, R. (2003) An extensible application for 
assembling annotation for genomic data.Bioinformatics 19(1), 155-156. 
}
\examples{
if(interactive()){
    # Get urls of all related databases  for "Homo sapiens".
    getALLUrl("Homo sapiens")
    
    # Get version/release information of all related databases for "Homo sapiens".
    getALLBuilt ("Homo sapiens")
}
}

