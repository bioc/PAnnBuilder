## Some functions are from Bioconductor "AnnBuilder" package, and
## Hong Li add many other functions for building protein annotation package in 
## 2008.

## Get the basic URL
.srcUrls <- function (name) {
    sourceURLs <- getOption("PAnnBuilderSourceUrls")
    sourceURLs[[name]]
}

## Transformation about organism
speciesNorganism <- function() {
    speciesNorganismTable <- rbind(
        c("human", "Homo sapiens", "human"),
        c("mouse", "Mus musculus", "mouse"),
        c("rat", "Rattus norvegicus", "rat"),
        c("danre", "Danio rerio", "zebrafish"),
        c("arath", "Arabidopsis thaliana", "arabidopsis"),
        c("chick", "Gallus gallus", "chicken"),
        c("bovin", "Bos taurus", "cow")
    )
    colnames(speciesNorganismTable) <- c("species", "organism", "alias")
    return(speciesNorganismTable)
}

organism2species <- function(organism) {
    speciesNorganismTable <- speciesNorganism()
    if( sum(speciesNorganismTable[,2]==organism)==0 ){
        stop(paste("Organism\"", organism, "\"is not supported by IPI."))
    }else{
        return(speciesNorganismTable[which(speciesNorganismTable[,2]==organism),
         1])
    }
}

organism2alias <- function(organism) {
    speciesNorganismTable <- speciesNorganism()
    if( sum(speciesNorganismTable[,2]==organism)==0 ){
        stop(paste("Organism\"", organism, "\"is not supported by IPI."))
    }else{
        return(speciesNorganismTable[which(speciesNorganismTable[,2]==organism),
         3])
    }
}

getShortSciName <- function(organism){
    temp <- unlist(strsplit(organism, " +"))
    if(length(temp) < 2){
        warning(paste("Species name may not be a two-word scientic",
                      "name.", organism, "was returned."))
    }
    return(paste(toupper(substr(temp[1], 1, 1)),
                 substr(temp[2], 1, 2), sep = ""))
}

organism2taxID <- function (organism) {
    taxList <- getOption("PAnnBuilderTaxLists")
    if( sum(taxList==organism)==0 ){
        stop( paste("organism", organism, "is not corrected.") ) 
    }else{
        return( names(taxList)[taxList==organism] )
    }
}

taxID2organism <- function (taxID) {
    taxList <- getOption("PAnnBuilderTaxLists")
    if( is.null(taxList[[taxID]]) ){
        stop( paste("taxID", taxID, "is not corrected.") ) 
    }else{
        return( taxList[[taxID]] )
    }
}

## Get Url for the specific database and organism
getSrcUrl <- function(src, organism=""){
    switch(toupper(src), 
        "ALL" = return(getALLUrl(organism)),
        "SP" = return(getSPUrl()),
        "TREMBL" = return(getTREMBLUrl()),
        "IPI" = return(getIPIUrl(organism)),
        "REFSEQ" = return(getREFSEQUrl(organism)),
        "GO" = return(getGOUrl()),
        "GOA" = return(getGOAUrl(organism)),
        "GENEINT" = return(getGENEINTUrl()),
        "INTACT" = return(getINTACTUrl()),
        "MPPI" = return(getMPPIUrl()),
        "3DID" = return(get3DIDUrl()),
        "DOMINE" = return(getDOMINEUrl()),
        #"LOCATE" = return(getLOCATEUrl()),
        "DBSUBLOC" = return(getDBSUBLOCUrl()),
        "BACELLO" = return(getBACELLOUrl()),
        "HOMOLOGENE" = return(getHOMOLOGENEUrl()),
        "INPARANOID" = return(getINPARANOIDUrl(organism)),
        "INTERPRO" = return(getINTERPROUrl()),
        "PFAM" = return(getPFAMUrl()),                
        "SCOP" = return(getSCOPUrl()),
        "PEPTIDEATLAS" = return(getPEPTIDEATLASUrl(organism)),
        "SYSPTM" = return(getSYSPTMUrl()),
        "SYSBODYFLUID" = return(getSYSBODYFLUIDUrl()),
        "GONAME" = return(getGONAMEUrl()),
        "KEGGNAME" = return(getKEGGNAMEUrl()),
        "TAXNAME" = return(getTAXNAMEUrl()),
        "PFAMNAME" = return(getPFAMNAMEUrl()),
        "PROSITENAME" = return(getPROSITENAMEUrl()),
        "INTERPRONAME" = return(getINTERPRONAMEUrl()),
        stop(paste("Src", src, "is not supported.", "Has to be sp, trembl, ipi, 
        refseq, go, goa, geneint, intact, mppi, did, domain, dbsubloc, bacello, 
        interpro, pfam, scop, homologene, inparanoid, peptideatlas, sysptm, prosite, 
        sysbodyfulid or ALL"))
    )
}

getALLUrl <- function(organism){
    urls <- c(SP = getSPUrl(),
        TREMBL = getTREMBLUrl(),
        IPI = getIPIUrl(organism),
        REFSEQ = getREFSEQUrl(organism),
        GO = getGOUrl(),
        GOA = getGOAUrl(organism),
        GENEINT = getGENEINTUrl(),
        INTACT = getINTACTUrl(),
        MPPI = getMPPIUrl(),
        DID = get3DIDUrl(),
        DOMINE = getDOMINEUrl(),
        #LOCATE = getLOCATEUrl(),
        DBSUBLOC = getDBSUBLOCUrl(),
        BACELLO = getBACELLOUrl(),
        INTERPRO = getINTERPROUrl(),
        PFAM = getPFAMUrl(),
        SCOP = getSCOPUrl(),
        HOMOLOGENE = getHOMOLOGENEUrl(),
        INPARANOID = .srcUrls("InParanoid"),
        PEPTIDEATLAS = .srcUrls("PeptideAtlas"),
        SYSPTM = getSYSPTMUrl(),
        SYSBODYFLUID = getSYSBODYFLUIDUrl(),
        GONAME = getGONAMEUrl(),
        KEGGNAME = getKEGGNAMEUrl(),
        TAXNAME = getTAXNAMEUrl(),
        PFAMNAME = getPFAMNAMEUrl(),
        INTERPRONAME = getINTERPRONAMEUrl() )
    return(urls)
}


getSPUrl <- function(){
    spUrl <- paste(.srcUrls("SP"), "uniprot_sprot.dat.gz", sep="/")
    spUrl 
}

getTREMBLUrl <- function(){
    tremblUrl <- paste(.srcUrls("SP"), "uniprot_trembl.dat.gz", sep="/")
    tremblUrl
}

getIPIUrl <- function(organism){
    speciesName <- toupper(organism2species(organism))
    ipiUrl <- paste(.srcUrls("IPI"), paste( speciesName, ".gz", sep=".dat"), 
     sep="/ipi.")
    ipiUrl
}

getREFSEQUrl <- function(organism){
    tmpName <- paste(unlist(strsplit(organism,split=""))[1], unlist(strsplit(
     organism,split=" "))[2], sep="_")
    refseqUrl <- paste(.srcUrls("REFSEQ"), tmpName, "mRNA_Prot", paste(
     organism2species(organism), "protein.faa.gz", sep="."), sep="/")     
    refseqUrl
}

getGOUrl <- function(){
    goUrl <- paste(.srcUrls("GO"), "go_daily-termdb.obo-xml.gz", sep="/")
    goUrl
}

getGENEINTUrl <- function(){
    geneintUrl <- paste(.srcUrls("geneint"), "interactions.gz" , sep="/")
    geneintUrl
}

getINTACTUrl <- function(){
    intactUrl = paste(.srcUrls("intact"), "intact.txt", sep="/")
    intactUrl
}

getMPPIUrl <- function(){
    .srcUrls("MPPI")
}

getPEPTIDEATLASUrl <- function(organism){
    tmpFile <- tempfile()
    switch(organism, 
        "Human" = download.file(paste(.srcUrls("PeptideAtlas"), "human", 
         sep="/"),tmpFile),
        "Human Plasma" = download.file(paste(.srcUrls("PeptideAtlas"), 
         "human/plasma", sep="/"),tmpFile),
        "Saccharomyces cerevisiae" = download.file(paste(.srcUrls("PeptideAtlas")
         , "yeast", sep="/"),tmpFile),
        "Drosophila Melanogaster" = download.file(paste(.srcUrls("PeptideAtlas"),
         "drosophila", sep="/"),tmpFile),
        "Mouse Plasma" = download.file(paste(.srcUrls("PeptideAtlas"), 
         "mouse/plasma", sep="/"),tmpFile),
        "Halobacterium" = download.file(paste(.srcUrls("PeptideAtlas"), "halo", 
         sep="/"),tmpFile),
        stop( paste(organism, "is not supported. Has to be: Human, Human Plasma, 
         Saccharomyces cerevisiae, Drosophila Melanogaster, Mouse Plasma, 
         Halobacterium") ) 
    )
    tmp <- unlist(strsplit(paste(readLines(tmpFile),collapse=""), 
     split="sub_header"))
    unlink(tmpFile)
    if( length(grep("CURRENT BUILD",tmp))==1 ){
        tmp <- tmp[grep("CURRENT BUILD",tmp)]
    }else{
        tmp <- tmp[grep("LATEST BUILD",tmp)]
    }
    tmp <- unlist(strsplit(tmp,split=" "))
    paUrl <- vector(length=2)
    names(paUrl) <- c("fasta","map")
    paUrl["fasta"] <- paste(paste(.srcUrls("PeptideAtlas"), "human", sep="/"), 
     sub("\".*", "", sub(".*HREF=\"", "", tmp[grep("fasta",tmp)] ) ), sep="/")
    paUrl["map"] <- paste(paste(.srcUrls("PeptideAtlas"), "human", sep="/"), 
     sub("\".*", "", sub(".*HREF=\"", "", tmp[grep("coordinate_mapping.txt",tmp)] 
     ) ), sep="/")
    paUrl
}

##getLOCATEUrl <- function(){
##    tmpFile <- tempfile()
##    download.file(.srcUrls("LOCATE"),tmpFile)
##    tmp <- readLines(tmpFile)
##    unlink(tmpFile)
##    locateUrl = vector(length=2)
##    names(locateUrl) = c("mouse","human")
##    for( speciesName in names(locateUrl) ){
##        tmp <- tmp[grep(paste("LOCATE_",".*zip",sep=speciesName),tmp)]
##        locateUrl[speciesName] <- paste(.srcUrls("LOCATE"), sub("\\\".*","",
##         sub(".*href=\\\"", "", tmp)), sep="/")
##    }
##    locateUrl
##}

getBACELLOUrl <- function(){
    srcUrl <- paste(paste(.srcUrls("BaCelLo"),"datasets",sep="/"),c(
     "animals_dataset.zip","fungi_dataset.zip","plants_dataset.zip"),sep="/")
    names(srcUrl) <- c("Animals","Fungi","Plants")
    srcUrl
}

getDBSUBLOCUrl <- function(){
    paste(.srcUrls("DBSubLoc"), "data/dbsubloc2.dat.gz", sep="/")
}

getNPDUrl <- function(){
    paste(.srcUrls("NPD"),"latestdatabase.zip",sep="/")
}

getINPARANOIDUrl <- function(organism){
    tmpFile <- tempfile()
    download.file(paste(.srcUrls("InParanoid"),"cgi-bin/summary.cgi",sep="/"),
     tmpFile)
    tmp <- unlist(strsplit(paste(readLines(tmpFile),collapse=""),split="<table"))
    unlink(tmpFile)
    tmp <- unlist(strsplit(tmp[grep("Species",tmp)],split="<tr>"))
    tmp1 <- tmp[grep(organism[1],tmp)]
    f1 <- sub("\".*","",sub(".*/processed/","",tmp1)) 
    tmp2 <- tmp[grep(organism[2],tmp)]
    f2 <- sub("\".*","",sub(".*/processed/","",tmp2))

    download.file(paste(.srcUrls("InParanoid"),"download/current/sqltables",
     sep="/"),tmpFile)
    tmp <- readLines(tmpFile)
    unlink(tmpFile)
    if( length(grep( paste("sqltable",paste(f2,f1,sep="-"),sep="."),tmp))==1 ){
        s <- paste("sqltable",paste(f2,f1,sep="-"),sep=".")
    }
    if( length(grep( paste("sqltable",paste(f1,f2,sep="-"),sep="."),tmp))==1 ){
        s <- paste("sqltable",paste(f1,f2,sep="-"),sep=".")
    }
    
    url <- vector(length=3)
    names(url) <- c(organism,"org1_org2")
    url[organism[1]] <- paste(.srcUrls("InParanoid"),f1,sep=
     "/download/current/sequences/processed/")
    url[organism[2]] <- paste(.srcUrls("InParanoid"),f2,sep=
     "/download/current/sequences/processed/")
    url["org1_org2"] <- paste(.srcUrls("InParanoid"),s,sep=
     "/download/current/sqltables/")
    url
}

getHOMOLOGENEUrl <- function(){
    paste(.srcUrls("HomoloGene"),"homologene.data", sep="/")
}

getSYSPTMUrl <- function(){
    url <- vector(length=3)
    names(url) <- c("ptm","basic","xref") 
    url["ptm"] <- paste(.srcUrls("SysPTM"),"Current/ptm", sep="/")    
    url["basic"] <- paste(.srcUrls("SysPTM"),"Current/name", sep="/")
    url["xref"] <- paste(.srcUrls("SysPTM"),"Current/Protein_ID_equal", sep="/")
    url
}

getSYSBODYFLUIDUrl <- function(){
    bfUrl <- vector(length=2)
    bfUrl["bf"] <- paste(.srcUrls("Sys-BodyFluid"),"Current/experiment_protein.txt", sep="/")
    bfUrl["paper"] <- paste(.srcUrls("Sys-BodyFluid"),"Current/paper.txt", sep="/")
    bfUrl
}

getINTERPROUrl <- function(){
    paste(.srcUrls("InterPro"),"protein2ipr.dat.gz", sep="/")
}

getPFAMUrl <- function(){
    paste(.srcUrls("Pfam"),"swisspfam.gz", sep="/")
}

getPROSITENAMEUrl <- function(){
    paste(.srcUrls("PROSITE"),"prosite.dat", sep="/")
}

get3DIDUrl <- function(){
    paste(.srcUrls("3DID"),"3did_flat.gz", sep="/")
}

getDOMINEUrl<- function(){
    paste(.srcUrls("DOMINE"),"domine-tables.zip", sep="/")
}

getSCOPUrl<- function(){
    tmpFile <- tempfile()
    download.file(paste(.srcUrls("SCOP"), "parse/index.html", sep="/"), tmpFile)
    tmp <- unlist(strsplit(paste(readLines(tmpFile),collapse=""),split="<hr>"))
    unlink(tmpFile)
    tmp <- unlist(strsplit(tmp[grep("scop parseable files",tmp)],split="<li>")) 
    
    scopUrl <- vector(length=3)
    names(scopUrl) <- c("des", "hie", "cla")
    scopUrl["des"] <- paste(paste(.srcUrls("SCOP"), "parse/dir.des.scop.txt_", 
     sep="/"), sub("<.*","",sub(".*>","",unlist(strsplit(tmp[grep(
     "dir.des.scop.txt",tmp)],split="> <"))[2])) , sep="")
    scopUrl["hie"] <- paste(paste(.srcUrls("SCOP"), "parse/dir.hie.scop.txt_", 
     sep="/"), sub("<.*","",sub(".*>","",unlist(strsplit(tmp[grep(
     "dir.hie.scop.txt",tmp)],split="> <"))[2])) , sep="")
    scopUrl["cla"] <- paste(paste(.srcUrls("SCOP"), "parse/dir.cla.scop.txt_", 
     sep="/"), sub("<.*","",sub(".*>","",unlist(strsplit(tmp[grep(
     "dir.cla.scop.txt",tmp)],split="> <"))[2])) , sep="")
    scopUrl
}

getGOAUrl <- function(organism){
    tmpFile <- tempfile()
    download.file(paste(.srcUrls("GOA"), "proteome2taxid", sep="/"), tmpFile)
    tmp <- as.matrix(read.csv(tmpFile,header=F,sep="\t"))
    unlink(tmpFile)
    goaUrl <- paste(.srcUrls("GOA"), tmp[tmp[,1]==organism,3], sep="/")
    goaUrl
}

getGONAMEUrl <- function(){
    .srcUrls("GO")
}

getKEGGNAMEUrl <- function(){        
    paste(.srcUrls("KEGG"), "pathway/map_title.tab", sep="/")
}

getTAXNAMEUrl <- function(){
    paste(.srcUrls("TAX"), "taxdmp.zip", sep="/")
}

getPFAMNAMEUrl <- function(){
    paste(.srcUrls("Pfam"), "database_files/pfamA.txt.gz", sep="/")
}

getINTERPRONAMEUrl <- function(){
    paste(.srcUrls("InterPro"), "short_names.dat", sep="/")
}

## Get Built information for the specific database and organism
getSrcBuilt <- function(src, organism = "") {
    switch(toupper(src), 
        "ALL" = return(getALLBuilt(organism)),
        "SP" = return(getSPBuilt()),
        "TREMBL" = return(getTREMBLBuilt()),
        "IPI" = return(getIPIBuilt(organism)),
        "REFSEQ" = return(getREFSEQBuilt(organism)),
        "GOA" = return(getGOABuilt(organism)),
        "GENEINT" = return(getGENEINTBuilt()),
        "INTACT" = return(getINTACTBuilt()),
        "MPPI" = return(getMPPIBuilt()),
        "3DID" = return(get3DIDBuilt()),
        "DOMINE" = return(getDOMINEBuilt()),
        #"LOCATE" = return(getLOCATEBuilt()),
        "DBSUBLOC" = return(getDBSUBLOCBuilt()),
        "BACELLO" = return(getBACELLOBuilt()),
        "INTERPRO" = return(getINTERPROBuilt()),
        "PFAM" = return(getPFAMBuilt()),        
        "SCOP" = return(getSCOPBuilt()),
        "HOMOLOGENE" = return(getHOMOLOGENEBuilt()),
        "INPARANOID" = return(getINPARANOIDBuilt()),
        "PEPTIDEATLAS" = return(getPEPTIDEATLASBuilt(organism)),
        "SYSPTM" = return(getSYSPTMBuilt()),
        "SYSBODYFLUID" = return(getSYSBODYFLUIDBuilt()),
        "GONAME" = return(getGONAMEBuilt()),
        "KEGGNAME" = return(getKEGGNAMEBuilt()),
        "TAXNAME" = return(getTAXNAMEBuilt()),
        "PFAMNAME" = return(getPFAMNAMEBuilt()),
        "PROSITENAME" = return(getPROSITENAMEBuilt()),
        "INTERPRONAME" = return(getINTERPRONAMEBuilt()),
        stop(paste("Source", src, "is not supported.", "Has to be sp, trembl, 
        ipi, refseq, go, goa, geneint, intact, mppi, did, domain, dbsubloc, 
        bacello, interpro, pfam, scop, homologene, inparanoid, peptideatlas, 
        sysptm, sysbodyfulid or ALL")))
}

getALLBuilt <- function(organism){
    builts <- c(SP = getSPBuilt(),
        TREMBL = getTREMBLBuilt(),
        IPI = getIPIBuilt(organism),
        REFSEQ = getREFSEQBuilt(organism),
        GOA = getGOABuilt(organism),
        GENEINT = getGENEINTBuilt(),
        INTACT = getINTACTBuilt(),
        MPPI = getMPPIBuilt(),
        DID = get3DIDBuilt(),
        DOMINE = getDOMINEBuilt(),
        #LOCATE = getLOCATEBuilt(),
        DBSUBLOC = getDBSUBLOCBuilt(),
        BACELLO = getBACELLOBuilt(),
        INTERPRO = getINTERPROBuilt(),
        PFAM = getPFAMBuilt(),
        SCOP = getSCOPBuilt(),
        HOMOLOGENE = getHOMOLOGENEBuilt(),
        INPARANOID = getINPARANOIDBuilt(),
        PEPTIDEATLAS = "",
        SYSPTM = getSYSPTMBuilt(),
        SYSBODYFLUID = getSYSBODYFLUIDBuilt(),
        GONAME = getGONAMEBuilt(),
        KEGGNAME = getKEGGNAMEBuilt(),
        TAXNAME = getTAXNAMEBuilt(),
        PFAMNAME = getPFAMNAMEBuilt(),
        INTERPRONAME = getINTERPRONAMEBuilt()
    )
    return(builts)
}

getSPBuilt <- function(){
  tmp <- paste(.srcUrls("SP"), "reldate.txt", sep="/")
  tmpFile <- loadFromUrl(tmp)
  tmp <- readLines(tmpFile)
  unlink(tmpFile)
  spBuilt <- tmp[grep("UniProtKB/Swiss-Prot",tmp)]
  spBuilt
}

getTREMBLBuilt <- function(){
  tmp <- paste(.srcUrls("SP"), "reldate.txt", sep="/")
  tmpFile <- loadFromUrl(tmp)
  tmp <- readLines(tmpFile)
  unlink(tmpFile)
  tremblBuilt <- tmp[grep("UniProtKB/TrEMBL",tmp)]
  tremblBuilt
}

getIPIBuilt <- function(organism){
  tmp <- paste(.srcUrls("IPI"), "README", sep="/")
  tmpFile <- loadFromUrl(tmp)
  tmp <- readLines(tmpFile)
  unlink(tmpFile)
  tmp <- tmp[which(tmp=="Current release"):which(tmp=="Past releases")]
  tmp <- tmp[which(tmp=="Date\t\tSpecies\t\tVersion\t\tEntries_count"):which
   (tmp=="This release was built using:")]
  
  date <- unlist(strsplit(tmp[grep("\thuman\t",tmp)],split="\t"))[1]
  tmp <- unlist(strsplit(tmp[grep(organism2alias(organism),tmp)],"\t\t"))
  version <- tmp[tmp!=""][2]
  ipiBuilt <- paste( paste(organism, version) , date, sep=", ")
  ipiBuilt
}

getREFSEQBuilt <- function(organism){
    tmpName <- paste(unlist(strsplit(organism,split=""))[1], unlist(strsplit(
     organism,split=" "))[2], sep="_")
    tmp <- paste(.srcUrls("REFSEQ"), tmpName, "README", sep="/")   
    tmpFile <- loadFromUrl(tmp)
    tmp <- readLines(tmpFile)
    unlink(tmpFile)
    tmp <- tmp[grep("Last updated:",tmp)]
    refseqBuilt <- sub("Last updated: ", "", tmp)
    refseqBuilt
}

## getGOBuilt <- function(){
##     tmp <- paste(.srcUrls("GO"), "go_daily-termdb.obo-xml.gz", sep="/")
##     tmpFile <- loadFromUrl(tmp)
##     tmp <- readLines(tmpFile)
##     unlink(tmpFile)
##     tmp <- tmp[grep("<date>[\\d:]+",tmp,perl=T)]
##     tmp <- sub("\\s+<date>", "", tmp)
##     GOBuilt <- sub(" .*", "", tmp)
##     GOBuilt
## }

getGENEINTBuilt <- function(){
    if( .Platform$OS.type=="windows" ){
        date()
    }else{
        tmpFile <- tempfile()
        download.file(paste(.srcUrls("geneint"), "", sep="/"),tmpFile,
         method="wget")
        tmp <- readLines(tmpFile)
        unlink(tmpFile)
        tmp <- tmp[grep(paste(">","</a> ",sep=basename(.srcUrls("geneint"))), 
         tmp, perl=T)]
        gointBuilt <- paste(unlist(strsplit(tmp,"\\s+"))[2:4],collapse=" ")
        gointBuilt
    }
}

getINTACTBuilt <- function(){
    if( .Platform$OS.type=="windows" ){
        intactBuilt <- date()
    }else{
        tmpFile <- tempfile()
        download.file(paste(.srcUrls("intact"), "", sep="/"),tmpFile,
         method="wget")
        tmp <- readLines(tmpFile)
        unlink(tmpFile)
        tmp <- tmp[grep(paste(">","</a> ",sep=basename(.srcUrls("intact"))), tmp, 
         perl=T)]
        intactBuilt <- paste(unlist(strsplit(tmp,"\\s+"))[2:4],collapse=" ")
    }
    intactBuilt
}

getMPPIBuilt <- function(){
    return("")
}

getPEPTIDEATLASBuilt <- function(organism){
    tmpFile <- tempfile()
    switch(organism, 
        "Human" = download.file(paste(.srcUrls("PeptideAtlas"), "human", sep="/"
         ),tmpFile),
        "Human Plasma" = download.file(paste(.srcUrls("PeptideAtlas"), 
         "human/plasma", sep="/"),tmpFile),
        "Saccharomyces cerevisiae" = download.file(paste(.srcUrls("PeptideAtlas"),
         "yeast", sep="/"),tmpFile),
        "Drosophila Melanogaster" = download.file(paste(.srcUrls("PeptideAtlas"),
         "drosophila", sep="/"),tmpFile),
        "Mouse Plasma" = download.file(paste(.srcUrls("PeptideAtlas"), 
         "mouse/plasma", sep="/"),tmpFile),
        "Halobacterium" = download.file(paste(.srcUrls("PeptideAtlas"), "halo", 
         sep="/"),tmpFile),
        stop( paste(organism, "is not supported. Has to be: Human, Human Plasma,
         Saccharomyces cerevisiae, Drosophila Melanogaster, Mouse Plasma, 
         Halobacterium") ) 
    )
    tmp <- unlist(strsplit(paste(readLines(tmpFile),collapse=""), split=
     "sub_header"))
    unlink(tmpFile)
    if( length(grep("CURRENT BUILD",tmp))==1 ){
        tmp <- tmp[grep("CURRENT BUILD",tmp)]
    }else{
         tmp <- tmp[grep("LATEST BUILD",tmp)]
    }
    paBuilt <- sub(".*: ","",sub(" build.*","", tmp))
    paBuilt
}

##getLOCATEBuilt <- function(organism){
##    tmpFile <- tempfile()
##    download.file(.srcUrls("LOCATE"),tmpFile)
##    tmp <- readLines(tmpFile)
##    unlink(tmpFile)
##    locateBuilt = vector(length=2)
##    names(locateBuilt) = c("mouse","human")
##    for( speciesName in names(locateBuilt) ){
##        tmp <- tmp[grep(paste("LOCATE_",".*zip",sep=speciesName),tmp)]
##        locateBuilt <- sub("\\.xml.*","",sub(".*_", ,"", sub("\\\".*","",
##         sub(".*href=\\\"", "", tmp)) ))
##    }
##    locateBuilt 
##}

getBACELLOBuilt <- function(){
    tmpFile <- tempfile()
    download.file(paste(.srcUrls("BaCelLo"), "dataset.htm", sep="/"),tmpFile)
    tmp <- readLines(tmpFile)
    unlink(tmpFile)
    tmp <- tmp[grep("Datasets derived from",tmp)]
    bacelloBuilt <- sub(".*Datasets derived from ","",sub(":.*","", tmp))
    bacelloBuilt
}

getDBSUBLOCBuilt <- function(){
    tmpFile <- tempfile()
    download.file(paste(.srcUrls("DBSubLoc"), "download.html", sep="/"),tmpFile)
    tmp <- readLines(tmpFile)
    unlink(tmpFile)
    tmp <- tmp[grep("Last Modified:",tmp)]
    dbsublocBuilt <- sub("<.*", "", sub(".*Modified: ","",tmp))
    dbsublocBuilt
}

getINPARANOIDBuilt <- function(){
    tmpFile <- tempfile()
    download.file(paste(.srcUrls("InParanoid"), "download/current/relnotes.txt",
     sep="/"),tmpFile)
    tmp <- readLines(tmpFile)
    unlink(tmpFile)
    r <- sub("Release\\s*", "", tmp[grep("Release",tmp)])
    inpBuilt <- paste( paste("Release",r) , unlist(strsplit(tmp[grep(paste("^", 
     "\\s*",sep=r),tmp)], split="\t"))[2], sep=", ")
    inpBuilt
}

getHOMOLOGENEBuilt <- function(){
    if( .Platform$OS.type=="windows" ){
        homoBuilt <- date()
    }else{
        tmpFile <- tempfile()
        download.file(paste(.srcUrls("HomoloGene"), "", sep="/"), tmpFile, 
         method="wget")
        tmp <- readLines(tmpFile)
        unlink(tmpFile)
        tmp <- tmp[grep("homologene.data",tmp)]
        homoBuilt <- paste(unlist(strsplit(tmp,split="\\s*"))[2:4], sep="", 
         collapse=" ")
    }
    homoBuilt
}

getSYSPTMBuilt <- function(){
    tmpFile <- tempfile()
    download.file(paste(.srcUrls("SysPTM"),"release.txt", sep="/"),tmpFile)
    tmp <- readLines(tmpFile)
    unlink(tmpFile)
    ptmBuilt <- sub(".*Current:\\s*", "", tmp[grep("^Current:",tmp)])
    ptmBuilt
}

getSYSBODYFLUIDBuilt <- function(){
    tmpFile <- tempfile()
    download.file(paste(.srcUrls("Sys-BodyFluid"), "release.txt", sep="/"), 
     tmpFile)
    tmp <- readLines(tmpFile)
    unlink(tmpFile)    
    sub(".*Current:\\s*", "", tmp[grep("^Current:",tmp)])
}

getINTERPROBuilt <- function(){
    if( .Platform$OS.type=="windows" ){
        date()
    }else{
        tmpFile <- tempfile()
        download.file(paste(.srcUrls("InterPro"), "release_notes.txt", sep="/"), 
         tmpFile,method="wget")
        tmp <- readLines(tmpFile)
        unlink(tmpFile)
        tmp[grep("Release\\s*\\d",tmp,perl=T)]
    }
}

getPFAMBuilt <- function(){
    tmpFile <- tempfile()
    download.file(paste(.srcUrls("Pfam"), "relnotes.txt", sep="/"), tmpFile)
    tmp <- readLines(tmpFile)
    unlink(tmpFile)
    r <- sub(".*RELEASE\\s*","",tmp[2])
    tmp <- tmp[sapply(tmp,function(x){unlist(strsplit(x, split="\\s+"))[2]})==r]
    tmp <- tmp[!is.na(tmp)]
    pfamBuilt <- paste( paste("Release",r) , 
     unlist(strsplit(tmp, split="\\s+"))[3], sep=", ")
    pfamBuilt
}

getPROSITENAMEBuilt <- function(){
    tmpFile <- tempfile()
    download.file(paste(.srcUrls("PROSITE"), "ps_reldt.txt", sep="/"), tmpFile)
    tmp <- readLines(tmpFile)
    unlink(tmpFile)
    tmp
}

get3DIDBuilt <- function(){
    if( .Platform$OS.type=="windows" ){
        didBuilt <- date()
    }else{
        tmpFile <- tempfile()
        download.file(paste(.srcUrls("3DID"), "", sep="/"),tmpFile, method="wget")
        tmp <- readLines(tmpFile)
        unlink(tmpFile)
        tmp <- tmp[grep("3did_flat",tmp)]
        didBuilt <- unlist(strsplit(sub(".*</a>","",tmp),split="\\s+"))[2]
    }
}

getDOMINEBuilt <- function(){
    tmpFile <- tempfile()
    download.file(paste(.srcUrls("DOMINE"),
     "cgi-bin/Domine?choice=1&page=about&title=About%20Domine",sep="/"),tmpFile)
    tmp <- readLines(tmpFile)
    unlink(tmpFile)
    r <- sub("<.*","",sub(".*Version\\s*","",tmp[grep("Version",tmp)]))
    domineBuilt <- paste( paste("Version",r) , 
     sub("<.*","",sub(".*Released\\s*","",tmp[grep("Released",tmp)])), sep=", ")
    domineBuilt
}

getSCOPBuilt <- function(){
    tmpFile <- tempfile()
    download.file(paste(.srcUrls("SCOP"),"index.html",sep="/"),tmpFile)
    tmp <- readLines(tmpFile)
    unlink(tmpFile)
    tmp<- tmp[grep("release",tmp)][1]
    scopBuilt <- paste( paste("Release", sub(".*>", "", sub("\\s*release.*","",
     tmp) )), sub(".*\\(", "", sub("\\).*","",tmp) ), sep=", ")
    scopBuilt
}

getGOABuilt <- function(organism){
    tmpFile <- tempfile()
    download.file(paste(.srcUrls("GOA"), "proteome2taxid", sep="/"), tmpFile)
    tmp <- as.matrix(read.csv(tmpFile,header=F,sep="\t"))
    unlink(tmpFile)
    n <- tmp[tmp[,1]==organism,3]
    tmpFile <- tempfile()
    if( .Platform$OS.type=="windows" ){
        goaBuilt<- date()
    }else{
        download.file(paste(.srcUrls("GOA"), "", sep="/"), tmpFile, method="wget")
        tmp <- readLines(tmpFile)
        unlink(tmpFile)
        goaBuilt <- paste(unlist(strsplit(tmp[grep(n,tmp)],split="\\s+"))[2:4],
         collapse=" ")
    }
    goaBuilt
}

getGONAMEBuilt <- function(){  
  tmpFile <- tempfile()
  download.file(.srcUrls("GO"),tmpFile)
  tmp <- readLines(tmpFile)
  unlink(tmpFile)
  tmp <- tmp[grep("^date: ",tmp)]
  tmp <- sub("^date: ","",tmp)
  tmp
}

getKEGGNAMEBuilt <- function(){    
    if( .Platform$OS.type=="windows" ){
        date()
    }else{
        tmpFile <- tempfile()  
        download.file(paste(.srcUrls("KEGG"), "pathway/", sep="/"), tmpFile, 
         method="wget")
        tmp <- readLines(tmpFile)
        unlink(tmpFile)
        paste(unlist(strsplit(tmp[grep("map_title.tab",tmp)], split="\\s+"))[2:4], 
         collapse=" ")
    }
}

getTAXNAMEBuilt <- function(){
    if( .Platform$OS.type=="windows" ){
        date()
    }else{
        tmpFile <- tempfile()
        download.file(paste(.srcUrls("TAX"), "", sep="/"),tmpFile,method="wget")
        tmp <- readLines(tmpFile)
        unlink(tmpFile)
        paste(unlist(strsplit(tmp[grep("taxdmp.zip",tmp)], split="\\s+"))[2:4], 
         collapse=" ")
    } 
}

getPFAMNAMEBuilt <- function(){
    getPFAMBuilt()
}

getINTERPRONAMEBuilt <- function(){
    getINTERPROBuilt()
}


getSrcSQUrl <- function(src,organism){
    switch(toupper(src), 
        "SP" = return(getSPSQUrl()),
        "TREMBL" = return(getTREMBLSQUrl()),
        "IPI" = return(getIPISQUrl(organism)),
        "GI" = return(getREFSEQSQUrl(organism)),
        "REFSEQ" = return(getREFSEQSQUrl(organism)),
        stop(paste("Src", src, "is not supported.", "Has to be sp, trembl, ipi 
         gi, or refseq"))
    )
}

getSPSQUrl <- function(){
  spUrl <- paste(.srcUrls("SP"), "uniprot_sprot.fasta.gz", sep="/")
  spUrl 
}

getTREMBLSQUrl <- function(){
  tremblUrl <- paste(.srcUrls("SP"), "uniprot_trembl.fasta.gz", sep="/")
  tremblUrl
}

getIPISQUrl <- function(organism){
    speciesName <- toupper(organism2species(organism))
    ipiUrl <- paste(.srcUrls("IPI"), paste( speciesName, ".gz", sep=".fasta"), 
     sep="/ipi.")
    ipiUrl
}

getREFSEQSQUrl <- function(organism){
    tmpName <- paste(unlist(strsplit(organism,split=""))[1], 
     unlist(strsplit(organism,split=" "))[2], sep="_")
    refseqUrl <- paste(.srcUrls("REFSEQ"), tmpName, "mRNA_Prot", 
     paste(organism2species(organism), "protein.faa.gz", sep="."), sep="/")   
    refseqUrl
}

getSrcSQBuilt <- function(src,organism){
    switch(toupper(src), 
        "SP" = return(getSPBuilt()),
        "TREMBL" = return(getTREMBLBuilt()),
        "IPI" = return(getIPIBuilt(organism)),
        "REFSEQ" = return(getREFSEQBuilt(organism)),
        "GI" = return(getREFSEQBuilt(organism)),
        stop(paste("Src", src, "is not supported.", "Has to be sp, trembl, 
         ipi or refseq"))
    )
}
