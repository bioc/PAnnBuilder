## Several functions are from Bioconductor "AnnBuilder" package, and modified by
## Hong Li, 2008.

getBaseParsers <- function(baseMapType, db=FALSE){
  ## Get the path of parser (segment of perl program)
  ##
  ## baseMapType - a character string that indicate which parser will be used.
  ##     baseMapType    Parser    Use
  ##     "sp"    spParser    parse protein data from SwissProt or TrEMBL
  ##     "trembl"    spParser    parse protein data from SwissProt or TrEMBL
  ##     "ipi"    ipiParser    parse protein data from IPI
  ##     "refseq"    refseqParser    parse protein data from NCBI RefSeq
  ##     "equal"    equalParser    find protein ID mapping with equal sequences
  ##     "merge"    mergeParser    merge different ID mapping files
  ##     "mppi"    mppiParser    parse protein protein interaction data from MIPS
  ##     "PeptideAtlas"    paParser    parse data from PeptideAtlas
  ##     "DBSubLoc"    dbsublocParser    parse data from DBSubLoc
  ##     "Pfam"    pfamParser    parse data from Pfam
  ##     "pfamname"    pfamNameParser    parse domain id and name from Pfam
  ##     "blast"    blastParser    filter the results of blast
  ##     "interpro"    interproParser    parse data from InterPro
  ##
  ## db - a boolean to indicate whether the parser of "sqlite based package" 
  ##     will be used.
  ##
  ##     Copyright 2008, Hong Li, all rights reserved.
  ##     
     
    # Default directory of parsers
    path <- file.path(.path.package("PAnnBuilder"), "scripts")
    parser <- switch(toupper(baseMapType),
           SP = file.path(path, "spParser"),
           TREMBL = file.path(path, "spParser"),
           IPI = file.path(path, "ipiParser"),
           REFSEQ = file.path(path, "refseqParser"),
           EQUAL = file.path(path, "equalParser"),
           MERGE = file.path(path, "mergeParser"),
           MPPI = file.path(path, "mppiParser"),
           PEPTIDEATLAS = file.path(path, "paParser"),
           DBSUBLOC = file.path(path, "dbsublocParser"),
           PFAM = file.path(path, "pfamParser"),
           PFAMNAME = file.path(path, "pfamNameParser"),
           BLAST = file.path(path, "blastParser"),
           INTERPRO = file.path(path, "interproParser"),
           stop("Invalid baseMapType")
    )
    if( db ){
        parser <- paste(parser, "DB", sep="_")
    }
    parser
}


fileMuncher <- function(outName, dataFile, parser, organism){
    OS <- .Platform$OS.type
    perlName <- paste(tempfile("tempPerl"), "pl", sep=".")

    writePerl <- function(toWrite){
        write(toWrite, file = perlName, append = TRUE)
    }

    ## Convert \ => / for Windows
    if (OS == "windows") {
        outName <- chartr("\\", "/", outName)
        dataFile <- chartr("\\", "/", dataFile)
    }

    if(!file.create(perlName))
        stop(paste("You do not have write permission to the ",
                   "directory for the Perl script createded", sep = ""))
    if(OS == "unix"){
        perlBin <- system("which perl", intern = TRUE)
        if(length(perlBin) > 1){
            stop("Perl is not available!")
        }
        writePerl(paste("#!", perlBin, "\n\n", sep = ""))
    }else if(OS == "windows"){
        writePerl("#!/usr/bin/perl -w\n\n")
    }


    if(dataFile != "" && !is.null(dataFile) && !is.na(dataFile)){
        statement <- paste("open(DATA, \"<", dataFile, "\") || die ",
                     "\"Can not open ", dataFile, "\";\n", sep = "")
        writePerl(statement)
    }

    delete <- writeInput(basename(parser), perlName, organism, dataFile)

    statement <- paste("open(OUT, \">", outName, "\") || die ",
                 "\"Can not open ", outName, "\";\n\n", sep = "")
    writePerl(statement)
    

    if(!is.null(parser))
        writePerl(readLines(parser))

    .callPerl(perlName, OS)    
    unlink(delete)
    return (outName)
}

fileMuncher_DB <- function(dataFile, parser, organism){
    OS <- .Platform$OS.type
    perlName <- paste(tempfile("tempPerl"), "pl", sep=".")

    writePerl <- function(toWrite){
        write(toWrite, file = perlName, append = TRUE)
    }

    ## Convert \ => / for Windows
    if (OS == "windows") {
        dataFile <- chartr("\\", "/", dataFile)
    }

    if(!file.create(perlName))
        stop(paste("You do not have write permission to the ",
                   "directory for the Perl script createded", sep = ""))
    if(OS == "unix"){
        perlBin <- system("which perl", intern = TRUE)
        if(length(perlBin) > 1){
            stop("Perl is not available!")
        }
        writePerl(paste("#!", perlBin, "\n\n", sep = ""))
    }else if(OS == "windows"){
        writePerl("#!/usr/bin/perl -w\n\n")
    }

    if(dataFile != "" && !is.null(dataFile) && !is.na(dataFile)){
        statement <- paste("open(DATA, \"<", dataFile, "\") || die ",
                     "\"Can not open ", dataFile, "\";\n", sep = "")
        writePerl(statement)
    }

    delete <- writeInput(basename(parser), perlName, organism, dataFile)
    outNames <- writeOutput(basename(parser), perlName)        

    if(!is.null(parser))
        writePerl(readLines(parser))

    .callPerl(perlName, OS)
    unlink(delete)
    return (outNames)
}

writeOutput <- function(parser, perlName){
    OS <- .Platform$OS.type
    outNames = switch(parser,
           "spParser_DB" = c( 
                           basic = tempfile("tempOut"),
                           seq = tempfile("tempOut"),                           
                           de = tempfile("tempOut"),
                           spac = tempfile("tempOut"),                      
                           pubmed = tempfile("tempOut"),
                           refseq  = tempfile("tempOut"), 
                           geneid = tempfile("tempOut"),                           
                           pdb = tempfile("tempOut"),
                           kegg = tempfile("tempOut"),
                           alias = tempfile("tempOut"),
                           int = tempfile("tempOut"),
                           ptm = tempfile("tempOut"),
                           go = tempfile("tempOut"),
                           pfam = tempfile("tempOut"),
                           interpro = tempfile("tempOut"),
                           path = tempfile("tempOut"),
                           prosite = tempfile("tempOut")
                         ), 
           "ipiParser_DB" = c( 
                           basic = tempfile("tempOut"),
                           seq = tempfile("tempOut"),
                           ipiac = tempfile("tempOut"),
                           go = tempfile("tempOut"),
                           pfam = tempfile("tempOut"),
                           interpro = tempfile("tempOut"),
                           path = tempfile("tempOut"),
                           prosite = tempfile("tempOut")
                         ),
           "refseqParser_DB" = c( 
                           basic = tempfile("tempOut"),
                           seq = tempfile("tempOut"),
                           go = tempfile("tempOut"),                           
                           path = tempfile("tempOut")                       
                         ),
           return("") 
    ) 
    for (i in names(outNames) ){        
        outName <- outNames[i]
        if (OS == "windows") {
            outName <- chartr("\\", "/", outNames[i] )
        }
        statement <- paste("open(OUT_", i, ", \">", outName, "\") || die ",
                     "\"Can not open ", outName, "\";\n\n", sep = "")        
        write(statement, file = perlName, append = TRUE)
    }
    outNames
}  
  
writeInput <- function(parser, perlName, organism, dataFile){
    switch(parser, 
           "spParser" = return(writeInputSP(perlName,organism)),
           "spParser_DB" = return(writeInputSP(perlName,organism)),
           "ipiParser_DB" = return(writeInputIPI(perlName,organism)),
           "ipiParser" = return(writeInputIPI(perlName,organism)),
           "refseqParser" = return(writeInputREFSEQ(perlName,organism)),
           "refseqParser_DB" = return(writeInputREFSEQ(perlName,organism)),
           "blastParser" = return(writeInputBLAST(perlName,organism, dataFile)),
           "pfamParser" = return(writeInputPFAM(perlName,organism)),
           "interproParser" = return(writeInputINTERPRO(perlName,organism)),
    return("") )  
}

writeInputBLAST <- function(perlName,organism, dataFile){
    OS <- .Platform$OS.type
    statement <- paste("$organism = \"", "\";\n" , sep = organism )
    write(statement, file = perlName, append = TRUE)    
    qFile <- paste(dataFile,"qLength",sep=".")
    if (OS == "windows") {
        qFile <- chartr("\\", "/", qFile)
    }
    statement <- paste("open(IN1, \"<", qFile, "\") || die ",
                 "\"Can not open ", qFile, "\";\n", sep = "")
    write(statement, file = perlName, append = TRUE)
    sFile <- paste(dataFile,"sLength",sep=".")
    if (OS == "windows") {
        sFile <- chartr("\\", "/", sFile)
    }
    statement <- paste("open(IN2, \"<", sFile, "\") || die ",
                 "\"Can not open ", sFile, "\";\n", sep = "")
    write(statement, file = perlName, append = TRUE)
        
    return( c(perlName,qFile,sFile) )
}

writeInputPFAM <- function(perlName,organism){
    OS <- .Platform$OS.type
    statement <- paste("$organism = \"", "\";\n" , sep = organism )
    write(statement, file = perlName, append = TRUE)    
    spFile <- loadFromUrl(getSrcUrl("sp"))
    if (OS == "windows") {
        spFile <- chartr("\\", "/", spFile)
    }
    statement <- paste("open(IN, \"<", spFile, "\") || die ",
                 "\"Can not open ", spFile, "\";\n", sep = "")
    write(statement, file = perlName, append = TRUE)
    
    return( c(perlName,spFile) )
}

writeInputINTERPRO <- function(perlName,organism){
    OS <- .Platform$OS.type
    statement <- paste("$organism = \"", "\";\n" , sep = organism )
    write(statement, file = perlName, append = TRUE)    
    spFile <- loadFromUrl(getSrcUrl("sp"))
    if (OS == "windows") {
        spFile <- chartr("\\", "/", spFile)
    }
    statement <- paste("open(IN1, \"<", spFile, "\") || die ",
                 "\"Can not open ", spFile, "\";\n", sep = "")
    write(statement, file = perlName, append = TRUE)
    tremblFile <- loadFromUrl(getSrcUrl("trembl"))
    if (OS == "windows") {
        tremblFile <- chartr("\\", "/", tremblFile)
    }
    statement <- paste("open(IN2, \"<", tremblFile, "\") || die ",
                 "\"Can not open ", tremblFile, "\";\n", sep = "")
    write(statement, file = perlName, append = TRUE)
    
    return( c(perlName,spFile,tremblFile) )
}

writeInputSP <- function(perlName,organism){
    OS <- .Platform$OS.type
    tmpName <- tolower(getShortSciName(organism))
    PATHUrl <- paste(.srcUrls("KEGG"), paste(tmpName,paste(tmpName,
               "pathway.list",sep="_"),sep="/"), sep="/genes/organisms/")
    PATHFile <- loadFromUrl(PATHUrl)
    if (OS == "windows") {
        PATHFile <- chartr("\\", "/", PATHFile)
    }
    statement <- paste("open(In_PATH, \"<", PATHFile, "\") || die ",
                 "\"Can not open ", PATHFile, "\";\n", sep = "")
    write(statement, file = perlName, append = TRUE)
  
    if(organism != "" && !is.null(organism) && !is.na(organism)){
        statement <- paste("$organism = \"", "\";\n" , sep = organism )
        write(statement, file = perlName, append = TRUE)
    }
    
    return( c(perlName, PATHFile) )
}

writeInputIPI <- function(perlName,organism){
    OS <- .Platform$OS.type
    tmpName <- tolower(getShortSciName(organism))
    PATHUrl <- paste(.srcUrls("KEGG"), paste(tmpName,paste(tmpName,
               "pathway.list",sep="_"),sep="/"), sep="/genes/organisms/")
    PATHFile <- loadFromUrl(PATHUrl)
    if (OS == "windows") {
        PATHFile <- chartr("\\", "/", PATHFile)
    }
    statement <- paste("open(In_PATH, \"<", PATHFile, "\") || die ",
                 "\"Can not open ", PATHFile, "\";\n", sep = "")
    write(statement, file = perlName, append = TRUE)
  
    tmpName <- tolower(getShortSciName(organism))
    KEGGUrl <- paste(.srcUrls("KEGG"), paste(tmpName,paste(tmpName,
               "ncbi-geneid.list",sep="_"),sep="/"), sep="/genes/organisms/")
    KEGGFile <- loadFromUrl(KEGGUrl)
    if (OS == "windows") {
        KEGGFile <- chartr("\\", "/", KEGGFile)
    }
    statement <- paste("open(In_KEGG, \"<", KEGGFile, "\") || die ",
                 "\"Can not open ", KEGGFile, "\";\n", sep = "")
    write(statement, file = perlName, append = TRUE)
  
    GOFile <- loadFromUrl(paste(.srcUrls("Gene"), "gene2go.gz", sep="/"))
    if (OS == "windows") {
        GOFile <- chartr("\\", "/", GOFile)
    }
    statement <- paste("open(In_GO, \"<", GOFile, "\") || die ",
                 "\"Can not open ", GOFile, "\";\n", sep = "")
    write(statement, file = perlName, append = TRUE)
  
    if(organism != "" && !is.null(organism) && !is.na(organism)){
        statement <- paste("$organism = \"", "\";\n" , sep = 
                     as.character(organism2taxID(organism)) )
        write(statement, file = perlName, append = TRUE)
    }
    
    return( c(perlName, PATHFile, KEGGFile, GOFile) )
}

writeInputREFSEQ <- function(perlName,organism){
  OS <- .Platform$OS.type
  tmpName <- tolower(getShortSciName(organism))
  PATHUrl <- paste(.srcUrls("KEGG"), paste(tmpName,paste(tmpName,
             "pathway.list",sep="_"),sep="/"), sep="/genes/organisms/")
  PATHFile <- loadFromUrl(PATHUrl)
  if (OS == "windows") {
      PATHFile <- chartr("\\", "/", PATHFile)
  }
  statement <- paste("open(In_PATH, \"<", PATHFile, "\") || die ",
               "\"Can not open ", PATHFile, "\";\n", sep = "")
  write(statement, file = perlName, append = TRUE)
  
  tmpName <- tolower(getShortSciName(organism))
  KEGGUrl <- paste(.srcUrls("KEGG"), paste(tmpName,paste(tmpName,
             "ncbi-gi.list",sep="_"),sep="/"), sep="/genes/organisms/")
  KEGGFile <- loadFromUrl(KEGGUrl)
  if (OS == "windows") {
      KEGGFile <- chartr("\\", "/", KEGGFile)
  }
  statement <- paste("open(In_KEGG, \"<", KEGGFile, "\") || die ",
               "\"Can not open ", KEGGFile, "\";\n", sep = "")
  write(statement, file = perlName, append = TRUE)
  
  GOFile <- loadFromUrl(paste(.srcUrls("Gene"), "gene2go.gz", sep="/"))
  if (OS == "windows") {
      GOFile <- chartr("\\", "/", GOFile)
  }
  statement <- paste("open(In_GO, \"<", GOFile, "\") || die ",
               "\"Can not open ", GOFile, "\";\n", sep = "")
  write(statement, file = perlName, append = TRUE)
  
  Gene2refseqFile <- loadFromUrl(paste(.srcUrls("Gene"), "gene2refseq.gz", 
                     sep="/"))
  if (OS == "windows") {
      Gene2refseqFile <- chartr("\\", "/", Gene2refseqFile)
  }
  statement <- paste("open(In_Gene2refseq, \"<", Gene2refseqFile, "\") || die ",
               "\"Can not open ", Gene2refseqFile, "\";\n", sep = "")
  write(statement, file = perlName, append = TRUE)
  
  GeneInfoFile <- loadFromUrl(paste(.srcUrls("Gene"), "gene_info.gz", sep="/"))
  if (OS == "windows") {
      GeneInfoFile <- chartr("\\", "/", GeneInfoFile)
  }
  statement <- paste("open(In_GeneInfo, \"<", GeneInfoFile, "\") || die ",
               "\"Can not open ", GeneInfoFile, "\";\n", sep = "")
  write(statement, file = perlName, append = TRUE)
 
  return(c(perlName, PATHFile, KEGGFile, GOFile, Gene2refseqFile, GeneInfoFile)) 
}

.callPerl <- function(script, os){
    if(os == "unix"){
        system(paste("chmod +x", script))
        system(script)
    }else if(os == "windows"){
        script <- gsub("/", "\\\\", script)
        system(paste("perl", script))
    }else{
        stop(paste("Do not know who to run perl under ", os))
    }
}

getSrcObjs <- function(srcUrls, organism,
    built, fromWeb=TRUE
    ){
  ## Define objects of class "pBase"
  ##
  ## Copyright 2008, Hong Li, all rights reserved.
  ##
  srcObjs <- list()
  for(i in names(srcUrls)){
       switch(toupper(i),
              SP = baseParser <- getBaseParsers("sp"),
              IPI = baseParser <- getBaseParsers("ipi"),
              REFSEQ = baseParser <- getBaseParsers("refseq"),          
              stop("Invalid imput for srcUrls")
       )
      switch(toupper(i),
             SP = srcObjs[["SP"]] <- pBase(srcUrls[i], parser = baseParser, 
                  built = built, fromWeb = fromWeb, organism = organism),
             TREMBL = srcObjs[["TREMBL"]] <- pBase(srcUrls[i], parser = baseParser,
                  built = built, fromWeb = fromWeb, organism = organism),
             IPI = srcObjs[["IPI"]] <- pBase(srcUrls[i], parser = baseParser, 
                  built = built, fromWeb = fromWeb, organism = organism),
             REFSEQ = srcObjs[["REFSEQ"]] <- pBase(srcUrls[i],parser = baseParser, 
                  built = built, fromWeb = fromWeb, organism = organism),             
             stop("Invalid name for srcUrls")
      )
  }

  return( srcObjs )
}

getBaseData <- function(srcObjs ){
  ## Get basic protein annotation data and sequence data from three protein 
  ## database: SwissProt, TREMBL, IPI, NCBI PefSeq.
  ##
  ## srcObj - a object where source annotation data will be retained. 
  ##                    Valid annotation data are from Swiss-Prot, TREMBL, International Protein Index and NCBI Reference Sequence. 
  ##
  ## Copyright 2008, Hong Li, all rights reserved.
  ##    
  for(i in names(srcObjs)){
      switch(toupper(i),
             SP = srcData <- parseData(srcObjs[["SP"]], sep = "\t", 
                             mergeKey = FALSE),
             TREMBL = srcData <- parseData(srcObjs[["TREMBL"]], sep = "\t", 
                                 mergeKey = FALSE),
             IPI = srcData <- parseData(srcObjs[["IPI"]], sep = "\t", 
                              mergeKey = FALSE),
             REFSEQ = srcData <- parseData(srcObjs[["REFSEQ"]], sep = "\t", 
                                 mergeKey = FALSE),
             stop("Invalid name for srcObjs")
      )
  }
  return ( srcData )     
}

## Split multiple entry for a given mapping
splitEntry <- function(dataRow, sep = ";", asNumeric = FALSE){
    if(is.na(dataRow) || is.null(dataRow) || dataRow == ""){
        return(NA)
    }else{
        if(asNumeric){
            return(unique(as.numeric(unlist(strsplit(dataRow, sep)))))
        }else{
            return(unique(unlist(strsplit(dataRow, sep))))
        }
    }
}

## Split multiple entry with two separaters (e. g. 12345@18;67891@18)
twoStepSplit <- function(dataRow, entrySep = ";", eleSep = "@",
    asNumeric = FALSE){
    if(asNumeric){
        tmp <- unique(unlist(strsplit(dataRow,split=entrySep))) 
        tmp <- lapply(tmp,function(x){as.numeric(unlist(strsplit(x,split=eleSep)))}) 
        names(tmp) <- sapply(tmp,function(x){x[1]}) 
    }else{    
        tmp <- unique(unlist(strsplit(dataRow,split=entrySep))) 
        tmp <- lapply(tmp,function(x){unlist(strsplit(x,split=eleSep))}) 
        names(tmp) <- sapply(tmp,function(x){x[1]}) 
        }    
    tmp[sapply(tmp,function(x){sum(!is.na(x))==0})] <- NA
    result <- tmp
    return(result)
}



## This function resolves the one to may relationship beween ids
## contained in a file in one column and the values in another
## column. Duplicating values for the same id will be merged as one
## value separated by ";".
##
## Copyright 2002, J. Zhang. All rights reserved.
##
mergeRowByKey <- function (mergeMe, keyCol = 1, sep = ";"){
    mergeTwoCol <- function(x){
        return(paste(unique(x[, -keyCol]), sep = "", collapse = ";"))
    }
    mergeMultiCol <- function(x){
        return(apply(x, 2, function(y)
                     paste(unique(y), sep = "", collapse = ";")))
    }
    # Returns the original value if mergeMe is a vector
    if(is.null(ncol(mergeMe))){
        return(mergeMe)
    }
    merged <- split.data.frame(mergeMe, factor(mergeMe[, keyCol]))
    if(ncol(mergeMe) == 2){
        merged <- sapply(merged, mergeTwoCol)
        merged <- cbind(names(merged), merged)
        colnames(merged) <- c(colnames(mergeMe)[keyCol],
                              colnames(mergeMe)[-keyCol])
        return(merged)

    }else{
        merged <- sapply(merged, mergeMultiCol)
        merged <- t(merged)
        colnames(merged) <- colnames(mergeMe)
        return(merged)
    }
}


            
#key2GO <- function(goEnv, GOType, mapType, keyword){    
    ## GOType - 
    ##          "CC"
    ##          "BP"
    ##          "MF"
    ##
    ## mapType -
    ##          "equal"
    ##          "grep"
    ##          "bottomup"
    ##          "all"
    ##
    ## GO2NAME -
    ##
    ## keyword -    
    ##          GOType=="CC": keyword=c("membrane","plasma membrane","nucleus","mitochondrion","Golgi apparatus","cytoplasm","peroxisome","lysosome","endoplasmic reticulum","cytosol")
    ##                        GOID:   c("GO:0016020","GO:0005886","GO:0005634","GO:0005739","GO:0005794","GO:0005737","GO:0005777","GO:0005764","GO:0005783","GO:0005829")
    ##          GOType=="BP": keyword=c("biological regulation","developmental process","cell communication","cell cycle","cell development","cell proliferation","gene expression","transcription","regulation of transcription")
    ##                        GOID:   c("GO:0065007","GO:0032502","GO:0007154","GO:0007049","GO:0048468","GO:0008283","GO:0010467","GO:0006350","GO:0045449")
    ##          GOType=="MF": keyword=c("protein binding","transcription factor binding","receptor binding","catalytic activity","transporter activity","transcription regulator activity","transferase activity" )
    ##                        GOID:   c("GO:0005515","GO:0008134","GO:0005102","GO:0003824","GO:0005215","GO:0030528","GO:0016740")
    ##
#        if( any(c( missing(GOType), missing(mapType), missing(goEnv),is.null(GOType), is.null(mapType), is.null(goEnv) )) ){
#            stop("Parameters goEnv, GOType, mapType can not be missing or NULL" )
#        }
#    if (any (GOType == c("CC","BP","MF"))){
#        GOType = toupper(GOType)
#    }else{
#        stop(paste("GOType", GOType, "is not supported.", "Has to be CC, BP or MF") )
#    }
#    if( missing(keyword) ){
#        keyword = switch( GOType, 
#            "CC" = c("membrane","plasma membrane","nucleus","mitochondrion","Golgi apparatus","cytoplasm","peroxisome","lysosome","endoplasmic reticulum","cytosol"), 
#            "BP" = c("biological regulation","developmental process","cell communication","cell cycle","cell development","cell proliferation","gene expression","transcription","regulation of transcription"), 
#            "MF" = c("protein binding","transcription factor binding","receptor binding","catalytic activity","transporter activity","transcription regulator activity","transferase activity" ), 
#            stop (paste("GOType", GOType, "is not supported.", "Has to be CC, BP or MF") )
#            )
#    }
#          
#    data = as.list(goEnv$NAME)
#    if(mapType == "equal"){
#        GOs = lapply(keyword,function(x){names(data)[data==x]})
#    }
#    if(mapType == "grep"){
#        GOs = lapply(keyword,function(x){names(data)[grep(x,data, ignore.case=T)]})
#    }
#    if(mapType == "bottomup"){
#        children = as.list(goEnv[[paste(GOType,"OFFSPRING",sep="")]])
#        GOs = lapply(keyword,function(x){ y=names(data)[data==x] ; z=unique(c(y,unlist(lapply(intersect(names(children),y), function(z){ children[[z]]} )))) ; z=z[!is.na(z)] })
#    }
#    if(mapType == "all"){
#        children = as.list(goEnv[[paste(GOType,"OFFSPRING",sep="")]])
#        GOs = lapply(keyword,function(x){ y=names(data)[grep(x,data, ignore.case=T)] ; z=unique(c(y,unlist(lapply(intersect(names(children),y), function(z){ children[[z]]} )))); z=z[!is.na(z)] })
#    }
#    names(GOs) = keyword
#    
#    keys = rep(NA,length(data))
#    names(keys) = names(data)
#    for (x in keyword){
#        keys[GOs[[x]]] = lapply( keys[GOs[[x]]] , function(y){ if(sum(!is.na(y))==0){x}else{c(y,x)} } )
#    }
#    
#    return(list(key2GOID=GOs, GOID2key=keys))
#}

#ID2GOkey <- function(ID2GO, goEnv, GOType, mapType, keyword) {
    ## Map given protein IDs to the given CC keyword via GO ID.
    ##
    ## ID2GO -
    ##
    ## GOType - interested type of GO term
    ##          CC: cellular component
    ##          BP: biological process
    ##          MF: molecular function
    ##
    ## mapType - a character string for finding the GO terms of protein.
    ##           equal: the name of GO term is equal to one keyword
    ##         grep: the name of GO term contain the keyword
    ##         bottomup: all protein mapping to the children GO term are regard as also mapping to the father GO term.
    ##         all:    
    ##
    ## keyword - a character vector for the GO term
    ##
    ## Copyright 2008, Hong Li, all rights reserved.
    ##
        
#    data = as.list(ID2GO)
#    GOID2key <- key2GO(GOType,mapType,keyword)$GOID2key
#    ID2GOkey = lapply(proteinID, function(x){ unique(GOID2key[[names(data[[x]])]]) } )
#    names(ID2GOkey) = proteinID
    
#    return(ID2GOkey)
#}