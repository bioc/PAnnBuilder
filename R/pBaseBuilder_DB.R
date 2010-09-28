pBaseBuilder_DB <- function(baseMapType = c("sp","trembl","ipi","refseq"), organism, 
                prefix, pkgPath, version, author
                ) {
  ## Build annotation data packages for proteins in primary protein database.
  ##
  ## baseMapType - a character string that can be either "sp", "trembl", "ipi" 
  ##               or "refseq" to indicate which protein database will be used.
  ##               "sp": UniProtKB/Swiss-Prot
  ##               "trembl": UniProtKB/TrEMBL
  ##               "ipi": International Protein Index (IPI)
  ##                      Homo sapiens, HUMAN
  ##                      Mus musculus, MOUSE
  ##                      Rattus norvegicus, RAT
  ##                      Danio rerio, DANRE
  ##                      Bos taurus, BOVIN
  ##                      Gallus gallus, CHICK
  ##                      Arabidopsis thaliana, ARATH
  ##               "refseq": NCBI Reference Sequence
  ##                      Homo sapiens, H_sapiens  
  ##                      Mus musculus, M_musculus
  ##                      Rattus norvegicus, R_norvegicus
  ##                      Danio rerio, D_rerio
  ##                      Bos taurus, B_taurus
  ##                      Xenopus tropicalis, X_tropicalis
  ##
  ##
  ## organism - a character string for the name of the organism of
  ##            concern. [eg: Homo sapiens]
  ##
  ## prefix - the prefix of the data package to be built (e.g. "hsaSP"). The name
  ##          of builded package is prefix+".db".  
  ##
  ## pkgPath - a character string for the full path of an existing
  ##           directory where the built backage will be stored.
  ##
  ## version - a character string for the version number.
  ##
  ## author - a list with named elements "authors" containing a character
  ##          vector of author names and "maintainer" containing the complete
  ##          character string for the maintainer field, for example, "Jane
  ##          Doe <jdoe@doe.com>".
  ## 
  ## Copyright 2008, Hong Li, all rights reserved.
  ##
  
  if(any(c(missing(baseMapType), missing(prefix), missing(pkgPath),
      missing(organism), missing(version), missing(author),
    is.null(baseMapType), is.null(prefix), is.null(pkgPath),
    is.null(organism), is.null(version), is.null(author)))){
      stop(paste("Parameters baseMapType, prefix, pkgPath, organism, ",
        "version, or author can not be missing or NULL",
        sep = ""))
  }

  type <- match.arg(baseMapType)
  if( !any(type==c("sp","trembl","ipi","refseq")) ){
      stop("Parameter baseMapType is not correct, must be sp,trembl,ipi or refseq")
  }
  if( type=="refseq" & !any(organism==c("Homo sapiens","Mus musculus",
      "Rattus norvegicus","Danio rerio","Bos taurus","Xenopus tropicalis"))){
      stop( paste("Parameter organism \"", "\" is not supported for RefSeq 
            database, must be Homo sapiens, Mus musculus, Rattus norvegicus, 
            Danio rerio, Bos taurus or Xenopus tropicalis." , sep=organism) )
  }
  if( type=="ipi" & !any(organism==c("Homo sapiens","Mus musculus",
      "Rattus norvegicus","Danio rerio","Bos taurus","Gallus gallus", 
      "Arabidopsis thaliana"))){
      stop( paste("Parameter organism \"", "\" is not supported for IPI database, 
            must be Homo sapiens, Mus musculus, Rattus norvegicus, Danio rerio, 
            Bos taurus, Gallus gallus or Arabidopsis thaliana." , sep=organism) )
  }
  
  pkgName = paste(prefix,"db",sep=".")
  srcUrls <- getSrcUrl(type, organism = organism)
  built <- getSrcBuilt(type, organism = organism)
       
  # create annotation package, and write data into "*.sqlite" file     
  createEmptyDPkg(pkgName, pkgPath, force = TRUE, 
      folders = c("man", "R",  paste("inst","extdata",sep=.Platform$file.sep) ))
  drv <- dbDriver("SQLite")
  outputFile <- file.path(pkgPath, pkgName, paste("inst",
      "extdata", sep=.Platform$file.sep), paste(prefix, "sqlite", sep="."))	
  db <- dbConnect(drv, outputFile)

  # write data
  writeData_DB(type, srcUrls, db, organism)  
  repList <- getRepList_DB(organism, type, srcUrls, built, pkgName)
  writeMeta_DB(db, repList)
  writeManAnno_DB(pkgName, pkgPath, version, author, repList)
  countFields <- data.frame(c("IPIAC","PFAM","PATH","INTERPRO"),
                           c("ipi_id","ipi_id","ipi_id","ipi_id"),
                           c("ipiac","pfam","path","interpro"))
  buildMapCounts(db,countFields)
  dbDisconnect(db)  
}
