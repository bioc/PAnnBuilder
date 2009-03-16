crossBuilder_DB <- function(src = c("sp","ipi","gi"), organism, 
    blast, match, 
    prefix, pkgPath, version, author    
    ) {
  ## Build annotation data packages for protein id mapping
  ##
  ## src - a character vector indicating which protein sequence database will be
  ##       used.
  ##        "sp": SwissProt
  ##        "trembl": Trembl
  ##        "ipi": International Protein Index (IPI)
  ##        "gi": NCBI Reference Sequence  
  ##
  ## blast - a named character vector defining the parameters of blastall.
  ##          p:  Program Name [String]
  ##          e:  Expectation value (E) [Real]
  ##          M:  Matrix [String]
  ##          W:  World Size, default if zero (blastn 11, megablast 28, all    
  ##               others 3) [Integer] default = 0
  ##          G:  Cost to open a gap (-1 invokes default behavior) [Integer]
  ##          E:  Cost to open a gap (-1 invokes default behavior) [Integer]
  ##          U:  Use lower case filtering of FASTA sequence [T/F]  Optional
  ##          F:  Filter query sequence (DUST with blastn, SEG with others) 
  ##               [String]
  ##
  ## match - a named character vector defining the parameters of two sequence 
  ##         matching.
  ##          e:  Expectation value of two sequence matching [Real]
  ##          c:  Coverage of the longest High-scoring Segment Pair (HSP) to the
  ##               whole protein sequence. (range: 0~1)
  ##          i:  Identity of the longest High-scoring Segment Pair (HSP). 
  ##               (range: 0~1)
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
  ## lazyLoad - a boolean indicating whether a lazy load database will be created
  ##  
  ## Copyright 2008, Hong Li, all rights reserved.
  ##  
 
 if(any(c(missing(src), missing(organism), missing(prefix), missing(pkgPath),
      missing(version), missing(author), is.null(organism),
    is.null(src), is.null(prefix), is.null(pkgPath),
    is.null(version), is.null(author)))){
      stop(paste("Parameters src, prefix, pkgPath, ",
        "version, or author can not be missing or NULL",
        sep = ""))
  }

  src <- intersect(src,c("sp","trembl","ipi","gi"))
  if( length(src)==0 ){
      stop("Parameter src is not correct, must be sp, trembl, ipi or gi")
  } 
  
  if(any(c( missing(blast),is.null(blast) ))){
      blast <- c("blastp", "10.0", "BLOSUM62", "0", "-1", "-1", "T", "F")
      names(blast) <- c("p","e","M","W","G","E","U","F")
  }
  if(any(c( missing(match),is.null(match) ))){
      match <- c(0.00001, 0.95, 0.95)
      names(match) <- c("e","c","i")
  } 
  
  pkgName <- paste(prefix,"db",sep=".")
  srcUrls <- sapply(src,function(x){getSrcSQUrl(x,organism)})
  built <- sapply(src,function(x){getSrcSQBuilt(x,organism)})
  names(srcUrls) <- src
  names(built) <- src

  # create annotation package, and write data into "*.sqlite" file     
  createEmptyDPkg(pkgName, pkgPath, force = TRUE, 
      folders = c("man", "R",  paste("inst","extdata",sep=.Platform$file.sep) ))
  drv <- dbDriver("SQLite")
  outputFile <- file.path(pkgPath, pkgName, paste("inst",
      "extdata", sep=.Platform$file.sep), paste(prefix, "sqlite", sep="."))	
  db <- dbConnect(drv, outputFile)
  
  sq <- lapply(src,function(x){  	  
  	  if( x=="sp" ){
  	      y = fasta2list(x,srcUrls[x],organism)
  	  }else{
  	      y = fasta2list(x,srcUrls[x])
  	  }            
      y = unlist(y)    
      tmp = data.frame(names(y),y)
      colnames(tmp) = c("protein_id","seq")
      if( dbExistsTable(db, "seq") ){
        dbWriteTable(db, "seq", tmp, row.names=F, append=T)
      }else{
        dbWriteTable(db, "seq", tmp, row.names=F)
      }
      y
  }) 
  names(sq) <- src
  
  for( i in names(sq) ){
      for( j in setdiff(names(sq),i) ){
          tmp <- data.frame(idBlast(query=sq[[i]], subject=sq[[j]], blast, match))
          colnames(tmp) = c("query","subject")
          if( dbExistsTable(db, "match") ){
            dbWriteTable(db, "match", tmp, row.names=F, append=T)
          }else{
            dbWriteTable(db, "match", tmp, row.names=F)          
          }
      }
  }
  
  parameter = data.frame(c(paste("blast",names(blast),sep="_"),
   paste("match",names(match),sep="_")), c(blast,match))
  dbWriteTable(db, "parameter", parameter, row.names=F)       
  
  repList <- getRepList_DB(organism, "cross", srcUrls, built, pkgName)
  writeMeta_DB(db, repList)
  dbDisconnect(db)
  writeManAnno_DB(pkgName, pkgPath, version, author, repList)
}

fasta2list <- function(type,srcUrl,organism=""){
    tmpFile <- loadFromUrl(srcUrl)
    data <- readLines(tmpFile)
    tag <- grep("^>",data)
    f <- sapply(1:(length(tag)-1), function(x){ 
     paste(data[(tag[x]+1):(tag[x+1]-1)], collapse="") })
    f <- c(f, paste(data[(tag[length(tag)]+1):length(data)], collapse="") )
    names(f) <- data[tag]
    if(type=="sp" & organism!=""){
        f <- f[grep(paste("OS=",organism,sep=""),names(f))]
    }
    switch(toupper(type), 
        "SP" = names(f) <- sub("\\|.*","",sub("^>sp\\|","",names(f))),
        "TREMBL" = names(f) <- sub("\\|.*","",sub("^>IPI:","",names(f))),
        "IPI" = names(f) <- sub("\\|.*","",sub("^>IPI:","",names(f))),
        "GI" = names(f) <- sub("\\|.*","",sub("^>gi\\|","",names(f))),
        stop(paste("Type", type, "is not supported.", "Has to be sp, trembl, ipi 
         or gi"))
    )
    as.list(f)
}


## qurey and subject are two named vectors of sequences
## blast is the parameter for blastall
## match is the parameter for similar sequences
## idBlast return a matrix containing the ID mapping between query and subject 
## using blast.
idBlast <- function(query, subject, blast, match){
    queryFile <- tempfile("blastQuery")
    if( is.null(names(query)) ){
        names(query) <- paste("query",1:length(query),sep="")
    }    
    write(paste(paste(">",names(query),sep=""),query,sep="\n",collapse="\n"),
     queryFile)    
    subjectFile <- tempfile("blastSubject")
    if( is.null(names(subject)) ){
        names(subject) <- paste("subject",1:length(subject),sep="")
    }
    write(paste(paste(">",names(subject),sep=""),subject,sep="\n",collapse="\n")
     ,subjectFile)
    outFile <- tempfile("blastOut")
    system(paste("formatdb -i", subjectFile))     
    system(paste("blastall -p", blast["p"], "-d", subjectFile, "-i", queryFile, 
     "-e",min(as.numeric(blast["e"]),match["e"]), "-M", blast["M"], "-W", 
     blast["W"], "-G", blast["G"], "-E", blast["E"], "-U", blast["U"],"-F", 
     blast["F"],  "-a 5 -m 8 -o", outFile))
    unlink(queryFile)
    unlink(subjectFile)   
    unlink(paste(subjectFile,"psq",sep="."))
    unlink(paste(subjectFile,"pin",sep="."))
    unlink(paste(subjectFile,"phr",sep="."))
    
    tmpFile <- tempfile()
    len1 <- sapply(query,function(x){ length(unlist(strsplit(x,split=""))) } )
    len2 <- sapply(subject,function(x){ length(unlist(strsplit(x,split=""))) } )  
    write.table(len1,paste(outFile,"qLength",sep="."),quote=F,col.names=F,sep="\t")
    write.table(len2,paste(outFile,"sLength",sep="."),quote=F,col.names=F,sep="\t")
    tmpData <- as.matrix(read.csv(fileMuncher(tmpFile, outFile, parser=
     getBaseParsers("blast"), organism=paste(match["i"],match["c"],sep="\t") ), 
     header=T, sep = "\t", colClasses="character" ))
    
    result <- unique(tmpData[,1:2])
    
    unlink(outFile)
    unlink(tmpFile)

    result
}
