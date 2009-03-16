pSeqBuilder_DB <- function(query, annPkgs, seqName, blast, match,
               prefix, pkgPath, version, author
               ) {
  ## Annotate proteins based on protein sequence similarity, and Build 
  ## annotation data packages.
  ##
  ## query - a named string vector to be used as query sequences. Blast 
  ##           will be called to map between query sequences and sequences 
  ##           from the given protein sequence package, and then get   
  ##           corresponding annotation data from the given annotation package.
  ##
  ## annPkgs - a string vector containing the name of annotation packages. 
  ##           In annotation package, data is saved as hash table using R 
  ##           environment object. The Key of R environment object is protein, 
  ##           and the value is its annotation.
  ##
  ## blast - a named character vector defining the parameters of blastall.
  ##         p:  Program Name [String]
  ##         e:  Expectation value (E) [Real]
  ##         M:  Matrix [String]
  ##         W:  World Size, default if zero (blastn 11, megablast 28, all 
  ##              others 3) [Integer] default = 0
  ##         G:  Cost to open a gap (-1 invokes default behavior) [Integer]
  ##         E:  Cost to open a gap (-1 invokes default behavior) [Integer]
  ##         U:  Use lower case filtering of FASTA sequence [T/F]  Optional
  ##         F:  Filter query sequence (DUST with blastn, SEG with others) 
  ##              [String]
  ##
  ## match - a named character vector defining the parameters of two sequence 
  ##         matching.
  ##         e:  Expectation value of two sequence matching [Real]
  ##         c:  Coverage of the longest High-scoring Segment Pair (HSP) to the 
  ##              whole protein sequence. (range: 0~1)
  ##         i:  Identity of the longest High-scoring Segment Pair (HSP). 
  ##              (range: 0~1)
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
    
  if(any(c(missing(query), missing(annPkgs), missing(seqName), missing(prefix), 
    missing(pkgPath), missing(version), missing(author), is.null(query),
    is.null(annPkgs), is.null(seqName), is.null(prefix), is.null(pkgPath),
    is.null(version), is.null(author)))){
      stop(paste("Parameters query, annPkgs, seqName, prefix, pkgPath, ",
        "version, or author can not be missing or NULL",
        sep = ""))
  }
    
  for( i in 1:length(annPkgs)){  
      require(annPkgs[i],character.only=TRUE) || 
        stop(paste("Package", annPkgs[i], "is unavailable!"))      
  }
  
 if(any(c( missing(blast),is.null(blast) ))){
      blast <- c("blastp", "10.0", "BLOSUM62", "0", "-1", "-1", "T", "F")
      names(blast) <- c("p","e","M","W","G","E","U","F")
  }
  if(any(c( missing(match),is.null(match) ))){
      match <- c(0.00001, 0.95, 0.95)
      names(match) <- c("e","c","i")
  } 
  
  map <- list()
  for( i in seqName){
      tmp1=AnnotationDbi:::as.list(base:::get(i))
      tmp <- idBlast(query=query, subject=unlist(tmp1), blast, match)      
      if( nrow(tmp)>0 ){
      	map[[i]] <- as.list(tapply(1:nrow(tmp),tmp[,1],function(x){tmp[x,2]}))
      }else{
      	map[[i]] <- NA
      }
  }
 
  tmpList = rep(NA,length(query) ) 
  names(tmpList) = names(query)
  anno <- list()  
  for( i in 1:length(annPkgs)){
      tmp <- map[[seqName[i]]]
      if( length(tmp)>0 ){
           print(annPkgs[i])
     print( seqName[i])
          indexs <- index_DB(annPkgs[i], seqName[i])
          for(j in indexs){            
              data <- AnnotationDbi:::as.list(base:::get(j))                    
              name <- sub(annPkgs[i],"",j)
              if( is.null(anno[[name]]) ){             
              	anno[[name]] <- as.list(tmpList)              	
              }
              for(x in names(tmp)){                
                   tmp1 <- unlist(data[tmp[[x]]])  
                   if( !is.null(tmp1) ){
                     anno[[name]][[x]] <- c( anno[[name]][[x]], tmp1)                  
                   }
              }
          }
      }
  } 
  
  pkgName = paste(prefix,"db",sep=".")
  # create annotation package, and write data into "*.sqlite" file     
  createEmptyDPkg(pkgName, pkgPath, force = TRUE, 
      folders = c("man", "R",  paste("inst","extdata",sep=.Platform$file.sep) ))
  drv <- dbDriver("SQLite")
  outputFile <- file.path(pkgPath, pkgName, paste("inst",
      "extdata", sep=.Platform$file.sep), paste(prefix, "sqlite", sep="."))	
  db <- dbConnect(drv, outputFile)
  
  sapply(names(anno),function(x){ 
      print(x)
      y = lapply(anno[[x]], function(z){ 
          if(sum(!is.na(z))>0){unique(z[!is.na(z)])} } ) ;    
      if( !is.null(unlist(y)) ){      
        tmp = data.frame(rep(names(y),sapply(y,length)), unlist(y) )  
        dbWriteTable(db, gsub("\\.","",x), tmp, row.names=F) 
      }
  } )
  
  repList <- getRepList_DB(organism="", type="pSeq", srcUrls=annPkgs,built="",
   pkgName)
  writeMeta_DB(db, repList)
  dbDisconnect(db)
  writeManAnno_DB(pkgName, pkgPath, version, author, repList)
}

index_DB<-function(pkgName,seqName){
  require(pkgName, character.only = TRUE) || 
    stop(paste("Package", pkgName, "is unavailable!"))     
  tmpIndex <- ls(paste("package",pkgName,sep=":"))
  if( !any(tmpIndex==seqName) ){
    stop(paste("Object",seqName,"is unavailable in package",pkgName))
  }
  tmp <- sub(".db$","",pkgName)
  tmpIndex <- setdiff(tmpIndex,c(pkgName,seqName,paste(tmp,"dbconn",sep="_"),
   paste(tmp,"dbschema",sep="_"), paste(tmp,"dbfile",sep="_"),
   paste(tmp,"dbInfo",sep="_"), paste(tmp,"MAPCOUNTS",sep="_") ))
  return(tmpIndex)
}

