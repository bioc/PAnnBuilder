## "pubRepo" is from Bioconductor "AnnBuilder" package
## other subClass of "pubRepo" are defined by Hong Li, 2008


## A generic class that reads or downloads data from public data repositories.
## srcUrl -  the url for the cgi script that initiates a query against
##            the databse. The value at the time of coding is:
##            "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?"
##
setClass("pubRepo", representation(srcUrl = "character",
                                   parser = "character",
                                   baseFile = "character",
                                   built = "character",
                                   fromWeb = "logical"))

## Set the get methods
setGeneric("srcUrl", function(object) standardGeneric("srcUrl"))
setMethod("srcUrl", "pubRepo", function(object) object@srcUrl)

setGeneric("builtInfo", function(object) standardGeneric("builtInfo"))
setMethod("builtInfo", "pubRepo", function(object) object@built)

setGeneric("fromWeb", function(object) standardGeneric("fromWeb"))
setMethod("fromWeb", "pubRepo", function(object) object@fromWeb)

setGeneric("baseFile", function(object) standardGeneric("baseFile"))
setMethod("baseFile", "pubRepo", function(object) object@baseFile)

setGeneric("parser", function(object) standardGeneric("parser"))
setMethod("parser", "pubRepo", function(object) object@parser)

# Define the replace methods
setGeneric("srcUrl<-", function(object, value)
            standardGeneric("srcUrl<-"))
setReplaceMethod("srcUrl", "pubRepo", function(object, value){
                  object@srcUrl <- value; object})
                  
setGeneric("baseFile<-", function(object, value) standardGeneric("baseFile<-"))
setReplaceMethod("baseFile", "pubRepo", function(object, value){
                  object@baseFile <- value; object})
                  
setGeneric("fromWeb<-", function(object, value) standardGeneric("fromWeb<-"))
setReplaceMethod("fromWeb", "pubRepo", function(object, value){
                  object@fromWeb <- value; object})
                  
setGeneric("parser<-", function(object, value) standardGeneric("parser<-"))
setReplaceMethod("parser", "pubRepo", function(object, value){
                  object@parser <- value; object})

# Defines functions
setGeneric("readData",
           function(object, ...)
           standardGeneric("readData"))
setMethod("readData", "pubRepo",
          function(object, ...){
          if(fromWeb(object)){
              conn <- url(srcUrl(object))
          }else{
              conn <- file(srcUrl(object))
          }
          temp <- readLines(conn)
          close(conn)
          return(temp)})

setGeneric("downloadData",
           function(object, dist)
           standardGeneric("downloadData"))

setMethod("downloadData", "pubRepo",
          function(object, dist)
          return(loadFromUrl(srcUrl(object), dist)))

setGeneric("parseData", function(object, ...)
           standardGeneric("parseData"))

setMethod("parseData", "pubRepo", 
          function(object, sep = "\t",
          ncol = 2, mergeKey = TRUE){
          if(fromWeb(object)){
              srcData <- downloadData(object, "")
          }else{
              srcData <- srcUrl(object)
          }
          tempOut <- tempfile("tempOut")
          obtained <- matrix(scan(fileMuncher(tempOut, baseFile(object),
                           srcData, parser(object)), what = "character",
                           sep = sep, quote = "", quiet = TRUE,
                           strip.white = TRUE, comment.char = ""),
                           ncol = ncol, byrow = TRUE)
          if(fromWeb(object)){
              unlink(srcData)
          }
          unlink(tempOut)
          if(nrow(obtained) <= 1 || !mergeKey){
              return(obtained)
          }else{
              return(mergeRowByKey(obtained))
          }} )

# getClass("pubRepo")

## sub class of "pubRepo"

## "pBase" class for SwissProt, TREMBL, IPI, NCBI RefSeq data
setClass("pBase", contains = "pubRepo",
         representation(organism = "character"))
                  
setGeneric("organism", function(object) standardGeneric("organism"))
setMethod("organism", "pBase", function(object) object@organism)
setGeneric("organism<-", function(object, value)
           standardGeneric("organism<-"))
setReplaceMethod("organism", "pBase", function(object, value){
           object@organism <- value; object})
setMethod("parseData", "pBase", function(object, sep = "\t", mergeKey = TRUE){
          if(fromWeb(object)){
              srcData <- downloadData(object, "")
          }else{
              srcData <- srcUrl(object)
          }
          tempOut <- tempfile("tempOut")
          obtained <- as.matrix(read.csv(fileMuncher(tempOut,
           srcData, parser(object),organism(object) ), header=T, sep = sep ) )
          if(fromWeb(object)){
              unlink(srcData)
          }
          unlink(tempOut)
          if(nrow(obtained) <= 1 || !mergeKey){
              return(obtained)
          }else{
              return(mergeRowByKey(obtained))
          }} )
          
pBase <- function(srcUrl, parser, built="", fromWeb=TRUE, organism ) {
    new("pBase", srcUrl = srcUrl, parser = parser,
     built = built, fromWeb = fromWeb, organism = organism) 
}


## "GOTermsAnnDbBimap" is from Bioconductor "AnnotationDbi" package.
## setMethod("as.list", "GOTermsAnnDbBimap",
##     function(x, ...)
##     {
##         y <- AnnotationDbi:::flatten(x, fromKeys.only=TRUE)
##         AnnotationDbi:::.toListOfLists(y, mode=1)
##     }
## )
## 
## 
##
##setClass("PtmAnnDbBimap",contains="AnnDbBimap",where)
##setMethod("as.list", "PtmAnnDbBimap",                       
##    function(x, ...)                                            
##    {                                                           
##        y <- AnnotationDbi:::flatten(x, fromKeys.only=TRUE)     
##        AnnotationDbi:::.toListOfLists(y, mode=1)               
##    }                                                           
##)                        
##                                       