\name{processData}
\alias{processData}
\alias{getBaseParsers}
\alias{fileMuncher}
\alias{fileMuncher_DB}
\alias{writeInput}
\alias{writeInputSP}
\alias{writeInputIPI}
\alias{writeInputREFSEQ}
\alias{writeInputBLAST}
\alias{writeInputPFAM}
\alias{writeInputINTERPRO}
\alias{writeOutput}
\alias{.callPerl}
\alias{getSrcObjs}
\alias{getBaseData}
\alias{splitEntry}
\alias{twoStepSplit}
\alias{mergeRowByKey}
\title{Convert Data Format}
\description{
    Convert data format by R function, or produce perl program to process data.
}
\usage{
getBaseParsers(baseMapType, db=FALSE)

fileMuncher(outName, dataFile, parser, organism)
fileMuncher_DB(dataFile, parser, organism)

writeInput(parser, perlName, organism, dataFile)
writeInputSP(perlName,organism)
writeInputIPI(perlName,organism)
writeInputREFSEQ(perlName,organism)
writeInputBLAST(perlName,organism, dataFile)
writeInputPFAM(perlName,organism)
writeInputINTERPRO(perlName,organism)
writeOutput(parser, perlName)
.callPerl(script, os)

getSrcObjs(srcUrls, organism, built, fromWeb = TRUE)
getBaseData(srcObjs)

splitEntry(dataRow, sep = ";", asNumeric = FALSE)
twoStepSplit(dataRow, entrySep = ";", eleSep = "@", asNumeric = FALSE)
mergeRowByKey(mergeMe, keyCol = 1, sep = ";")
}
\arguments{  
  \item{baseMapType}{a character string to indicate which database will be 
    parsed. It can be "sp","trembl","ipi","refseq","equal", "merge","mppi",
    "PeptideAtlas","DBSubLoc","Pfam", "pfamname" or "blast".}
  \item{db}{a boolean to indicate whether the parser file for the SQLite-based 
    package will be returned. }    
  \item{outName}{a character string for the output file name of perl program.}
  \item{dataFile}{a character string for the input file name of perl program.}
  \item{parser}{a character string for the path of the parser file.}
  \item{organism}{a character string for the name of the organism of
    concern. (eg: "Homo sapiens")}
  \item{perlName}{a character string for the name of perl program.}
  \item{script}{a character string for the name of perl program.}
  \item{os}{character string, giving the Operating System (family) of 
   the computer.}
  \item{srcUrls}{character string, giving the url of concerned database.}
  \item{built}{a character string for the release/version information of
    source data.}
  \item{fromWeb}{a boolean to indicate whether the source data will be 
    downloaded from the web or read from a local file}
  \item{srcObjs}{a object of class "pBase". }
  \item{dataRow}{character vector, each element of which is to be split.}
  \item{sep}{a character string containing regular expression(s) to use as 
    "split". }
  \item{asNumeric}{a boolean to indicate whether the elements will be 
    converted to objects of type "numeric".}
  \item{entrySep}{a character string containing regular expression(s) to use 
    in the first "split". }
    \item{eleSep}{a character string containing regular expression(s) to use 
      in the second "split". }
    \item{mergeMe}{a vector or a matrix which duplicating values for the same id
      will be merged}
    \item{keyCol}{a integer indicating the column index to be regarded as key.}
}
\details{
    These functions are from Bioconductor "AnnBuilder" package, but add many 
    new operations depend on the requirements of building proteomic annotation 
    data packages.
    
    \code{\link{getBaseParsers}} return a character string of the name of a 
      parser file based on the given database. Each parser file is a part of 
      perl script and used to parse relevant data.
                    
    \code{\link{fileMuncher}} produce perl file based on given parser and 
      additional input files, then perform this perl program via R. 
    \code{\link{fileMuncher_DB}} produce perl file based on given parser and 
      additional input, then perform this perl program via R. Result data are 
      stored in the relative ouput files. It is designed for the SQLite-based
      annotation package.
    \code{\link{writeInput}} write additional information including input files 
      into the perl script.
    \code{\link{writeOutput}} write information about ouput files into the perl 
      script.
    \code{\link{.callPerl}} perform perl program via R.
    
    \code{\link{getSrcObjs}} given url of database and concerned organism, 
      define objects of class "pBase". pBase is a sub class of "pubRepo", and it 
      is used for SwissProt, TREMBL, IPI and NCBI RefSeq data. 
    \code{\link{getBaseData}} get basic protein annotation data and sequence 
      data from protein database: SwissProt, TREMBL, IPI, NCBI PefSeq.
    
    \code{\link{splitEntry}} split multiple entry for a given mapping.
    \code{\link{twoStepSplit}} split multiple entry with two separaters 
      (e.g. 12345@18;67891@18).
    \code{\link{mergeRowByKey}} merge duplicating values for the same key. 
}
\value{
    \code{\link{getBaseParsers}} returns the path of parser file.
    
    \code{\link{getSrcObjs}} returns a list of defined the objects of class 
      "pBase".
    
    \code{\link{getBaseData}} returns a matrix of protein annotation data.
    
    \code{\link{splitEntry}} returns a vector.
    
    \code{\link{twoStepSplit}} returns a vector.
    
    \code{\link{mergeRowByKey}} returns a data frame containing the merged values.
}
\author{Hong Li}
\references{ 
Zhang, J., Carey, V., Gentleman, R. (2003) An extensible application for 
assembling annotation for genomic data.Bioinformatics 19(1), 155-156. 
}

