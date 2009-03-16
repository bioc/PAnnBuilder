.annBuilderParseTaxLists <- function() {
	dataFile <- "taxLists.txt"
  cols <- c("integer","character")
	taxName <- read.table(dataFile, header=TRUE, sep="\t", colClasses=cols)
	taxLists <- as.list(taxName$taxName)
	names(taxLists) <- taxName$taxID
	taxLists 
}
taxLists <- .annBuilderParseTaxLists()
rm(.annBuilderParseTaxLists)
