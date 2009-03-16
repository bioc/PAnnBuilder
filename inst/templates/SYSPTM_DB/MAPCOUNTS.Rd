\name{#PREFIX#MAPCOUNTS} 
\alias{#PREFIX#MAPCOUNTS}
\title{Number of mapped keys for the maps in package #PREFIX#.db}
\description{
  #PREFIX#MAPCOUNTS provides the "map count" (i.e. the count of mapped
  keys) for each map in package #PREFIX#.db.
}
\details{
  This "map count" information is precalculated and stored in the package
  annotation DB. This allows some quality control and is used by the
  \code{\link[AnnotationDbi]{checkMAPCOUNTS}} function defined in
  AnnotationDbi to compare and validate different methods (like
  \code{count.mappedkeys(x)} or \code{sum(!is.na(as.list(x)))})
  for getting the "map count" of a given map.
}
\seealso{
  \code{\link[AnnotationDbi]{mappedkeys}},
  \code{\link[AnnotationDbi]{count.mappedkeys}},
  \code{\link[AnnotationDbi]{checkMAPCOUNTS}}
}
\examples{
  #PREFIX#MAPCOUNTS
  mapnames <- names(#PREFIX#MAPCOUNTS)
  #PREFIX#MAPCOUNTS[mapnames[1]]
  x <- get(mapnames[1])
  sum(!is.na(as.list(x)))
  count.mappedkeys(x)   # much faster!
}

\keyword{datasets}

