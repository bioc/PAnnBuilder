\name{#PKGNAME#KEY}
\alias{#PKGNAME#KEY}
\title{A protein key mapping file}
\description{ 
  The annotation package was built using given annotation/sequence packages and this key mapping file:  
  #keyFile#
  
  Three possible method to map Keys in different annotation packages, and produce a keyFile:
	1) From a mapping file provided by user.
	2) Map protein with completely equal sequences.
	3) Map protein using blast.
	
	Package built: #DATE#
  
}\examples{
	xx <- as.list(#PKGNAME#KEY)
	if(length(xx) > 0){
		xx[[1]]
	}
}
\keyword{datasets}

