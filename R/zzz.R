.First.lib <- function(libname, pkgname) {
	#data(IdMappingAnnotationData.root);
}

.onAttach = function(libname, pkgname) {
	desc <- packageDescription(pkgname)
	DQdate <-  desc$Date
	DQVersion =  desc$Version
	cat("This is ", pkgname, " ", desc$Version, " ", desc$Date, "\n")
	return(invisible(NULL))
}



