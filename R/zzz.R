.onAttach = function(libname, pkgname) {
	desc <- packageDescription(pkgname)
	DQdate <-  desc$Date
	DQVersion =  desc$Version
	packageStartupMessage("This is ", pkgname, " ", desc$Version,
            " ", desc$Date, "\n")
	return(invisible(NULL))
}



