.First.lib <- function(lib, pkg) {
if(.Platform$OS.type == "windows" && require(Biobase) && interactive()  && .Platform$GUI == "Rgui") {
	addVigs2WinMenu("annotationTools")
	}

}