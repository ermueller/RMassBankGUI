.onLoad <- function(libname, pkgname) {
  	
	if(!is.tclObj(tclRequire("tablelist"))){
		stop("Tablelist package not installed")
	}
	
}

startRMBGUI <- function() {
	initWorkflowEnv()	##Variables for main Window
	initXCMSEnv()
	initDebugEnv()
	startGUI()
}