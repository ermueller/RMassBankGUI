.onLoad <- function(libname, pkgname) {
	Scrolls()
	WorkflowEnv <<- new.env(parent = .GlobalEnv)
	XCMSEnv <<- new.env(parent = .GlobalEnv)
	SLEnv <<- new.env(parent = .GlobalEnv)
	RmbDefaultSettings()
	oo <- getOption("RMassBank")
	
	oo$xcms <- list(ppm=25, peakwidth=c(20,50), snthresh=10, prefilter=c(3,100), method= "centWave",
				mzCenterFun="wMeanApex3", integrate=1, mzdiff=-0.001, fitgauss=FALSE, 
				scanrange= numeric(), noise=0, sleep=0)
	XCMSEnv$ppm <- tclVar(25)
	XCMSEnv$peakwidthstart <- tclVar(20)
	XCMSEnv$peakwidthend <- tclVar(50)
	XCMSEnv$snthresh <- tclVar(10)
	XCMSEnv$prefilterpeaks <- tclVar(3)
	XCMSEnv$prefilterintensity <- tclVar(100)
	XCMSEnv$method <- tclVar("centWave")
	XCMSEnv$mzCenterFun <- tclVar("wMeanApex3")
	XCMSEnv$integrate <- tclVar(1)
	XCMSEnv$mzdiff <- tclVar(-0.001)
	XCMSEnv$fitGauss <- tclVar("FALSE")
	options("RMassBank" = oo)
	WorkflowGUI()
}