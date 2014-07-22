initXCMS <- function(){
	initXCMSDummyEnv()		##Variables for dummy values for the scales
	initXCMSWindow()		##Initialize the XCMS window
	initXCMSLabels()		##Initialize labels for XCMS window
	initXCMSEntries()		##Initialize entries for XCMS window
	initXCMSScales()		##Initialize scales for XCMS window
	initXCMSCheckbuttons()	##Initialize checkbuttons for XCMS window
}

initXCMSDummyEnv <- function(){
	##Environment for the dummies (necessary because ttktoolbar is garbage)
	XCMSDUMMY$ppm <- tclVar(as.numeric(tclvalue(XCMSEnv$ppm)))
	XCMSDUMMY$peakwidthstart <- tclVar(as.numeric(tclvalue(XCMSEnv$peakwidthstart)) * 10)
	XCMSDUMMY$peakwidthend <- tclVar(as.numeric(tclvalue(XCMSEnv$peakwidthend)) * 10)
	XCMSDUMMY$snthresh <- tclVar(as.numeric(tclvalue(XCMSEnv$snthresh)) * 5)
	XCMSDUMMY$prefilterpeaks <- tclVar(as.numeric(tclvalue(XCMSEnv$prefilterpeaks)))
	XCMSDUMMY$prefilterintensity <- tclVar(as.numeric(tclvalue(XCMSEnv$prefilterintensity)))
	XCMSDUMMY$mzdiff <- tclVar(as.numeric(tclvalue(XCMSEnv$mzdiff)) * 1000)
	XCMSDUMMY$fitGauss <- tclVar(as.logical(tclvalue(XCMSEnv$fitGauss)))
}

initXCMSWindow <- function(){
	##Open the window
	ObjectEnv$tt2 <- tktoplevel(ObjectEnv$tt)
	tkwm.title(ObjectEnv$tt2,"Edit xcms Parameters(Only centwave)")
}

initXCMSLabels <- function(){
	##Labels for XCMS GUI
	ObjectEnv$ppmlabel <- ttklabel(parent = ObjectEnv$tt2, text="Tolerated m/z deviation (ppm)")
	ObjectEnv$pwslabel <- ttklabel(parent = ObjectEnv$tt2, text="Minimal peak width (s)")
	ObjectEnv$pwelabel <- ttklabel(parent = ObjectEnv$tt2, text="Maximal peak width (s)")
	ObjectEnv$sntlabel <- ttklabel(parent = ObjectEnv$tt2, text="Signal to noise ratio cutoff")
	ObjectEnv$pfplabel <- ttklabel(parent = ObjectEnv$tt2, text="Minimum number of peaks in mass trace")
	ObjectEnv$pfilabel <- ttklabel(parent = ObjectEnv$tt2, text="Minimum intensity of peaks")
	ObjectEnv$mzdlabel <- ttklabel(parent = ObjectEnv$tt2, text="Minimum difference in m/z for peaks")
	ObjectEnv$itglabel <- ttklabel(parent = ObjectEnv$tt2, text="Use real data for finding peak limits?")
}

initXCMSEntries <- function(){
	##Entries for XCMS GUI
	ObjectEnv$ppmentry <- ttkentry(parent=ObjectEnv$tt2, textvariable=XCMSEnv$ppm, width=6)
	ObjectEnv$peakwidthstartentry <- ttkentry(parent=ObjectEnv$tt2, textvariable=XCMSEnv$peakwidthstart, width=6)
	ObjectEnv$peakwidthendentry <- ttkentry(parent=ObjectEnv$tt2, textvariable=XCMSEnv$peakwidthend, width=6)
	ObjectEnv$snthreshentry <- ttkentry(parent=ObjectEnv$tt2, textvariable=XCMSEnv$snthresh, width=6)
	ObjectEnv$pfpeaksentry <- ttkentry(parent=ObjectEnv$tt2, textvariable=XCMSEnv$prefilterpeaks, width=6)
	ObjectEnv$pfintentry <- ttkentry(parent=ObjectEnv$tt2, textvariable=XCMSEnv$prefilterintensity, width=6)
	ObjectEnv$mzdiffentry <- ttkentry(parent=ObjectEnv$tt2, textvariable=XCMSEnv$mzdiff, width=6)
}

initXCMSScales <- function(){
	ObjectEnv$scaleppm <- tkwidget(parent=ObjectEnv$tt2, type="ttk::scale", from = 0, to = 100, variable = XCMSDUMMY$ppm, orient = "horizontal",
		command=function(x, ...){ 
			tclvalue(XCMSEnv$ppm) <- (floor(as.numeric(...)))
			updateXCMSoptions()
		})

	ObjectEnv$scalepwstart <- tkwidget(parent=ObjectEnv$tt2, type="ttk::scale", from = 0, to = 1000, variable = XCMSDUMMY$peakwidthstart, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(XCMSEnv$peakwidthstart) <- (floor(as.numeric(...)) / 10)
			updateXCMSoptions()
		})

	ObjectEnv$scalepwend <- tkwidget(parent=ObjectEnv$tt2, type="ttk::scale", from = 0, to = 1000, variable = XCMSDUMMY$peakwidthend, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(XCMSEnv$peakwidthend) <- (floor(as.numeric(...)) / 10)
			updateXCMSoptions()
		})

	ObjectEnv$scalesnthresh <- tkwidget(parent=ObjectEnv$tt2, type="ttk::scale", from = 0, to = 100, variable = XCMSDUMMY$snthresh, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(XCMSEnv$snthresh) <- (floor(as.numeric(...)) / 5)
			updateXCMSoptions()
		})

	ObjectEnv$scalepfpeaks <- tkwidget(parent=ObjectEnv$tt2, type="ttk::scale", from = 0, to = 10, variable = XCMSDUMMY$prefilterpeaks, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(XCMSEnv$prefilterpeaks) <- (floor(as.numeric(...)))
			updateXCMSoptions()
		})

	ObjectEnv$scalepfint <- tkwidget(parent=ObjectEnv$tt2, type="ttk::scale", from = 0, to = 1000, variable = XCMSDUMMY$prefilterintensity, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(XCMSEnv$prefilterintensity) <- (floor(as.numeric(...)))
			updateXCMSoptions()
		})

	ObjectEnv$scalemzdiff <- tkwidget(parent=ObjectEnv$tt2, type="ttk::scale", from = -1000, to = 1000, variable = XCMSDUMMY$mzdiff, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(XCMSEnv$mzdiff) <- (floor(as.numeric(...)) / 1000)
			updateXCMSoptions()
		})
}

initXCMSCheckbuttons <- function(){
	##XCMS GUI Checkbutton
	ObjectEnv$integratebutton <- ttkcheckbutton(parent = ObjectEnv$tt2, variable = XCMSEnv$integrate, onvalue = 2, offvalue = 1, 
	command=function(x, ...){
		oo <- getOption("RMassBank")
		oo$xcms$integrate <- as.numeric(tclvalue(XCMSEnv$integrate))
		options("RMassBank" = oo)
	})
}
