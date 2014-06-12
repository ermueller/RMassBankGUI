xcmsGUI <- function(tlwindow){
	
	##Open the window
	tt2 <- tktoplevel(tlwindow)
	tkwm.title(tt2,"Edit xcms Parameters(Only centwave)")
	#scaleppm <- tkwidget(parent=tt2, type="ttk::scale", label= "Dinkidongdungbam", showvalue=TRUE, from = 0, to = 100, orient = "horizontal", variable = XCMSEnv$ppm)
	
	XCMSDUMMY <- new.env()
	XCMSDUMMY$ppm <- tclVar(as.numeric(XCMSEnv$ppm))
	XCMSDUMMY$peakwidthstart <- tclVar(as.numeric(XCMSEnv$peakwidthstart) * 10)
	XCMSDUMMY$peakwidthend <- tclVar(as.numeric(XCMSEnv$peakwidthend) * 10)
	XCMSDUMMY$snthresh <- tclVar(as.numeric(XCMSEnv$snthresh) * 5)
	XCMSDUMMY$prefilterpeaks <- tclVar(as.numeric(XCMSEnv$prefilterpeaks))
	XCMSDUMMY$prefilterintensity <- tclVar(as.numeric(XCMSEnv$prefilterintensity))
	XCMSDUMMY$mzdiff <- tclVar(as.numeric(XCMSEnv$mzdiff) * 1000)
	XCMSDUMMY$fitGauss <- tclVar("FALSE")
	scaleppm <- tkwidget(parent=tt2, type="ttk::scale", from = 0, to = 100, variable = XCMSDUMMY$ppm, orient = "horizontal",
		command=function(x, ...){ 
			tclvalue(XCMSEnv$ppm) <- (floor(as.numeric(...)))
			oo <- getOption("RMassBank")
			oo$xcms$ppm <- (floor(as.numeric(...)))
			options("RMassBank" = oo)
		})
	scalepwstart <- tkwidget(parent=tt2, type="ttk::scale", from = 0, to = 1000, variable = XCMSDUMMY$peakwidthstart, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(XCMSEnv$peakwidthstart) <- (floor(as.numeric(...)) / 10)
			oo <- getOption("RMassBank")
			oo$xcms$peakwidth[1] <- (floor(as.numeric(...)) / 10)
			options("RMassBank" = oo)
		})
	scalepwend <- tkwidget(parent=tt2, type="ttk::scale", from = 0, to = 1000, variable = XCMSDUMMY$peakwidthend, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(XCMSEnv$peakwidthend) <- (floor(as.numeric(...)) / 10)
			oo <- getOption("RMassBank")
			oo$xcms$peakwidth[2] <- (floor(as.numeric(...)) / 10)
			options("RMassBank" = oo)
		})
	scalesnthresh <- tkwidget(parent=tt2, type="ttk::scale", from = 0, to = 100, variable = XCMSDUMMY$snthresh, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(XCMSEnv$snthresh) <- (floor(as.numeric(...)) / 5)
			oo <- getOption("RMassBank")
			oo$xcms$snthresh <- (floor(as.numeric(...)) / 5)
			options("RMassBank" = oo)
		})
	scalepfpeaks <- tkwidget(parent=tt2, type="ttk::scale", from = 0, to = 10, variable = XCMSDUMMY$prefilterpeaks, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(XCMSEnv$prefilterpeaks) <- (floor(as.numeric(...)))
			oo <- getOption("RMassBank")
			oo$xcms$prefilter[1] <- (floor(as.numeric(...)))
			options("RMassBank" = oo)
		})
	scalepfint <- tkwidget(parent=tt2, type="ttk::scale", from = 0, to = 1000, variable = XCMSDUMMY$prefilterintensity, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(XCMSEnv$prefilterintensity) <- (floor(as.numeric(...)))
			oo <- getOption("RMassBank")
			oo$xcms$prefilter[2] <- (floor(as.numeric(...)))
			options("RMassBank" = oo)
		})
	scalemzdiff <- tkwidget(parent=tt2, type="ttk::scale", from = -1000, to = 1000, variable = XCMSDUMMY$mzdiff, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(XCMSEnv$mzdiff) <- (floor(as.numeric(...)) / 1000)
			oo <- getOption("RMassBank")
			oo$xcms$mzdiff <- (floor(as.numeric(...)) / 1000)
			options("RMassBank" = oo)
		})
	
	
	ppmentry <- ttkentry(parent=tt2, textvariable=XCMSEnv$ppm, width=6)
	peakwidthstartentry <- ttkentry(parent=tt2, textvariable=XCMSEnv$peakwidthstart, width=6)
	peakwidthendentry <- ttkentry(parent=tt2, textvariable=XCMSEnv$peakwidthend, width=6)
	snthreshentry <- ttkentry(parent=tt2, textvariable=XCMSEnv$snthresh, width=6)
	pfpeaksentry <- ttkentry(parent=tt2, textvariable=XCMSEnv$prefilterpeaks, width=6)
	pfintentry <- ttkentry(parent=tt2, textvariable=XCMSEnv$prefilterintensity, width=6)
	mzdiffentry <- ttkentry(parent=tt2, textvariable=XCMSEnv$mzdiff, width=6)
	integratebutton <- ttkcheckbutton(parent = tt2, variable = XCMSEnv$integrate, onvalue = 2, offvalue = 1, 
		command=function(x, ...){
			oo <- getOption("RMassBank")
			oo$xcms$integrate <- as.numeric(tclvalue(XCMSEnv$integrate))
			options("RMassBank" = oo)
		})
	
	
	ppmlabel <- ttklabel(parent = tt2, text="Tolerated m/z deviation (ppm)")
	pwslabel <- ttklabel(parent = tt2, text="Minimal peak width (s)")
	pwelabel <- ttklabel(parent = tt2, text="Maximal peak width (s)")
	sntlabel <- ttklabel(parent = tt2, text="Signal to noise ratio cutoff")
	pfplabel <- ttklabel(parent = tt2, text="Minimum number of peaks in mass trace")
	pfilabel <- ttklabel(parent = tt2, text="Minimum intensity of peaks")
	mzdlabel <- ttklabel(parent = tt2, text="Minimum difference in m/z for peaks")
	itglabel <- ttklabel(parent = tt2, text="Use real data for finding peak limits?")
	
	tkgrid(ppmlabel, scaleppm)
	tkgrid(ppmentry, row=0, column=2, sticky = "w")
	tkgrid(pwslabel, scalepwstart)
	tkgrid(peakwidthstartentry, row=1, column=2, sticky = "w")
	tkgrid(pwelabel, scalepwend)
	tkgrid(peakwidthendentry, row=2, column=2, sticky = "w")
	tkgrid(sntlabel, scalesnthresh)
	tkgrid(snthreshentry, row=3, column=2, sticky = "w")
	tkgrid(pfplabel, scalepfpeaks)
	tkgrid(pfpeaksentry, row=4, column=2, sticky = "w")
	tkgrid(pfilabel, scalepfint)
	tkgrid(pfintentry, row=5, column=2, sticky = "w")
	tkgrid(mzdlabel, scalemzdiff)
	tkgrid(mzdiffentry, row=6, column=2, sticky = "w")
	tkgrid(itglabel, integratebutton)
}

RTGUI <- function(tlwindow){
	
	tt3 <- tktoplevel(tlwindow)
	tkwm.title(tt3,"Edit RT Margin/Shift")
	oo <- getOption("RMassBank")
	
	RTEnv <- new.env()
	RTEnv$margin <- tclVar(0)
	RTEnv$shift <- tclVar(0)
	
	RTDUMMYEnv <- new.env()
	RTDUMMYEnv$margin <- tclVar(as.numeric(oo$rtMargin)*10)
	RTDUMMYEnv$shift <- tclVar(as.numeric(oo$rtShift)*10)
	
	scaleRTM <- tkwidget(parent=tt3, type="ttk::scale", from = 0, to = 10, variable = RTDUMMYEnv$margin, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(RTEnv$margin) <- (floor(as.numeric(...)) / 10)
			oo <- getOption("RMassBank")
			oo$rtMargin <- (floor(as.numeric(...)) / 10)
			options("RMassBank" = oo)
		})
	scaleRTS <- tkwidget(parent=tt3, type="ttk::scale", from = -100, to = 100, variable = RTDUMMYEnv$shift, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(RTEnv$shift) <- (floor(as.numeric(...)) / 10)
			oo <- getOption("RMassBank")
			oo$rtShift <- (floor(as.numeric(...)) / 10)
			options("RMassBank" = oo)
		})
	
	marentry <- ttkentry(parent=tt3, textvariable=RTEnv$margin, width=6)
	shientry <- ttkentry(parent=tt3, textvariable=RTEnv$shift, width=6)
	
	marlabel <- ttklabel(parent = tt3, text="Deviation allowed for retention time (m)")
	shilabel <- ttklabel(parent = tt3, text="Systematic retention time shift (m)")
	
	tkgrid(marlabel, scaleRTM)
	tkgrid(marentry, row=0, column=2, sticky = "w")
	tkgrid(shilabel, scaleRTS)
	tkgrid(shientry, row=1, column=2, sticky = "w")
}

SLGUI <- function(tlwindow){
	
	tt4 <- tktoplevel(tlwindow)
	tkwm.title(tt4, "Edit spectra list")
	
	ybar2 <- ttkscrollbar(tt4, command=function(...) pseudoSynchScroll(list(lboxMode,lboxCE,lboxCES,lboxRES),...))
	lboxMode <- tklistbox(tt4, width = 5, borderwidth = 0 , height = 10 , selectmode = "multiple",  
						yscrollcommand = function(...)pseudoSetScroll(ybar2, ...), 
						background = "white")
	lboxCE <- tklistbox(tt4, width = 5, borderwidth = 0 , height = 10 , selectmode = "multiple",  
						yscrollcommand = function(...)pseudoSetScroll(ybar2, ...), 
						background = "white")
	lboxCES <- tklistbox(tt4, width = 20, borderwidth = 0 , height = 10 , selectmode = "multiple",  
						yscrollcommand = function(...)pseudoSetScroll(ybar2, ...), 
						background = "white")
	lboxRES <- tklistbox(tt4, width = 7, borderwidth = 0 , height = 10 , selectmode = "multiple",  
						yscrollcommand = function(...)pseudoSetScroll(ybar2, ...), 
						background = "white")
	oo <- getOption("RMassBank")
	SL <- oo$spectraList
	
	SLtostring <- function(x){
		lapply(x, function(y){
			sapply(y, function(z) return(z))
			})
		}
	
	SLstring <- SLtostring(SL)
	
	for(i in 1:length(SLstring)){
		tkinsert(lboxMode, "end", SLstring[[i]][1])
		tkinsert(lboxCE, "end", SLstring[[i]][2])
		tkinsert(lboxCES, "end", SLstring[[i]][3])
		tkinsert(lboxRES, "end", SLstring[[i]][4])
	}
	
	tkbind(lboxMode, "<Delete>", function(...) {
		index <- tclvalue(tkcurselection(lboxMode))
		if(nchar(index) > 1){
			index <- unlist(strsplit(index, " "))
		}
		index <- as.numeric(index)
		if(!is.na(index)){
			sapply(rev(index), function(x){tkdelete(lboxMode,x)})
			sapply(rev(index), function(x){tkdelete(lboxCE,x)})
			sapply(rev(index), function(x){tkdelete(lboxCES,x)})
			sapply(rev(index), function(x){tkdelete(lboxRES,x)})
			oo <- getOption("RMassBank")
			print(index)
			for(j in rev(index)){
				SL[[j+1]] <<- NULL
				oo <- getOption("RMassBank")
				oo$spectraList[[j+1]] <- NULL
				options("RMassBank" = oo)
			}
		}
	})
	
	
	tkbind(lboxCE, "<Delete>", function(...) {
			index <- tclvalue(tkcurselection(lboxCE))
			if(nchar(index) > 1){
				index <- unlist(strsplit(index, " "))
			}
			index <- as.numeric(index)
			if(!is.na(index)){
				sapply(rev(index), function(x){tkdelete(lboxMode,x)})
				sapply(rev(index), function(x){tkdelete(lboxCE,x)})
				sapply(rev(index), function(x){tkdelete(lboxCES,x)})
				sapply(rev(index), function(x){tkdelete(lboxRES,x)})
				oo <- getOption("RMassBank")
				print(index)
				for(j in rev(index)){
					SL[[j+1]] <<- NULL
					oo <- getOption("RMassBank")
					oo$spectraList[[j+1]] <- NULL
					options("RMassBank" = oo)
				}
			}
		})
	
	tkbind(lboxCES, "<Delete>", function(...) {
			index <- tclvalue(tkcurselection(lboxCES))
			if(nchar(index) > 1){
				index <- unlist(strsplit(index, " "))
			}
			index <- as.numeric(index)
			if(!is.na(index)){
				sapply(rev(index), function(x){tkdelete(lboxMode,x)})
				sapply(rev(index), function(x){tkdelete(lboxCE,x)})
				sapply(rev(index), function(x){tkdelete(lboxCES,x)})
				sapply(rev(index), function(x){tkdelete(lboxRES,x)})
				oo <- getOption("RMassBank")
				print(index)
				for(j in rev(index)){
					SL[[j+1]] <<- NULL
					oo <- getOption("RMassBank")
					oo$spectraList[[j+1]] <- NULL
					options("RMassBank" = oo)
				}
			}
		})
	
	tkbind(lboxRES, "<Delete>", function(...) {
			index <- tclvalue(tkcurselection(lboxRES))
			if(nchar(index) > 1){
				index <- unlist(strsplit(index, " "))
			}
			index <- as.numeric(index)
			if(!is.na(index)){
				sapply(rev(index), function(x){tkdelete(lboxMode,x)})
				sapply(rev(index), function(x){tkdelete(lboxCE,x)})
				sapply(rev(index), function(x){tkdelete(lboxCES,x)})
				sapply(rev(index), function(x){tkdelete(lboxRES,x)})
				oo <- getOption("RMassBank")
				print(index)
				for(j in rev(index)){
					SL[[j+1]] <<- NULL
					oo <- getOption("RMassBank")
					oo$spectraList[[j+1]] <- NULL
					options("RMassBank" = oo)
				}
			}
		})
	
	addSpectrum <- function(){
		oo <- getOption("RMassBank")
		oo$spectraList[[length(oo$spectraList)+1]] <- list(mode=tclvalue(SMODE) ,ce=tclvalue(CE) ,ces=tclvalue(CES), res=tclvalue(RESOL))
		options("RMassBank" = oo)
		tkinsert(lboxMode, "end", tclvalue(SMODE))
		tkinsert(lboxCE, "end", tclvalue(CE))
		tkinsert(lboxCES, "end", tclvalue(CES))
		tkinsert(lboxRES, "end", tclvalue(RESOL))
		SMODE <- tclVar("")
		CE <- tclVar("")
		CES <- tclVar("")
		RESOL <- tclVar("")
	}
	
	addbutton <- ttkbutton(parent=tt4, text = "Add new spectrum", command = addSpectrum)
	
	SMODE <- tclVar("")
	CE <- tclVar("")
	CES <- tclVar("")
	RESOL <- tclVar("")
	
	SMODEentry <- ttkentry(parent=tt4, textvariable=SMODE, width=5)
	CEentry <- ttkentry(parent=tt4, textvariable=CE, width=5)
	CESentry <- ttkentry(parent=tt4, textvariable=CES, width=20)
	RESentry <- ttkentry(parent=tt4, textvariable=RESOL, width=7)
	
	mlabel <- ttklabel(parent = tt4, text = "Mode")
	CElabel <- ttklabel(parent = tt4, text = "CE")
	CESlabel <- ttklabel(parent = tt4, text = "CES")
	RESlabel <- ttklabel(parent = tt4, text = "Res")
	
	tkgrid(mlabel, CElabel, CESlabel, RESlabel, padx=c(0,0))
	tkgrid(lboxMode, lboxCE, lboxCES, lboxRES, padx=c(0,0), pady = c(10,0))
	tkgrid(ybar2,row=1, column=4, sticky="nsw", padx=c(0,10))
	
	m <- ttkseparator(parent=tt4, orient="horizontal")
	tkgrid(m)
	tkgrid(SMODEentry, CEentry, CESentry, RESentry, padx=c(0,0))
	tkgrid(addbutton, row=4, column = 1, columnspan=2)
}

addSpecGUI <- function(tlwind){
	tt5 <- tktoplevel(tlwind)
	tkwm.title(tt5, "Add new spectrum")
	
	
	
	mlabel <- ttklabel(parent = tt5, text="Mode")
	CElabel <- ttklabel(parent = tt5, text="CE")
	CESlabel <- ttklabel(parent = tt5, text="CES")
	RESlabel <- ttklabel(parent = tt5, text="Res")
	
	addb <- ttkbutton(parent=tt5, text="Add spectrum", command = function(...){
		
		tkdestroy(tt5)
	})
	
	tkgrid(mlabel, CElabel, CESlabel, RESlabel, padx=c(0,0))
	
	tkgrid(addb, row=2, column = 1, columnspan=2)
}