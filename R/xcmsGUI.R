xcmsGUI <- function(){
	
	initXCMS()
	#scaleppm <- tkwidget(parent=tt2, type="ttk::scale", label= "Dinkidongdungbam", showvalue=TRUE, from = 0, to = 100, orient = "horizontal", variable = XCMSEnv$ppm)
	tkgrid(ObjectEnv$ppmlabel, ObjectEnv$scaleppm)
	tkgrid(ObjectEnv$ppmentry, row=0, column=2, sticky = "w")
	tkgrid(ObjectEnv$pwslabel, ObjectEnv$scalepwstart)
	tkgrid(ObjectEnv$peakwidthstartentry, row=1, column=2, sticky = "w")
	tkgrid(ObjectEnv$pwelabel, ObjectEnv$scalepwend)
	tkgrid(ObjectEnv$peakwidthendentry, row=2, column=2, sticky = "w")
	tkgrid(ObjectEnv$sntlabel, ObjectEnv$scalesnthresh)
	tkgrid(ObjectEnv$snthreshentry, row=3, column=2, sticky = "w")
	tkgrid(ObjectEnv$pfplabel, ObjectEnv$scalepfpeaks)
	tkgrid(ObjectEnv$pfpeaksentry, row=4, column=2, sticky = "w")
	tkgrid(ObjectEnv$pfilabel, ObjectEnv$scalepfint)
	tkgrid(ObjectEnv$pfintentry, row=5, column=2, sticky = "w")
	tkgrid(ObjectEnv$mzdlabel, ObjectEnv$scalemzdiff)
	tkgrid(ObjectEnv$mzdiffentry, row=6, column=2, sticky = "w")
	tkgrid(ObjectEnv$itglabel, ObjectEnv$integratebutton)
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

