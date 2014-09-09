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
	
	if(!is.null(SL)){
		SLstring <- SLtostring(SL)
	
		for(i in 1:length(SLstring)){
			tkinsert(lboxMode, "end", SLstring[[i]][1])
			tkinsert(lboxCE, "end", SLstring[[i]][2])
			tkinsert(lboxCES, "end", SLstring[[i]][3])
			tkinsert(lboxRES, "end", SLstring[[i]][4])
		}
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
	
	addbutton <- ttkbutton(parent=tt4, text = "Add spectrum", command = addSpectrum)
	
	savebut <- ttkbutton(tt4, text = "Apply", command = function(){
		tkdestroy(tt4)
	})
	
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
	tkgrid(m, columnspan=4, pady=c(5,5), sticky = "ew")
	tkgrid(SMODEentry, CEentry, CESentry, RESentry, padx=c(0,0))
	
	tkgrid(savebut, row=4, column = 3, columnspan=2)
	tkgrid(addbutton, row=4, column = 1, columnspan=2)
	Sys.sleep(0.1)
	.Tcl(paste("wm resizable", .Tk.ID(tt4), 0, 0))
	
}