WorkflowGUI <- function(){
	
	##Initialize Variables
	guifiles <- list()
	cpdID <- list()
	
	##Dummy variables
	dummyradio <- tclVar("NA")
	
	##SETTINGS GETS ITS OWN MENU
	settings <- tclVar("")
	
	##COMPOUNDLIST, TOO?
	compoundList <- tclVar("")
	
	
	rMethod <- tclVar("mzR")
	mzmode <- tclVar("pH")
   
	##Open the window
	tt <- tktoplevel(height=400, width = 900)
	tkwm.title(tt,"Interactive GUI for using the RMassBank Workflow")
	tkgrid.propagate(tt,1)

	
	##Use the icon
	##tkwm.iconbitmap(tt, "something.ico")
	
	settings.entry <- ttkentry(tt, textvariable = settings)
	
	###??????
	compoundList.entry <- ttkentry(tt, textvariable = compoundList)
	
	##List box for files
	xbar <- ttkscrollbar(tt, orient = "horizontal", command = function(...)tkxview(lboxfiles,...))
	
	##Synched scrollbar
	ybar <- ttkscrollbar(tt, command = function(...)pseudoSynchScroll(list(lboxfiles,lboxcpdID),...))
	
	
	##Listbox definition
	lboxfiles <- tklistbox(tt, width = 60, borderwidth = 0 , height = 4 , selectmode = "multiple", 
						xscrollcommand = function(...)tkset(xbar, ...),
						yscrollcommand = function(...)pseudoSetScroll(ybar, ...), 
						background = "white")
	lboxcpdID <- tklistbox(tt, width = 20, borderwidth = 0 , height = 4 , selectmode = "multiple", 
						yscrollcommand = function(...)pseudoSetScroll(ybar, ...), 
						background = "white")
	
	##Bind the del key to the listboxes
	tkbind(lboxfiles, "<Delete>", function(...) {
		index <- tclvalue(tkcurselection(lboxfiles))
		if(nchar(index) > 1){
			index <- unlist(strsplit(index, " "))
		}
		index <- as.numeric(index)
		if(!is.na(index)){
			sapply(rev(index), function(x){tkdelete(lboxfiles,x)})
			sapply(rev(index), function(x){tkdelete(lboxcpdID,x)})
			for(i in rev(index)){
				guifiles[[i+1]] <<- NULL
				cpdID[[i+1]] <<- NULL
			}
		}
	})
	
	tkbind(lboxcpdID, "<Delete>", function(...) {
		index <- tclvalue(tkcurselection(lboxcpdID))
		if(nchar(index) > 1){
			index <- unlist(strsplit(index, " "))
		}
		index <- as.numeric(index)
		if(!is.na(index)){
			sapply(rev(index), function(x){tkdelete(lboxfiles,x)})
			sapply(rev(index), function(x){tkdelete(lboxcpdID,x)})
			for(i in rev(index)){
				guifiles[[i+1]] <<- NULL
				cpdID[[i+1]] <<- NULL
			}
		}
	})
	
	##List box for compound IDs
	ybarcpdID <- ttkscrollbar(tt, command = function(...)tkyview(lboxcpdID,...))
	
	cBoxmethod <- ttkcombobox(tt, state = "readonly", textvariable = rMethod, values = c("mzR", "xcms", "peaklist"))
	cBoxmode <- ttkcombobox(tt, state = "readonly", textvariable = mzmode, values = c("pH", "mH"))
	
	choosefiles <- function(){
		F <- tk_choose.files()
		offset <- length(guifiles)
		if(length(F) > 0){
			for(i in 1:length(F)){
				guifiles[[offset + i]] <<- F[i]
				tkinsert(lboxfiles,"end",basename(F[i]))
			}
		}
	}
	
	editcpdID <- function(){
		newtop <- tktoplevel()
		twindow <- tktext(newtop, width = 10, height = 7)
		tkgrid(twindow)
		tkmark.set(twindow,"insert","0.0")
		tkfocus(twindow)
	}
	
	chooseSettings <- function(){
		change <- TRUE
		o <- getOption("RMassBank", NULL)
		if(!is.null(o)){
			change <- changeSettings()
		}
		if(change){
			SET <- tk_choose.files(multi = FALSE)
			if(length(SET) > 0){
				tclvalue(settings) <- SET
				p <- getOption("RMassBank")$xcms
				loadRmbSettings(SET)
				pp <- getOption("RMassBank")
				pp$xcms <- p
				options("RMassBank"=pp)
				if(is.na(getOption("RMassBank")$deprofile)){
					dummyradio <- tclVar("NA")
				} else { dummyradio <- tclVar(getOption("RMassBank")$deprofile) }
			}
		}
	}
	
	choosecompoundList <- function(){
		CLI <- tk_choose.files(multi = FALSE)
		if(length(CLI) > 0){
			tclvalue(compoundList) <- CLI
		}
	}
	
	example <- function(){
		if("RMassBankData" %in% rownames(installed.packages())){
			.Tcl(paste(.Tk.ID(cBoxmethod) ,"set mzR"))
			.Tcl(paste(.Tk.ID(cBoxmode) ,"set pH"))
			
			##Compound List
			tclvalue(compoundList) <- system.file("list/NarcoticsDataset.csv", package = "RMassBankData", 
								lib.loc = NULL, mustWork = TRUE)
			
			##Infolist
			iList <- system.file("infolists/NarcoticsDataset.csv", package = "RMassBankData", 
								lib.loc = NULL, mustWork = TRUE) 
			
			##Files
			j <- 1
			for(i in list.files(system.file("spectra", package = "RMassBankData", 
								lib.loc = NULL, mustWork = TRUE),pattern=".mzML", full.names=TRUE)[1:2]){
				guifiles[[j]] <<- i
				j <- j+1
				tkinsert(lboxfiles,"end",basename(i))
			}
			
			##cpdIDs
			for(i in 2818:2819){
				cpdID[[i-2817]] <<- i
				tkinsert(lboxcpdID,"end",i)
			}
			
			##settings
			RmbSettingsTemplate("mysettings.ini")
			
			tclvalue(settings) <- "mysettings.ini"

		} else {
			tkmessageBox(message = "You need to install RMassBankData to show the example!")
		}
	}
	
	submit <- function(){
		
		m <- matrix("",length(guifiles),2)
		colnames(m) <- c("Files", "ID")
		for(i in 1:length(guifiles)){
			m[i,1] <- guifiles[[i]]
			m[i,2] <- cpdID[[i]]
		}
		
		write.csv(m, file="ftable.csv")
		fileConn <- file("logfile.txt", open="w")
		writeLines(as.yaml(getOption("RMassBank")), fileConn)
		close(fileConn)
		w <- newMsmsWorkspace()
		ROfiles <- vector()
		ROcpdid <- vector()
		oo <- getOption("RMassBank")$xcms
		for(i in 1:length(guifiles)){
			ROfiles[i] <- guifiles[[i]]
			ROcpdid[i] <- as.numeric(cpdID[[i]])
		}
		
		Args <- list(method = oo$method, ppm = oo$ppm, snthresh = oo$snthresh,
                  peakwidth = oo$peakwidth, integrate = oo$integrate, mzdiff = oo$mzdiff, mzCenterFun = oo$mzCenterFun)
		loadRmbSettings(tclvalue(settings))
		loadList(tclvalue(compoundList))
		w <- msmsRead(w,files = ROfiles, cpdids = ROcpdid, readMethod = tclvalue(rMethod), mode = tclvalue(mzmode), 
					confirmMode = FALSE, useRtLimit = TRUE, Args = Args, settings = getOption("RMassBank"), 
					progressbar = "progressBarHook", MSe = FALSE)
		w <- msmsWorkflow(w, steps=2:8)
		w2 <- newMbWorkspace(w)
		w2 <- mbWorkflow(w2, steps=1:2)
		w2 <- loadInfolist(w2, "infolist.csv")
		w2 <- mbWorkflow(w2, steps=3:8)
	}
	
	choosefiles.but <- ttkbutton(tt, text = "Choose mz-files", command = choosefiles, width=25)
	editcpdID.but <- ttkbutton(tt, text = "Edit compound IDs", command = editcpdID, width=25)
	chooseSettings.but <- ttkbutton(tt, text = "Choose the file settings", command = chooseSettings, width=25)
	choosecompoundList.but <- ttkbutton(tt, text = "Choose the compound list", command = choosecompoundList, width=25)
	example.but <- ttkbutton(tt, text = "Example Workflow", command = example)
	submit.but <- ttkbutton(tt, text = "Submit", command = submit)
	
	
	##Labels
	Filestext <- ttklabel(tt,text="Files:")
	Settingstext <- ttklabel(tt,text="Settings:")
	Compoundlisttext <- ttklabel(tt,text="Compound list:")
	
	##IMAGE
	fpath <- system.file("cosmetics/RMassBank_logo.gif", package="RMassBankGUI")
	iconName <- "::tcl::logo"
	i1 <- tkimage.create("photo", iconName, file=fpath)
	a <- ttklabel(tt,image=iconName)
	
	##1st row
	tkgrid(Filestext, lboxfiles,  pady = c(10,0))
	tkgrid(lboxcpdID, pady = c(10,0), row = 0, column = 3, sticky = "sew")
	tkgrid(ybar, pady = c(10,0), row = 0, column = 4, sticky = "w", padx = c(0,10))
	tkgrid(choosefiles.but, row = 0, column = 5, padx = 5, pady = c(10,0), sticky = "w")
	tkgrid(a, row = 0, column = 5)
	tkgrid.configure(Filestext, sticky = "e")
	tkgrid.configure(lboxfiles, columnspan = 2, sticky = "sew", padx = c(0,0))
	#tkgrid.configure(ybar, sticky = "w", padx = c(0,10))
	
	
	##2nd row
	tkgrid(xbar, column = 1, columnspan = 3, pady = c(0,10), padx = 0, sticky="new")
	
	##3rd row
	tkgrid(Settingstext, pady = 5)
	tkgrid(settings.entry, row = 2, column = 1, columnspan = 3, pady = 5)
	tkgrid(chooseSettings.but, column = 5, row = 2, padx = 5, sticky = "w")
	##CONF
	tkgrid.configure(settings.entry, sticky = "new")
	tkgrid.configure(Settingstext, sticky="e")
	
	##4th row
	tkgrid(Compoundlisttext, pady = 5)
	tkgrid(compoundList.entry, row = 3, column = 1, columnspan = 3, pady = 5)
	tkgrid(choosecompoundList.but, column = 5, row = 3, padx = 5, sticky = "w")
	##CONF
	tkgrid.configure(compoundList.entry, sticky = "new")
	tkgrid.configure(Compoundlisttext, sticky="e")
	
	##5th row
	tkgrid(tklabel(tt, text = "Method of reading the \n files"), cBoxmethod, tklabel(tt, text = "Ionization mass:"), cBoxmode)
	
	##6th row
	tkgrid(submit.but, row = 5, column = 1, pady = c(10,0))
	
	tkgrid.columnconfigure(tt, 0, weight = 1)
	tkgrid.columnconfigure(tt, 1, weight = 1)
	tkgrid.columnconfigure(tt, 2, weight = 1)
	tkgrid.columnconfigure(tt, 3, weight = 1)
	tkgrid.columnconfigure(tt, 4, weight = 1)
	tkgrid.columnconfigure(tt, 5, weight = 1)
	tkgrid.rowconfigure(tt, 0, weight = 1)
	tkgrid.rowconfigure(tt, 1, weight = 1)
	tkgrid.rowconfigure(tt, 2, weight = 1)
	tkgrid.rowconfigure(tt, 3, weight = 1)
	tkgrid.rowconfigure(tt, 4, weight = 1)
	tkgrid.rowconfigure(tt, 5, weight = 1)
	
	##TOP MENU:
	topMenu <- tkmenu(tt) # Create a menu
	tkconfigure(tt, menu = topMenu) # Add it to the 'tt' window
	
	##FILE MENU:
	fileMenu <- tkmenu(topMenu, tearoff = FALSE)
	
	##TAB 1
	filelistMenu <- tkmenu(fileMenu, tearoff = FALSE)
	tkadd(filelistMenu, "command", label = "New...", command = function() {
		filename <- tk_choose.files(multi = FALSE, filters = NULL, index = 1)
		filemat <- as.matrix(read.csv(filename))
		cpdID <- list()
		guifiles <- list()
		.Tcl(paste(.Tk.ID(lboxfiles), "delete 0 end"))
		.Tcl(paste(.Tk.ID(lboxcpdID), "delete 0 end")) 
		for(i in 1:nrow(filemat)){
			tkinsert(lboxfiles, "end", filemat[i,"Files"])
			tkinsert(lboxcpdID, "end", filemat[i,"ID"])
			guifiles[[i]] <<- filemat[i,"Files"]
			cpdID[[i]] <<- filemat[i,"ID"]
		}
	})
	tkadd(filelistMenu, "command", label = "Append to current files", command = function() {
		filename <- tk_choose.files(multi = FALSE, filters = NULL, index = 1)
		filemat <- as.matrix(read.csv(filename))
		lfiles <- length(guifiles)
		for(i in 1:nrow(filemat)){
			guifiles[[lfiles+i]] <<- filemat[i,"Files"]
			cpdID[[lfiles+i]] <<- filemat[i,"ID"]
			tkinsert(lboxfiles, "end", filemat[i,"Files"])
			tkinsert(lboxcpdID, "end", filemat[i,"ID"])
		}
	})
	tkadd(fileMenu, "cascade", label = "Import file table...", menu = filelistMenu)
	tkadd(fileMenu, "separator")
	tkadd(fileMenu, "command", label = "Quit", command = function() tkdestroy(tt))
	
	
	##TAB 2
	optionsMenu <- tkmenu(fileMenu, tearoff = FALSE)
	tkadd(optionsMenu, "command", label = "Edit RMassBank settings", command=function(){
		SettingsGUI(tt)
	})
	tkadd(optionsMenu, "command", label = "Edit record annotations", command=function(){
		##Add later
	})
	tkadd(optionsMenu, "command", label = "Edit xcms parameters", command=function(){
		xcmsGUI(tt)
	})
	
	##SUBTAB 1 (deprofile)
	deprofileMenu <- tkmenu(fileMenu, tearoff = FALSE)
	tkadd(deprofileMenu, "radiobutton", label = "None", value = "NA", variable=dummyradio, command=function(){
		o <- getOption("RMassBank")
		o$deprofile <- NA
		options("RMassBank" = o)
	})
	tkadd(deprofileMenu, "radiobutton", label = "spline", value = "deprofile.spline", variable=dummyradio, command=function(){
		o <- getOption("RMassBank")
		o$deprofile <- "deprofile.spline"
		options("RMassBank" = o)
	})
	tkadd(deprofileMenu, "radiobutton", label = "fwhm", value = "deprofile.fwhm", variable=dummyradio, command=function(){
		o <- getOption("RMassBank")
		o$deprofile <- "deprofile.fwhm"
		options("RMassBank" = o)
	})
	tkadd(deprofileMenu, "radiobutton", label = "localMax", value = "deprofile.localMax", variable=dummyradio, command=function(){
		o <- getOption("RMassBank")
		o$deprofile <- "deprofile.localMax"
		options("RMassBank" = o)
	})
	
	tkadd(optionsMenu, "cascade", label = "Deprofiling options", menu=deprofileMenu)
	tkadd(optionsMenu, "command", label = "RT Shift/Margin", command=function(){
		RTGUI(tt)
	})
	tkadd(optionsMenu, "command", label = "Edit spectra list", command=function(){
		SLGUI(tt)
	})
	
	tkadd(topMenu, "cascade", label = "File", menu = fileMenu)
	tkadd(topMenu, "cascade", label = "Settings", menu = optionsMenu)
}
