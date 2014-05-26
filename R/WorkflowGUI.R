require(tcltk)
WorkflowGUI <- function(){
	
	##Initialize Variables
	guifiles <- list()
	cpdID <- list()
	
	##SETTINGS GETS ITS OWN MENU
	settings <- tclVar("")
	
	##COMPOUNDLIST, TOO?
	compoundList <- tclVar("")
	
	
	rMethod <- tclVar("mzR")
	mzmode <- tclVar("pH")
   
	##Open the window
	tt <- tktoplevel()
	tkwm.title(tt,"Interactive GUI for using the RMassBank Workflow")
	
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
			do.call(tkdelete,c(list(lboxfiles),as.list(index)))
			do.call(tkdelete,c(list(lboxcpdID),as.list(index)))
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
			do.call(tkdelete,c(list(lboxfiles),as.list(index)))
			do.call(tkdelete,c(list(lboxcpdID),as.list(index)))
			for(i in rev(index)){
				guifiles[[i+1]] <<- NULL
				cpdID[[i+1]] <<- NULL
			}
		}
	})
	
	##List box for compound IDs
	ybarcpdID <- ttkscrollbar(tt, command = function(...)tkyview(lboxcpdID,...))
	
	cBoxmethod <- ttkcombobox(tt, state = "readonly", textvariable = rMethod, values = c("mzR", "xcms", "peaklists"))
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
		SET <- tk_choose.files(multi = FALSE)
		if(length(SET) > 0){
			tclvalue(settings) <- SET
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
				guifiles[[j]] <- i
				j <- j+1
				tkinsert(lboxfiles,"end",basename(i))
			}
			
			##cpdIDs
			for(i in 2818:2819){
				cpdID[[i-2817]] <- i
			}
			
			##settings
			RmbSettingsTemplate("mysettings.ini")
			
			tclvalue(settings) <- "mysettings.ini"
			print(guifiles)
		} else {
			tkmessageBox(message = "You need to install RMassBankData to show the example!")
		}
	}
	
	submit <- function(){
		w <- newMsmsWorkspace()
		ROfiles <- vector()
		ROcpdid <- vector()
		print(guifiles[[1]])
		for(i in 1:length(guifiles)){
			ROfiles[i] <- tclvalue(guifiles[[i]])
			ROcpdid[i] <- as.numeric(tclvalue(cpdID[[i]]))
		}
		
		Args <- list(method = "centWave", ppm = 5, snthresh = 1.5,
                  peakwidth = c(20,60), integrate = 1, mzdiff = -0.001, mzCenterFun = "meanApex3")
		loadRmbSettings(tclvalue(settings))
		loadList(tclvalue(compoundList))
		w <- msmsRead(w,files = ROfiles, cpdids = ROcpdid, readMethod = tclvalue(rMethod), mode = tclvalue(mzmode), 
					confirmMode = FALSE, useRtLimit = TRUE, Args = Args, settings = getOption("RMassBank"), 
					progressbar = "progressBarHook", MSe = FALSE)
		w <- msmsWorkflow(w, steps=2:8)
	}
	
	choosefiles.but <- ttkbutton(tt, text = "Choose mz-files", command = choosefiles)
	editcpdID.but <- ttkbutton(tt, text = "Edit compound IDs", command = editcpdID)
	chooseSettings.but <- ttkbutton(tt, text = "Choose the file settings", command = chooseSettings)
	choosecompoundList.but <- ttkbutton(tt, text = "Choose the compound list", command = choosecompoundList)
	example.but <- ttkbutton(tt, text = "Example Workflow", command = example)
	submit.but <- ttkbutton(tt, text = "Submit", command = submit)
	
	
	##Labels
	Filestext <- ttklabel(tt,text="Files:")
	Filetabletext <- ttklabel(tt,text="File table:")
	Settingstext <- ttklabel(tt,text="Settings:")
	Compoundlisttext <- ttklabel(tt,text="Compound list:")
	
	
	##1st row
	tkgrid(Filestext, lboxfiles,  pady = c(10,0))
	tkgrid(lboxcpdID, pady = c(10,0), row = 0, column = 3, sticky = "sew")
	tkgrid(ybar, pady = c(10,0), row = 0, column = 4, sticky = "w", padx = c(0,10))
	tkgrid(choosefiles.but, row = 0, column = 5, padx = 5, pady = c(10,0), sticky = "w")
	tkgrid.configure(Filestext, sticky = "e")
	tkgrid.configure(lboxfiles, columnspan = 2, sticky = "sew", padx = c(0,0))
	#tkgrid.configure(ybar, sticky = "w", padx = c(0,10))
	
	
	##2nd row
	tkgrid(xbar, column = 1, columnspan = 3, pady = c(0,10), padx = 0, sticky="new")
	
	
	##3rd row
	tkgrid(Filetabletext, pady = 5)
	tkgrid(fileTable.entry, row = 2, column = 1, columnspan = 3, pady = 5)
	tkgrid(choosefileTable.but, column = 5, row = 2, padx = 5, sticky = "w")
	##CONF
	tkgrid.configure(fileTable.entry, sticky = "new")
	tkgrid.configure(Filetabletext, sticky="e")
	
	
	##4th row
	tkgrid(Settingstext, pady = 5)
	tkgrid(settings.entry, row = 3, column = 1, columnspan = 3, pady = 5)
	tkgrid(chooseSettings.but, column = 5, row = 3, padx = 5, sticky = "w")
	##CONF
	tkgrid.configure(settings.entry, sticky = "new")
	tkgrid.configure(Settingstext, sticky="e")
	
	##5th row
	tkgrid(Compoundlisttext, pady = 5)
	tkgrid(compoundList.entry, row = 4, column = 1, columnspan = 3, pady = 5)
	tkgrid(choosecompoundList.but, column = 5, row = 4, padx = 5, sticky = "w")
	##CONF
	tkgrid.configure(compoundList.entry, sticky = "new")
	tkgrid.configure(Compoundlisttext, sticky="e")
	
	##6th row
	tkgrid(tklabel(tt, text = "Method of reading the \n files"), cBoxmethod, tklabel(tt, text = "Ionization mass:"), cBoxmode)
	
	##7th row
	tkgrid(example.but, submit.but, pady = c(30,0))
	
	
	##TOP MENU:
	topMenu <- tkmenu(tt) # Create a menu
	tkconfigure(tt, menu = topMenu) # Add it to the 'tt' window
	
	##FILE MENU:
	fileMenu <- tkmenu(topMenu, tearoff = FALSE)
	filelistMenu <- tkmenu(fileMenu, tearoff = FALSE)
	tkadd(filelistMenu, "command", label = "New...", command = function() {
		filename <- tk_choose.files(multi = FALSE, filters = NULL, index = 1)
		filemat <- as.matrix(read.csv(filename))
		cpdID <- list()
		guifiles <- list()
		.Tcl(paste(.Tk.ID(lboxfiles), "delete 0 END"))
		.Tcl(paste(.Tk.ID(lboxcpdID), "delete 0 END")) 
		for(i in 1:nrow(filemat)){
			tkinsert(lboxfiles, "end", filemat[i,"files"])
			tkinsert(lboxcpdID, "end", filemat[i,"cpdID"])
			guifiles[[i]] <<- filemat[i,"files"]
			cpdID[[i]] <<- filemat[i,"cpdID"]
		}
	})
	tkadd(filelistMenu, "command", label = "Append to current files", command = function() {
		filename <- tk_choose.files(multi = FALSE, filters = NULL, index = 1)
		filemat <- as.matrix(read.csv(filename))
		lfiles <- length(guifiles)
		for(i in 1:nrow(filemat)){
			guifiles[[lfiles+i]] <<- filemat[i,"files"]
			cpdID[[lfiles+i]] <<- filemat[i,"cpdID"]
			tkinsert(lboxfiles, "end", filemat[i,"files"])
			tkinsert(lboxcpdID, "end", filemat[i,"cpdID"])
		}
	})
	tkadd(fileMenu, "cascade", label = "Import file table...", menu = filelistMenu)
	tkadd(fileMenu, "separator")
	tkadd(fileMenu, "command", label = "Quit", command = function() tkdestroy(tt))
	
	tkadd(topMenu, "cascade", label = "File", menu = fileMenu)

}