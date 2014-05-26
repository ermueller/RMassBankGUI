WorkflowGUI <- function(tlwindow){
	require(tcltk)
	
	##Initialize Variables
	##???
	##WHAT
	
	##Open the window
	tt2 <- tktoplevel(tlwindow)
	tkwm.title(tt2,"Editing the settings")
	
	##Use the icon
	#tkwm.iconbitmap(tt, "something.ico")
	
	##Entry boxes
	fileTable.entry <- ttkentry(tt, textvariable = fileTable)
	settings.entry <- ttkentry(tt, textvariable = settings)
	compoundList.entry <- ttkentry(tt, textvariable = compoundList)
	
	##List box for files
	xbar <- ttkscrollbar(tt, orient = "horizontal", command = function(...)tkxview(lboxfiles,...))
	ybar <- ttkscrollbar(tt, command = function(...)tkyview(lboxfiles,...))
	lboxfiles <- tklistbox(tt, width = 60, borderwidth = 0 , height = 4 , selectmode = "single", 
						xscrollcommand = function(...)tkset(xbar,...), 
						yscrollcommand = function(...)tkset(ybar,...), 
						background = "white")
	# # lboxtest <- tklistbox(tt, width = 60, borderwidth = 0 , height = 4 , selectmode = "single", 
						# # yscrollcommand = function(...)tkset(ybar,...), 
						# # background = "white")
	
	# # setScroll <- function(bar) {
		# # tkset(bar, ...)
		# # tcget(command, ...)
	# # }
	
	# # synchScroll <- function(wid) {
		# # wid[[1]] foreach w $widgets {$w {*}$args}
	# # }
	
	##List box for compound IDs
	ybarcpdID <- ttkscrollbar(tt, command = function(...)tkyview(lboxcpdID,...))
	lboxcpdID <- tklistbox(tt, width = 6, borderwidth = 0 , height = 4 , selectmode = "single",  
						yscrollcommand = function(...)tkset(ybarcpdID,...), 
						background = "white")
	
	cBoxmethod <- ttkcombobox(tt, state = "readonly", textvariable = rMethod, values = c("mzR", "xcms", "peaklists"))
	cBoxmode <- ttkcombobox(tt, state = "readonly", textvariable = mzmode, values = c("pH", "mH"))
	
	choosefiles <- function(){
		F <- tk_choose.files()
		offset <- length(files)
		if(length(F) > 0){
			for(i in 1:length(F)){
				files[[offset + i]] <<- F[i]
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
	
	choosefileTable <- function(){
		FLIST <- tk_choose.files(multi = FALSE)
		if(length(FLIST) > 0){
			tclvalue(fileTable) <- FLIST
		}
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
			.Tcl(paste(.Tk.ID(cBoxmethod) ,"set xcms"))
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
				files[[j]] <- i
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
			
		} else {
			tkmessageBox(message = "You need to install RMassBankData to show the example!")
		}
	}
	
	submit <- function(){
		w <- newMsmsWorkspace()
		ROfiles <- vector()
		ROcpdid <- vector()
		print(files[[1]])
		for(i in 1:length(files)){
			ROfiles[i] <- tclvalue(files[[i]])
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
	choosefileTable.but <- ttkbutton(tt, text = "Choose the file table", command = choosefileTable)
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
	tkgrid(Filestext, lboxfiles, pady = c(10,0))
	tkgrid(ybar, pady = c(10,0), row = 0, column = 4)
	tkgrid(choosefiles.but, row = 0, column = 5, padx = 5, pady = c(10,0), sticky = "w")
	#tkgrid(lboxtest)
	#for(i in 1:10)
	#tkinsert(lboxtest, "end", i)
	##CONF
	tkgrid.configure(Filestext, sticky = "e")
	tkgrid.configure(lboxfiles, columnspan = 3, sticky = "sew", padx = c(0,0))
	tkgrid.configure(ybar, sticky = "w", padx = c(0,10))
	
	
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
	#tkgrid()
	
	tkgrid(example.but, submit.but, pady = c(30,0))
}