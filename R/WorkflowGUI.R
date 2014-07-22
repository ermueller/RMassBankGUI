WorkflowGUI <- function(){
	
	##Initialize Variables
	initMain()		##Initialize objects on the grid
	
	##Dummy variables
	ObjectEnv$dummyradio <- tclVar("NA")

	##IMAGE
	#fpath <- system.file("cosmetics/RMassBank_logo.gif", package="RMassBankGUI")
	#iconName <- "::tcl::logo"
	#i1 <- tkimage.create("photo", iconName, file=fpath)
	#a <- ttklabel(ObjectEnv$tt,image=iconName)
	
	##1st row
	tkgrid(ObjectEnv$files.label, ObjectEnv$lboxfiles,  pady = c(10,0))
	tkgrid(ObjectEnv$lboxcpdID, pady = c(10,0), row = 0, column = 3, sticky = "sew")
	tkgrid(ObjectEnv$ybar, pady = c(10,0), row = 0, column = 4, sticky = "w", padx = c(0,10))
	tkgrid(ObjectEnv$choosefiles.but, row = 0, column = 5, padx = 5, pady = c(10,0), sticky = "w")
	#tkgrid(a, row = 0, column = 5)
	tkgrid.configure(ObjectEnv$files.label, sticky = "e")
	tkgrid.configure(ObjectEnv$lboxfiles, columnspan = 2, sticky = "sew", padx = c(0,0))
	#tkgrid.configure(ybar, sticky = "w", padx = c(0,10))
	
	
	##2nd row
	tkgrid(ObjectEnv$xbar, column = 1, columnspan = 3, pady = c(0,10), padx = 0, sticky="new")
	
	##3rd row
	tkgrid(ObjectEnv$settings.label, pady = 5)
	tkgrid(ObjectEnv$settings.entry, row = 2, column = 1, columnspan = 3, pady = 5)
	tkgrid(ObjectEnv$chooseSettings.but, column = 5, row = 2, padx = 5, sticky = "w")
	##CONF
	tkgrid.configure(ObjectEnv$settings.entry, sticky = "new")
	tkgrid.configure(ObjectEnv$settings.label, sticky="e")
	
	##4th row
	tkgrid(ObjectEnv$compoundlist.label, pady = 5)
	tkgrid(ObjectEnv$compoundList.entry, row = 3, column = 1, columnspan = 3, pady = 5)
	tkgrid(ObjectEnv$choosecompoundList.but, column = 5, row = 3, padx = 5, sticky = "w")
	##CONF
	tkgrid.configure(ObjectEnv$compoundList.entry, sticky = "new")
	tkgrid.configure(ObjectEnv$compoundlist.label, sticky="e")
	
	##5th row
	tkgrid(tklabel(ObjectEnv$tt, text = "Method of reading the \n files"), ObjectEnv$cBoxmethod, tklabel(ObjectEnv$tt, text = "Ionization mass:"), ObjectEnv$cBoxmode)
	
	##6th row
	tkgrid(ObjectEnv$submit.but, row = 5, column = 1, pady = c(10,0))
	
	tkgrid.columnconfigure(ObjectEnv$tt, 0, weight = 1)
	tkgrid.columnconfigure(ObjectEnv$tt, 1, weight = 1)
	tkgrid.columnconfigure(ObjectEnv$tt, 2, weight = 1)
	tkgrid.columnconfigure(ObjectEnv$tt, 3, weight = 1)
	tkgrid.columnconfigure(ObjectEnv$tt, 4, weight = 1)
	tkgrid.columnconfigure(ObjectEnv$tt, 5, weight = 1)
	tkgrid.rowconfigure(ObjectEnv$tt, 0, weight = 1)
	tkgrid.rowconfigure(ObjectEnv$tt, 1, weight = 1)
	tkgrid.rowconfigure(ObjectEnv$tt, 2, weight = 1)
	tkgrid.rowconfigure(ObjectEnv$tt, 3, weight = 1)
	tkgrid.rowconfigure(ObjectEnv$tt, 4, weight = 1)
	tkgrid.rowconfigure(ObjectEnv$tt, 5, weight = 1)
	
	##TOP MENU:
	ObjectEnv$topMenu <- tkmenu(ObjectEnv$tt) # Create a menu
	tkconfigure(ObjectEnv$tt, menu = ObjectEnv$topMenu) # Add it to the 'ObjectEnv$tt' window
	
	##FILE MENU:
	ObjectEnv$fileMenu <- tkmenu(ObjectEnv$topMenu, tearoff = FALSE)
	
	##TAB 1
	filelistMenu <- tkmenu(ObjectEnv$fileMenu, tearoff = FALSE)
	tkadd(filelistMenu, "command", label = "New...", command = function() {
		filename <- tk_choose.files(multi = FALSE, filters = NULL, index = 1)
		filemat <- as.matrix(read.csv(filename))
		WorkflowEnv$cpdID <- vector()
		WorkflowEnv$guifiles <- vector()
		.Tcl(paste(.Tk.ID(ObjectEnv$lboxfiles), "delete 0 end"))
		.Tcl(paste(.Tk.ID(ObjectEnv$lboxcpdID), "delete 0 end")) 
		for(i in 1:nrow(filemat)){
			tkinsert(ObjectEnv$lboxfiles, "end", filemat[i,"Files"])
			tkinsert(ObjectEnv$lboxcpdID, "end", filemat[i,"ID"])
			WorkflowEnv$guifiles[i] <- filemat[i,"Files"]
			WorkflowEnv$cpdID[i] <- filemat[i,"ID"]
		}
	})
	
	tkadd(filelistMenu, "command", label = "Append to current files", command = function() {
		filename <- tk_choose.files(multi = FALSE, filters = NULL, index = 1)
		filemat <- as.matrix(read.csv(filename))
		lfiles <- length(WorkflowEnv$guifiles)
		for(i in 1:nrow(filemat)){
			WorkflowEnv$guifiles[lfiles+i] <- filemat[i,"Files"]
			WorkflowEnv$cpdID[lfiles+i] <- filemat[i,"ID"]
			tkinsert(ObjectEnv$lboxfiles, "end", filemat[i,"Files"])
			tkinsert(ObjectEnv$lboxcpdID, "end", filemat[i,"ID"])
		}
	})
	
	tkadd(ObjectEnv$fileMenu, "cascade", label = "Import file table...", menu = filelistMenu)
	tkadd(ObjectEnv$fileMenu, "separator")
	tkadd(ObjectEnv$fileMenu, "command", label = "Quit", command = function() tkdestroy(ObjectEnv$tt))
	
	
	##TAB 2
	ObjectEnv$optionsMenu <- tkmenu(ObjectEnv$fileMenu, tearoff = FALSE)
	tkadd(ObjectEnv$optionsMenu, "command", label = "Edit RMassBank settings", command=function(){
		SettingsGUI(ObjectEnv$tt)
	})
	tkadd(ObjectEnv$optionsMenu, "command", label = "Edit record annotations", command=function(){
		##Add later
	})
	tkadd(ObjectEnv$optionsMenu, "command", label = "Edit xcms parameters", command=function(){
		xcmsGUI()
	})
	
	##SUBTAB 1 (deprofile)
	ObjectEnv$deprofileMenu <- tkmenu(ObjectEnv$fileMenu, tearoff = FALSE)
	tkadd(ObjectEnv$deprofileMenu, "radiobutton", label = "None", value = "NA", variable=ObjectEnv$dummyradio, command=function(){
		o <- getOption("RMassBank")
		o$deprofile <- NA
		options("RMassBank" = o)
	})
	tkadd(ObjectEnv$deprofileMenu, "radiobutton", label = "spline", value = "deprofile.spline", variable=ObjectEnv$dummyradio, command=function(){
		o <- getOption("RMassBank")
		o$deprofile <- "deprofile.spline"
		options("RMassBank" = o)
	})
	tkadd(ObjectEnv$deprofileMenu, "radiobutton", label = "fwhm", value = "deprofile.fwhm", variable=ObjectEnv$dummyradio, command=function(){
		o <- getOption("RMassBank")
		o$deprofile <- "deprofile.fwhm"
		options("RMassBank" = o)
	})
	tkadd(ObjectEnv$deprofileMenu, "radiobutton", label = "localMax", value = "deprofile.localMax", variable=ObjectEnv$dummyradio, command=function(){
		o <- getOption("RMassBank")
		o$deprofile <- "deprofile.localMax"
		options("RMassBank" = o)
	})
	
	tkadd(ObjectEnv$optionsMenu, "cascade", label = "Deprofiling options", menu=ObjectEnv$deprofileMenu)
	tkadd(ObjectEnv$optionsMenu, "command", label = "RT Shift/Margin", command=function(){
		RTGUI(ObjectEnv$tt)
	})
	tkadd(ObjectEnv$optionsMenu, "command", label = "Edit spectra list", command=function(){
		SLGUI(ObjectEnv$tt)
	})
	
	tkadd(ObjectEnv$topMenu, "cascade", label = "File", menu = ObjectEnv$fileMenu)
	tkadd(ObjectEnv$topMenu, "cascade", label = "Settings", menu = ObjectEnv$optionsMenu)
}