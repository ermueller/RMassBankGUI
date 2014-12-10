WorkflowGUI <- function(){

	##Dummy variables
	initMain()
	updateXCMSoptions()
	stateGUI(ObjectEnv$tt)
	
	##IMAGE
	fpath <- system.file("cosmetics/RMassBank_logo.gif", package="RMassBankGUI")
	iconName <- "::tcl::logo"
	i1 <- tkimage.create("photo", iconName, file=fpath)
	a <- ttklabel(ObjectEnv$tt,image=iconName)
	
	##1st row
	tkgrid(a, column = 1, sticky = "w")
	
	##2nd row
	tkgrid(ObjectEnv$files.label, ObjectEnv$lboxfiles,  pady = c(10,0))
	tkgrid(ObjectEnv$lboxcpdID, pady = c(10,0), row = 1, column = 3, sticky = "sew")
	tkgrid(ObjectEnv$ybar, pady = c(10,0), row = 1, column = 4, sticky = "w", padx = c(0,10))
	tkgrid(ObjectEnv$choosefiles.but, row = 1, column = 5, padx = 5, pady = c(10,0), sticky = "w")
	tkgrid.configure(ObjectEnv$files.label, sticky = "e")
	tkgrid.configure(ObjectEnv$lboxfiles, columnspan = 2, sticky = "sew", padx = c(0,0))
	#tkgrid.configure(ybar, sticky = "w", padx = c(0,10))
	
	
	##3rd row
	tkgrid(ObjectEnv$xbar, column = 1, columnspan = 3, pady = c(0,10), padx = 0, sticky="new")
	
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
	tkgrid(tklabel(ObjectEnv$tt, text = "From step"), ObjectEnv$cBoxSteps1, tklabel(ObjectEnv$tt, text = "to step"), ObjectEnv$cBoxSteps2, row = 5, pady = 5)

	##7th row
	tkgrid(ObjectEnv$submit.but, row = 6, column = 1, pady = c(10,0))
	
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
	tkgrid.rowconfigure(ObjectEnv$tt, 6, weight = 1)
	
	##TOP MENU:
	ObjectEnv$topMenu <- tkmenu(ObjectEnv$tt) # Create a menu
	tkconfigure(ObjectEnv$tt, menu = ObjectEnv$topMenu) # Add it to the 'ObjectEnv$tt' window
	
	##MENU TABS:
	ObjectEnv$projectMenu <- tkmenu(ObjectEnv$topMenu, tearoff = FALSE)
	ObjectEnv$fileMenu <- tkmenu(ObjectEnv$topMenu, tearoff = FALSE)
	ObjectEnv$optionsMenu <- tkmenu(ObjectEnv$topMenu, tearoff = FALSE)
	ObjectEnv$debugMenu <- tkmenu(ObjectEnv$topMenu, tearoff = FALSE)
	
	##TAB 1
	##NEW PROJECT
	# tkadd(ObjectEnv$projectMenu, "command", label = "New project", command = function() {
		
		# yesnocancel <- tkmessageBox(icon = "warning" , message = "Do you want to save the changes to the current project?", title = "Warning", type = "yesnocancel")
		
		# if(tclvalue(yesnocancel) == "yes"){
			# saveCurrentProject()
		# }
		
		# if(tclvalue(yesnocancel) != "cancel"){
			# createNewProjectGUI()
		# }
	# })
	
	##OPEN PROJECT
	tkadd(ObjectEnv$projectMenu, "command", label = "Open Project Manager", command = function() {
	
		yesnocancel <- tkmessageBox(icon = "warning" , message = "Do you want to save the changes to the current project?", title = "Warning", type = "yesnocancel")
		
		if(tclvalue(yesnocancel) == "yes"){
			saveCurrentProject()
		}
		
		if(tclvalue(yesnocancel) == "no"){
			startGUI()
			tkdestroy(ObjectEnv$tt)
		}
		
	})
	
	##SAVE CURRENT PROJECT
	tkadd(ObjectEnv$projectMenu, "command", label = "Save", command = function() {
		saveCurrentProject()
	})
	
	tkadd(ObjectEnv$projectMenu, "separator")
	tkadd(ObjectEnv$projectMenu, "command", label = "Save and Close", command = function() {
		saveCurrentProject()
		tkdestroy(ObjectEnv$tt)
	})
	
	
	
	##TAB 2
	ObjectEnv$filelistMenu <- tkmenu(ObjectEnv$fileMenu, tearoff = FALSE)
	tkadd(ObjectEnv$filelistMenu, "command", label = "New...", command = function() {
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
	
	tkadd(ObjectEnv$filelistMenu, "command", label = "Append to current files", command = function() {
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
	
	tkadd(ObjectEnv$fileMenu, "cascade", label = "Import file table...", menu = ObjectEnv$filelistMenu)
	tkadd(ObjectEnv$fileMenu, "separator")
	tkadd(ObjectEnv$fileMenu, "command", label = "Quit", command = function() tkdestroy(ObjectEnv$tt))
	
	
	##TAB 3
	tkadd(ObjectEnv$optionsMenu, "command", label = "Edit Project settings", command=function(){
		editProjectGUI()
	})
	tkadd(ObjectEnv$optionsMenu, "command", label = "Edit xcms parameters", command=function(){
		xcmsGUI()
	})
	
	##SUBTAB 1 (deprofile)
	ObjectEnv$deprofileMenu <- tkmenu(ObjectEnv$fileMenu, tearoff = FALSE)
	tkadd(ObjectEnv$deprofileMenu, "radiobutton", label = "None", value = "NA", variable=WorkflowEnv$dummyradio, command=function(){
		o <- getOption("RMassBank")
		o$deprofile <- NA
		options("RMassBank" = o)
	})
	tkadd(ObjectEnv$deprofileMenu, "radiobutton", label = "spline", value = "deprofile.spline", variable=WorkflowEnv$dummyradio, command=function(){
		o <- getOption("RMassBank")
		o$deprofile <- "deprofile.spline"
		options("RMassBank" = o)
	})
	tkadd(ObjectEnv$deprofileMenu, "radiobutton", label = "fwhm", value = "deprofile.fwhm", variable=WorkflowEnv$dummyradio, command=function(){
		o <- getOption("RMassBank")
		o$deprofile <- "deprofile.fwhm"
		options("RMassBank" = o)
	})
	tkadd(ObjectEnv$deprofileMenu, "radiobutton", label = "localMax", value = "deprofile.localMax", variable=WorkflowEnv$dummyradio, command=function(){
		o <- getOption("RMassBank")
		o$deprofile <- "deprofile.localMax"
		options("RMassBank" = o)
	})
	
	tkadd(ObjectEnv$optionsMenu, "cascade", label = "Deprofiling options", menu=ObjectEnv$deprofileMenu)
	
	tkadd(ObjectEnv$optionsMenu, "command", label = "Edit spectra list", command=function(){
		SLGUI(ObjectEnv$tt)
	})
	tkadd(ObjectEnv$optionsMenu, "command", label = "RT Shift/Margin", command=function(){
		RTGUI(ObjectEnv$tt)
	})
	
	##TAB 4
	tkadd(ObjectEnv$debugMenu, "command", label = "Debug options", command=function(){
		debugGUI(ObjectEnv$tt)
	})
	
	
	
	tkadd(ObjectEnv$topMenu, "cascade", label = "File", menu = ObjectEnv$projectMenu)
	tkadd(ObjectEnv$topMenu, "cascade", label = "Edit", menu = ObjectEnv$fileMenu)
	tkadd(ObjectEnv$topMenu, "cascade", label = "Settings", menu = ObjectEnv$optionsMenu)
	tkadd(ObjectEnv$topMenu, "cascade", label = "Debug", menu = ObjectEnv$debugMenu)
	
	Sys.sleep(0.1)
	.Tcl(paste("wm resizable", .Tk.ID(ObjectEnv$tt), 0, 0))
}