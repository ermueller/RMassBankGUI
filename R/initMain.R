XCMSEnv <- new.env()
XCMSDUMMY <- new.env()
WorkflowEnv <- new.env()
ObjectEnv <- new.env()

initMain <- function(){
	Scrolls()			##Initialize Scrolling commands for tcl/tk
	##initXCMSEnv()		##Variables for XCMS Parameters
	initMainWindow()	##Initialize the main window
	initEntries()		##Initialize entries for main window
	initLabels()		##Initialize labels for main window
	initBars()			##Initialize scrollbars for main window
	initListBoxes()		##Initialize listboxes for main window
	initCboxes()		##Initialize comboboxes for main window
	initButtons()		##Initialize buttons for main window
}

resetMain <- function(){

	##Variables
	tclvalue(WorkflowEnv$compoundList) <- ""
	tclvalue(WorkflowEnv$rMethod) <- "mzR"
	tclvalue(WorkflowEnv$mzmode) <- "pH"
	WorkflowEnv$guifiles <- vector()
	WorkflowEnv$cpdID <- vector()
	tclvalue(WorkflowEnv$settings) <- ""
	tclvalue(WorkflowEnv$compoundList) <- ""
	
	##Listboxes
	.Tcl(paste(.Tk.ID(ObjectEnv$lboxfiles), "delete 0 end"))
	.Tcl(paste(.Tk.ID(ObjectEnv$lboxcpdID), "delete 0 end"))
	
	##XCMS Variables
	tclvalue(XCMSEnv$ppm) <- 25
	tclvalue(XCMSEnv$peakwidthstart) <-20
	tclvalue(XCMSEnv$peakwidthend) <- 50
	tclvalue(XCMSEnv$snthresh) <- 10
	tclvalue(XCMSEnv$prefilterpeaks) <- 3
	tclvalue(XCMSEnv$prefilterintensity) <- 100
	tclvalue(XCMSEnv$method) <- "centWave"
	tclvalue(XCMSEnv$mzCenterFun) <- "wMeanApex3"
	tclvalue(XCMSEnv$integrate) <- 1
	tclvalue(XCMSEnv$mzdiff) <- -0.001
	tclvalue(XCMSEnv$fitGauss) <- "FALSE"
	updateXCMSoptions()
	
	##XCMS Dummy variables
	# tclvalue(XCMSDUMMY$ppm) <- as.numeric(tclvalue(XCMSEnv$ppm))
	# tclvalue(XCMSDUMMY$peakwidthstart) <- as.numeric(tclvalue(XCMSEnv$peakwidthstart)) * 10
	# tclvalue(XCMSDUMMY$peakwidthend) <- as.numeric(tclvalue(XCMSEnv$peakwidthend)) * 10
	# tclvalue(XCMSDUMMY$snthresh) <- as.numeric(tclvalue(XCMSEnv$snthresh)) * 5
	# tclvalue(XCMSDUMMY$prefilterpeaks) <- as.numeric(tclvalue(XCMSEnv$prefilterpeaks))
	# tclvalue(XCMSDUMMY$prefilterintensity) <- as.numeric(tclvalue(XCMSEnv$prefilterintensity))
	# tclvalue(XCMSDUMMY$mzdiff) <- as.numeric(tclvalue(XCMSEnv$mzdiff)) * 1000
	# tclvalue(XCMSDUMMY$fitGauss) <- as.logical(tclvalue(XCMSEnv$fitGauss))
	
	
	
}


initWorkflowEnv <- function(){
	##Environment for main GUI
	WorkflowEnv$compoundList <- tclVar("")
	WorkflowEnv$rMethod <- tclVar("mzR")
	WorkflowEnv$mzmode <- tclVar("pH")
	WorkflowEnv$guifiles <- vector()
	WorkflowEnv$cpdID <- vector()
	WorkflowEnv$settings <- tclVar("")
	WorkflowEnv$compoundList <- tclVar("")
	WorkflowEnv$dummyradio <- tclVar("NA")
}

initXCMSEnv <- function(){
	##Environment for XCMS values
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
}

initMainWindow <- function(){
	ObjectEnv$tt <- tktoplevel(height=400, width = 900)  ##Create window
	tkwm.title(ObjectEnv$tt,currentProjectEnv$currentProject) ##Window title
	tkgrid.propagate(ObjectEnv$tt,1)
}

initEntries <- function(){
	##Entries for main GUI
	ObjectEnv$settings.entry <- ttkentry(ObjectEnv$tt, textvariable = WorkflowEnv$settings)
	ObjectEnv$compoundList.entry <- ttkentry(ObjectEnv$tt, textvariable = WorkflowEnv$compoundList)
}

initLabels <- function(){
	##Labels for main GUI
	ObjectEnv$files.label <- ttklabel(ObjectEnv$tt,text="Files:")
	ObjectEnv$settings.label <- ttklabel(ObjectEnv$tt,text="Settings:")
	ObjectEnv$compoundlist.label <- ttklabel(ObjectEnv$tt,text="Compound list:")
}

initBars <- function(){
	##Bars for main GUI
	ObjectEnv$xbar <- ttkscrollbar(ObjectEnv$tt, orient = "horizontal", command = function(...)tkxview(ObjectEnv$lboxfiles,...))
	ObjectEnv$ybar <- ttkscrollbar(ObjectEnv$tt, command = function(...)pseudoSynchScroll(list(ObjectEnv$lboxfiles,ObjectEnv$lboxcpdID),...))
}

initListBoxes <- function(){
	##ListBoxes for main GUI
	ObjectEnv$lboxfiles <- tklistbox(ObjectEnv$tt, width = 60, borderwidth = 0 , height = 4 , selectmode = "multiple", 
						xscrollcommand = function(...)tkset(ObjectEnv$xbar, ...),
						yscrollcommand = function(...)pseudoSetScroll(ObjectEnv$ybar, ...), 
						background = "white")
	ObjectEnv$lboxcpdID <- tklistbox(ObjectEnv$tt, width = 20, borderwidth = 0 , height = 4 , selectmode = "multiple", 
						yscrollcommand = function(...)pseudoSetScroll(ObjectEnv$ybar, ...), 
						background = "white")
	bindDel()
}

initCboxes <- function(){
	##Comboboxes for main GUI
	ObjectEnv$cBoxmethod <- ttkcombobox(ObjectEnv$tt, state = "readonly", textvariable = WorkflowEnv$rMethod, values = c("mzR", "xcms", "peaklist"))
	ObjectEnv$cBoxmode <- ttkcombobox(ObjectEnv$tt, state = "readonly", textvariable = WorkflowEnv$mzmode, values = c("pH", "mH"))
	#ObjectEnv$cBoxsteps <- ttkcombobox(ObjectEnv$tt, state = "readonly", textvariable = WorkflowEnv$steps, values = 1:8
}

initButtons <- function(){

	choosefiles <- function(){
		F <- tk_choose.files()
		if(length(F) > 0){
			addFilesGUI(F)
		}
	}
	
	chooseSettings <- function(){
		if(!is.null(getOption("RMassBank", NULL))){
			change <- changeSettings()
		}
		
		if(change){
			SET <- tk_choose.files(multi = FALSE)
			
			if(length(SET) > 0){
				tclvalue(WorkflowEnv$settings) <- SET
				loadRmbSettings(SET)
				updateXCMSoptions()
				if(is.na(getOption("RMassBank")$deprofile)){
					ObjectEnv$dummyradio <- tclVar("NA")
				} else { ObjectEnv$dummyradio <- tclVar(getOption("RMassBank")$deprofile) }
			}
		}
	}
	
	choosecompoundList <- function(){
		CLI <- tk_choose.files(multi = FALSE)
		if(length(CLI) > 0){
			tclvalue(WorkflowEnv$compoundList) <- CLI
			loadList(tclvalue(WorkflowEnv$compoundList))
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
				WorkflowEnv$guifiles[j] <- i
				j <- j+1
				tkinsert(ObjectEnv$lboxfiles,"end",basename(i))
			}
			
			##cpdIDs
			for(i in 2818:2819){
				cpdID[[i-2817]] <- i
				tkinsert(lboxcpdID,"end",i)
			}
			
			##settings
			RmbSettingsTemplate("mysettings.ini")
			
			tclvalue(WorkflowEnv$settings) <- "mysettings.ini"

		} else {
			tkmessageBox(message = "You need to install RMassBankData to show the example!")
		}
	}
	
	submit <- function(){
		updateXCMSoptions()
		if(tclvalue(WorkflowEnv$compoundList)!=""){
			o <- getOption("RMassBank")
			if(!is.null(o$xcms)){
				saveCurrentProject()
				w <- newMsmsWorkspace()
				loadList(tclvalue(WorkflowEnv$compoundList))
				oo <- o$xcms
				Args <- list(method = oo$method, ppm = oo$ppm, snthresh = oo$snthresh,
						  peakwidth = oo$peakwidth, integrate = oo$integrate, mzdiff = oo$mzdiff, mzCenterFun = oo$mzCenterFun)
				w <- msmsRead(w,files = WorkflowEnv$guifiles, cpdids = WorkflowEnv$cpdID, readMethod = tclvalue(WorkflowEnv$rMethod), mode = tclvalue(WorkflowEnv$mzmode), 
							confirmMode = FALSE, useRtLimit = TRUE, Args = Args, settings = getOption("RMassBank"), 
							progressbar = "progressBarHook", MSe = FALSE)
				w <- msmsWorkflow(w, steps=2:8)
				w2 <- newMbWorkspace(w)
				w2 <- mbWorkflow(w2, steps=1:2)
				w2 <- loadInfolist(w2, "infolist.csv")
				w2 <- mbWorkflow(w2, steps=3:8)
				tk_messageBox(message = paste("The record data and moldata have been written to", file.path(getwd(),o$annotations$entry_prefix)))
			}
		}
	}
	
	ObjectEnv$choosefiles.but <- ttkbutton(ObjectEnv$tt, text = "Choose mz-files", command = choosefiles, width=25)
	ObjectEnv$chooseSettings.but <- ttkbutton(ObjectEnv$tt, text = "Choose the file settings", command = chooseSettings, width=25)
	ObjectEnv$choosecompoundList.but <- ttkbutton(ObjectEnv$tt, text = "Choose the compound list", command = choosecompoundList, width=25)
	ObjectEnv$example.but <- ttkbutton(ObjectEnv$tt, text = "Example Workflow", command = example)
	ObjectEnv$submit.but <- ttkbutton(ObjectEnv$tt, text = "Submit", command = submit)
}

initMenus <- function(){
	
}

updateXCMSoptions <- function(){
	##Update the XCMS values into the options
	oo <- getOption("RMassBank")
	oo$xcms$ppm <- as.numeric(tclvalue(XCMSEnv$ppm))
	oo$xcms$peakwidth[1] <- as.numeric(tclvalue(XCMSEnv$peakwidthstart))
	oo$xcms$peakwidth[2] <- as.numeric(tclvalue(XCMSEnv$peakwidthend))
	oo$xcms$snthresh <- as.numeric(tclvalue(XCMSEnv$snthresh))
	oo$xcms$prefilter[1] <- as.numeric(tclvalue(XCMSEnv$prefilterpeaks))
	oo$xcms$prefilter[2] <- as.numeric(tclvalue(XCMSEnv$prefilterintensity))
	oo$xcms$method <- tclvalue(XCMSEnv$method)
	oo$xcms$mzCenterFun <- tclvalue(XCMSEnv$mzCenterFun)
	oo$xcms$integrate <- as.numeric(tclvalue(XCMSEnv$integrate))
	oo$xcms$mzdiff <- as.numeric(tclvalue(XCMSEnv$mzdiff))
	oo$xcms$fitGauss <- tclvalue(XCMSEnv$fitGauss)
	options("RMassBank" = oo)
}

