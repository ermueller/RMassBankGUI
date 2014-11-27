currentProjectEnv <- new.env()
currentProjectEnv$currentProject <- NA
currentProjectEnv$fileDir <- NA
currentProjectEnv$annotations <- list()
projectNames <- list()

createNewProjectGUI <- function(){
	
	##WINDOW
	newProj <- tktoplevel()
	tkwm.title(newProj,"Project Specifications")
	tkgrid.propagate(newProj,1)
	
	
	##VARIABLES
	currentProjectEnv$name <- tclVar("")
	
	currentProjectEnv$annotations$authors <- tclVar("")
    currentProjectEnv$annotations$copyright <- tclVar("Copyright (C) XXX")
    currentProjectEnv$annotations$publication <- tclVar("")
    currentProjectEnv$annotations$license <- tclVar("")
    currentProjectEnv$annotations$instrument <- tclVar("")
    currentProjectEnv$annotations$instrument_type <- tclVar("")
    currentProjectEnv$annotations$confidence_comment <- tclVar("standard compound")
    currentProjectEnv$annotations$compound_class <- tclVar("N/A; Environmental Standard")
    currentProjectEnv$annotations$internal_id_fieldname <- tclVar("INTERNAL_ID")
	currentProjectEnv$annotations$entry_prefix <- tclVar("XX")
    currentProjectEnv$annotations$ms_type <- tclVar("MS2")
    currentProjectEnv$annotations$ionization <- tclVar("ESI")
	
	
    currentProjectEnv$annotations$lc_gradient <- tclVar("90/10 at 0 min, 50/50 at 4 min, 5/95 at 17 min, 5/95 at 25 min, 90/10 at 25.1 min, 90/10 at 30 min")
    currentProjectEnv$annotations$lc_flow <- tclVar("200 uL/min")
    currentProjectEnv$annotations$lc_solvent_a <- tclVar("water with 0.1% formic acid")
    currentProjectEnv$annotations$lc_solvent_b <- tclVar("water with 0.1% formic acid")
    currentProjectEnv$annotations$lc_column <- tclVar("XBridge C18 3.5um, 2.1x50mm, Waters")
	
	##ENTRIES
	name.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$name)
	authors.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$authors)
	copyright.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$copyright)
	publication.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$publication)
	license.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$license)
	instrument.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$instrument)
	instrument_type.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$instrument_type)
	confidence_comment.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$confidence_comment)
	compound_class.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$compound_class)
	internal_id_fieldname.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$internal_id_fieldname)
	entry_prefix.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$entry_prefix)
	ms_type.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$ms_type)
	ionization.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$ionization)
	lc_gradient.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$lc_gradient)
	lc_flow.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$lc_flow)
	lc_solvent_a.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$lc_solvent_a)
	lc_solvent_b.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$lc_solvent_b)
	lc_column.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$lc_column)
	
	##Labels
	name.label <- ttklabel(newProj, text="Name of Project:", foreground="red")
	authors.label <- ttklabel(newProj, text="Author(s) of the project:", foreground="red")
	copyright.label <- ttklabel(newProj,text="Copyright:")
	publication.label <- ttklabel(newProj,text="Publication:")
	license.label <- ttklabel(newProj,text="License:", foreground="red")
	instrument.label <- ttklabel(newProj,text="Instrument:", foreground="red")
	instrument_type.label <- ttklabel(newProj,text="Instrument type:", foreground="red")
	confidence_comment.label <-ttklabel(newProj,text="Confidence comment:", foreground="red")
	compound_class.label <- ttklabel(newProj,text="Compound class:", foreground="red")
	internal_id_fieldname.label <- ttklabel(newProj,text="Internal ID:", foreground="red")
	entry_prefix.label <- ttklabel(newProj,text="Entry prefix:", foreground="red")
	ms_type.label <- ttklabel(newProj,text="MS Type:", foreground="red")
	ionization.label <- ttklabel(newProj,text="Ionization:", foreground="red")
	lc_gradient.label <- ttklabel(newProj,text="FLOW_GRADIENT:")
	lc_flow.label <- ttklabel(newProj,text="FLOW_RATE:")
	lc_solvent_a.label <- ttklabel(newProj,text="SOLVENT A:")
	lc_solvent_b.label <- ttklabel(newProj,text="SOLVENT B:")
	lc_column.label <- ttklabel(newProj,text="COLUMN_NAME:")
	
	##Import
	import.but <- ttkbutton(parent=newProj, text="Import settings for project", command = function(){
		setpath <- tk_choose.files()
		if((length(setpath) > 0)){
			if(!grepl("ini", setpath, ignore.case = TRUE)){
				tkmessageBox(message="The supplied file is not an .ini file!")
			} else {
				loadRmbSettings(setpath)
				oo <- getOption("RMassBank")
				tclvalue(currentProjectEnv$annotations$authors) <- oo$annotations$authors
				if(!is.null(oo$annotations$copyright)){
					tclvalue(currentProjectEnv$annotations$copyright) <- oo$annotations$copyright
				}
				if(!is.null(oo$annotations$publication)){
					tclvalue(currentProjectEnv$annotations$publication) <- oo$annotations$publication
				}
				tclvalue(currentProjectEnv$annotations$license)	<- oo$annotations$license
				tclvalue(currentProjectEnv$annotations$instrument) <- oo$annotations$instrument
				tclvalue(currentProjectEnv$annotations$instrument_type) <- oo$annotations$instrument_type
				tclvalue(currentProjectEnv$annotations$confidence_comment) <- oo$annotations$confidence_comment
				tclvalue(currentProjectEnv$annotations$compound_class) <- oo$annotations$compound_class
				tclvalue(currentProjectEnv$annotations$internal_id_fieldname) <- oo$annotations$internal_id_fieldname
				tclvalue(currentProjectEnv$annotations$entry_prefix) <- oo$annotations$entry_prefix
				tclvalue(currentProjectEnv$annotations$ms_type) <- oo$annotations$ms_type
				tclvalue(currentProjectEnv$annotations$ionization) <- oo$annotations$ionization

				if(!is.null(oo$annotations$lc_gradient)){
					tclvalue(currentProjectEnv$annotations$lc_gradient) <- oo$annotations$lc_gradient
				}
				if(!is.null(oo$annotations$lc_flow)){
					tclvalue(currentProjectEnv$annotations$lc_flow)	<- oo$annotations$lc_flow
				}
				if(!is.null(oo$annotations$lc_solvent_a)){
					tclvalue(currentProjectEnv$annotations$lc_solvent_a) <- oo$annotations$lc_solvent_a
				}
				if(!is.null(oo$annotations$lc_solvent_b)){
					tclvalue(currentProjectEnv$annotations$lc_solvent_b) <- oo$annotations$lc_solvent_b
				}
				if(!is.null(oo$annotations$lc_column)){
					tclvalue(currentProjectEnv$annotations$lc_column) <- oo$annotations$lc_column
				}
			}
		}
	})
	
	##Submit
	##
	##
	submit.but <- ttkbutton(parent=newProj, text="Create project with these parameters", command = function(){
		
		##Which entryfields are empty?
		missingFields <- which(sapply(currentProjectEnv$annotations, function(x){return(tclvalue(x) == "")}))

		if(nchar(tclvalue(currentProjectEnv$annotations$entry_prefix)) == 2){
			if(all(missingFields %in% c(2,3,13,14,15,16,17))){
				if(all(tclvalue(currentProjectEnv$name) != startEnv$projects) && (tclvalue(currentProjectEnv$name) != "")){
					
					##Project directory
					projectDir <- file.path(currentProjectEnv$fileDir, tclvalue(currentProjectEnv$name))
					currentProjectEnv$currentProject <- tclvalue(currentProjectEnv$name)
	
							
					##If there are no settings currently loaded, use default settings
					print(getOption("RMassBank"))
					if(is.null(getOption("RMassBank"))){
						defaultSettings()
					}
					

					
					##Create directory and save the project
					dir.create(projectDir)
					saveCurrentProject()
					

					
					##Delete the "Projects" Listbox in the startGUI and fille them + the new project
					tkdelete(startEnv$lboxProjects,0,length(startEnv$projects)-1)
					startEnv$projects <- list.files(currentProjectEnv$fileDir)
					for(i in startEnv$projects){
						tkinsert(startEnv$lboxProjects,"end",i)
					}
					
					##Destroy the "create project" window 
					tkdestroy(newProj)
				} else{
					##Error message for double project name
					tkmessageBox(message = "A project with this name already exists", icon= "warning")
				}
			} else {
				##Error message for missing specifications
				tkmessageBox(message = "There are some mandatory specifications (marked in red) missing", icon= "warning")
			}
		} else {
			##Error for entry prefix not having 2 letters
			tkmessageBox(message = "The entry prefix must be made up of exactly 2 letters", icon= "warning")
		}
	})
	
	tkgrid(name.label, name.entry, sticky="ew")
	tkgrid(authors.label, authors.entry, sticky="ew")
	tkgrid(copyright.label, copyright.entry, sticky="ew")
	tkgrid(publication.label, publication.entry, sticky="ew")	
	tkgrid(license.label, license.entry, sticky="ew")	
	tkgrid(instrument.label, instrument.entry, sticky="ew")	
	tkgrid(instrument_type.label, instrument_type.entry, sticky="ew")
	tkgrid(confidence_comment.label, confidence_comment.entry, sticky="ew")
	tkgrid(compound_class.label, compound_class.entry, sticky="ew")	
	tkgrid(internal_id_fieldname.label, internal_id_fieldname.entry, sticky="ew")	
	tkgrid(entry_prefix.label, entry_prefix.entry, sticky="ew")	
	tkgrid(ms_type.label, ms_type.entry, sticky="ew")
	tkgrid(ionization.label, ionization.entry, sticky="ew")
	tkgrid(lc_gradient.label, lc_gradient.entry, sticky="ew")
	tkgrid(lc_flow.label, lc_flow.entry, sticky="ew")
	tkgrid(lc_solvent_a.label, lc_solvent_a.entry, sticky="ew")
	tkgrid(lc_solvent_b.label, lc_solvent_b.entry, sticky="ew")
	tkgrid(lc_column.label, lc_column.entry, sticky="ew")
	
	SEP <- ttkseparator(parent=newProj, orient="horizontal")
	tkgrid(SEP, columnspan=2, pady=c(5,5), sticky = "ew")
	tkgrid(import.but,submit.but)
	
	tkgrid.configure(name.label, sticky="e")
	tkgrid.configure(authors.label, sticky="e")
	tkgrid.configure(copyright.label, sticky="e")
	tkgrid.configure(publication.label, sticky="e")
	tkgrid.configure(license.label, sticky="e")
	tkgrid.configure(instrument.label, sticky="e")
	tkgrid.configure(instrument_type.label, sticky="e")
	tkgrid.configure(confidence_comment.label, sticky="e")
	tkgrid.configure(compound_class.label, sticky="e")
	tkgrid.configure(internal_id_fieldname.label, sticky="e")
	tkgrid.configure(entry_prefix.label, sticky="e")
	tkgrid.configure(ms_type.label, sticky="e")
	tkgrid.configure(ionization.label, sticky="e")
	tkgrid.configure(lc_gradient.label, sticky="e")
	tkgrid.configure(lc_flow.label, sticky="e")
	tkgrid.configure(lc_solvent_a.label, sticky="e")
	tkgrid.configure(lc_solvent_b.label, sticky="e")
	tkgrid.configure(lc_column.label, sticky="e")
	
	tkgrid.columnconfigure(newProj, 0, weight = 1)
	tkgrid.columnconfigure(newProj, 1, weight = 3)
	
    #currentProjectEnv$annotations$ms_dataprocessing <- list()
	Sys.sleep(0.1)
	.Tcl(paste("wm resizable", .Tk.ID(newProj), 0, 0)) ##Block resizing
}

###EDIT SETTINGS

editProjectGUI <- function(){
	
	##WINDOW
	newProj <- tktoplevel()
	tkwm.title(newProj,"Project Specifications")
	tkgrid.propagate(newProj,1)
	
	
	##VARIABLES
	currentProjectEnv$name <- tclVar("")
	ooc <- getOption("RMassBank")
	
	
	currentProjectEnv$annotations$authors <- tclVar(ooc$annotations$authors)
    if(!is.null(ooc$annotations$copyright)){
		currentProjectEnv$annotations$copyright <- tclVar(ooc$annotations$copyright)
    } else{
		currentProjectEnv$annotations$copyright <- tclVar("")
	}
	if(!is.null(ooc$annotations$publication)){
		currentProjectEnv$annotations$publication <- tclVar(ooc$annotations$publication)
    } else{
		currentProjectEnv$annotations$publication <- tclVar("")
	}
    currentProjectEnv$annotations$license <- tclVar(ooc$annotations$license)
    currentProjectEnv$annotations$instrument <- tclVar(ooc$annotations$instrument)
    currentProjectEnv$annotations$instrument_type <- tclVar(ooc$annotations$instrument_type)
    currentProjectEnv$annotations$confidence_comment <- tclVar(ooc$annotations$confidence_comment)
    currentProjectEnv$annotations$compound_class <- tclVar(ooc$annotations$compound_class)
    currentProjectEnv$annotations$internal_id_fieldname <- tclVar(ooc$annotations$internal_id_fieldname)
	currentProjectEnv$annotations$entry_prefix <- tclVar(ooc$annotations$entry_prefix)
    currentProjectEnv$annotations$ms_type <- tclVar(ooc$annotations$ms_type)
    currentProjectEnv$annotations$ionization <- tclVar(ooc$annotations$ionization)
	
	if(!is.null(ooc$annotations$lc_gradient)){
		currentProjectEnv$annotations$lc_gradient <- tclVar(ooc$annotations$lc_gradient)
    } else{
		currentProjectEnv$annotations$lc_gradient <- tclVar("")
	}
	
	if(!is.null(ooc$annotations$lc_flow)){
		currentProjectEnv$annotations$lc_flow <- tclVar(ooc$annotations$lc_flow)
    } else{
		currentProjectEnv$annotations$lc_flow <- tclVar("")
	}
	
	if(!is.null(ooc$annotations$lc_solvent_a)){
		currentProjectEnv$annotations$lc_solvent_a <- tclVar(ooc$annotations$lc_solvent_a)
    } else{
		currentProjectEnv$annotations$lc_solvent_a <- tclVar("")
	}
	
	if(!is.null(ooc$annotations$lc_solvent_b)){
		currentProjectEnv$annotations$lc_solvent_b <- tclVar(ooc$annotations$lc_solvent_b)
    } else{
		currentProjectEnv$annotations$lc_solvent_b <- tclVar("")
	}
	
	if(!is.null(ooc$annotations$lc_column)){
		currentProjectEnv$annotations$lc_column <- tclVar(ooc$annotations$lc_column)
    } else{
		currentProjectEnv$annotations$lc_column <- tclVar("")
	}
	
    currentProjectEnv$annotations$lc_solvent_a <- tclVar(ooc$annotations$lc_solvent_a)
    currentProjectEnv$annotations$lc_solvent_b <- tclVar(ooc$annotations$lc_solvent_b)
    currentProjectEnv$annotations$lc_column <- tclVar(ooc$annotations$lc_column)
	
	##ENTRIES
	#name.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$name)
	authors.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$authors)
	copyright.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$copyright)
	publication.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$publication)
	license.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$license)
	instrument.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$instrument)
	instrument_type.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$instrument_type)
	confidence_comment.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$confidence_comment)
	compound_class.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$compound_class)
	internal_id_fieldname.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$internal_id_fieldname)
	entry_prefix.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$entry_prefix)
	ms_type.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$ms_type)
	ionization.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$ionization)
	lc_gradient.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$lc_gradient)
	lc_flow.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$lc_flow)
	lc_solvent_a.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$lc_solvent_a)
	lc_solvent_b.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$lc_solvent_b)
	lc_column.entry <- ttkentry(newProj, width=20,  textvariable=currentProjectEnv$annotations$lc_column)
	
	##Labels
	#name.label <- ttklabel(newProj, text="Name of Project:", foreground="red")
	authors.label <- ttklabel(newProj, text="Author(s) of the project:", foreground="red")
	copyright.label <- ttklabel(newProj,text="Copyright:")
	publication.label <- ttklabel(newProj,text="Publication:")
	license.label <- ttklabel(newProj,text="License:", foreground="red")
	instrument.label <- ttklabel(newProj,text="Instrument:", foreground="red")
	instrument_type.label <- ttklabel(newProj,text="Instrument type:", foreground="red")
	confidence_comment.label <-ttklabel(newProj,text="Confidence comment:", foreground="red")
	compound_class.label <- ttklabel(newProj,text="Compound class:", foreground="red")
	internal_id_fieldname.label <- ttklabel(newProj,text="Internal ID:", foreground="red")
	entry_prefix.label <- ttklabel(newProj,text="Entry prefix:", foreground="red")
	ms_type.label <- ttklabel(newProj,text="MS Type:", foreground="red")
	ionization.label <- ttklabel(newProj,text="Ionization:", foreground="red")
	lc_gradient.label <- ttklabel(newProj,text="FLOW_GRADIENT:")
	lc_flow.label <- ttklabel(newProj,text="FLOW_RATE:")
	lc_solvent_a.label <- ttklabel(newProj,text="SOLVENT A:")
	lc_solvent_b.label <- ttklabel(newProj,text="SOLVENT B:")
	lc_column.label <- ttklabel(newProj,text="COLUMN_NAME:")
	
	##Import
	import.but <- ttkbutton(parent=newProj, text="Import settings for project", command = function(){
		setpath <- tk_choose.files()
		if((length(setpath) > 0)){
			if(!grepl("ini", setpath, ignore.case = TRUE)){
				tkmessageBox(message="The supplied file is not an .ini file!")
			} else {
				loadRmbSettings(setpath)
				oo <- getOption("RMassBank")
				tclvalue(currentProjectEnv$annotations$authors) <- oo$annotations$authors
				if(!is.null(oo$annotations$copyright)){
					tclvalue(currentProjectEnv$annotations$copyright) <- oo$annotations$copyright
				}
				if(!is.null(oo$annotations$publication)){
					tclvalue(currentProjectEnv$annotations$publication) <- oo$annotations$publication
				}
				tclvalue(currentProjectEnv$annotations$license)	<- oo$annotations$license
				tclvalue(currentProjectEnv$annotations$instrument) <- oo$annotations$instrument
				tclvalue(currentProjectEnv$annotations$instrument_type) <- oo$annotations$instrument_type
				tclvalue(currentProjectEnv$annotations$confidence_comment) <- oo$annotations$confidence_comment
				tclvalue(currentProjectEnv$annotations$compound_class) <- oo$annotations$compound_class
				tclvalue(currentProjectEnv$annotations$internal_id_fieldname) <- oo$annotations$internal_id_fieldname
				tclvalue(currentProjectEnv$annotations$entry_prefix) <- oo$annotations$entry_prefix
				tclvalue(currentProjectEnv$annotations$ms_type) <- oo$annotations$ms_type
				tclvalue(currentProjectEnv$annotations$ionization) <- oo$annotations$ionization

				if(!is.null(oo$annotations$lc_gradient)){
					tclvalue(currentProjectEnv$annotations$lc_gradient) <- oo$annotations$lc_gradient
				}
				if(!is.null(oo$annotations$lc_flow)){
					tclvalue(currentProjectEnv$annotations$lc_flow)	<- oo$annotations$lc_flow
				}
				if(!is.null(oo$annotations$lc_solvent_a)){
					tclvalue(currentProjectEnv$annotations$lc_solvent_a) <- oo$annotations$lc_solvent_a
				}
				if(!is.null(oo$annotations$lc_solvent_b)){
					tclvalue(currentProjectEnv$annotations$lc_solvent_b) <- oo$annotations$lc_solvent_b
				}
				if(!is.null(oo$annotations$lc_column)){
					tclvalue(currentProjectEnv$annotations$lc_column) <- oo$annotations$lc_column
				}
			}
		}
	})
	
	##Submit
	##
	##
	submit.but <- ttkbutton(parent=newProj, text="Save these parameters", command = function(){
		
		##Which entryfields are empty?
		missingFields <- which(sapply(currentProjectEnv$annotations, function(x){return(tclvalue(x) == "")}))

		if(nchar(tclvalue(currentProjectEnv$annotations$entry_prefix)) == 2){
			if(all(missingFields %in% c(2,3,13,14,15,16,17))){
				
					##Project directory
					projectDir <- file.path(currentProjectEnv$fileDir,currentProjectEnv$currentProject)
	
					##Overwrite options
					oo <- getOption("RMassBank")
					oo$annotations$authors <- tclvalue(currentProjectEnv$annotations$authors)
					oo$annotations$license <- tclvalue(currentProjectEnv$annotations$license)
						
					if(tclvalue(currentProjectEnv$annotations$publication) != ""){
						oo$annotations$publication <- tclvalue(currentProjectEnv$annotations$publication)
					} else {
						oo$annotations$publication <- NULL
					}

					if(tclvalue(currentProjectEnv$annotations$copyright) != ""){
						oo$annotations$copyright <- tclvalue(currentProjectEnv$annotations$copyright)
					} else {
						oo$annotations$copyright <- NULL
					}

					oo$annotations$instrument <- tclvalue(currentProjectEnv$annotations$instrument)
					oo$annotations$instrument_type <- tclvalue(currentProjectEnv$annotations$instrument_type)
					oo$annotations$confidence_comment <- tclvalue(currentProjectEnv$annotations$confidence_comment)
					oo$annotations$compound_class <- tclvalue(currentProjectEnv$annotations$compound_class)
					oo$annotations$internal_id_fieldname <- tclvalue(currentProjectEnv$annotations$internal_id_fieldname)
					oo$annotations$entry_prefix <- tclvalue(currentProjectEnv$annotations$entry_prefix)
					oo$annotations$ms_type <- tclvalue(currentProjectEnv$annotations$ms_type)
					oo$annotations$ionization <- tclvalue(currentProjectEnv$annotations$ionization)
					
					
					if(tclvalue(currentProjectEnv$annotations$lc_gradient) != ""){
						oo$annotations$lc_gradient <- tclvalue(currentProjectEnv$annotations$lc_gradient)
					} else {
						oo$annotations$lc_gradient <- NULL
					}
					
					if(tclvalue(currentProjectEnv$annotations$lc_flow) != ""){
						oo$annotations$lc_flow <- tclvalue(currentProjectEnv$annotations$lc_flow)	
					} else {
						oo$annotations$lc_flow <- NULL
					}
					
					if(tclvalue(currentProjectEnv$annotations$lc_solvent_a) != ""){
						oo$annotations$lc_solvent_a <- tclvalue(currentProjectEnv$annotations$lc_solvent_a)
					} else {
						oo$annotations$lc_solvent_a <- NULL
					}
					
					if(tclvalue(currentProjectEnv$annotations$lc_solvent_b) != ""){
						oo$annotations$lc_solvent_b <- tclvalue(currentProjectEnv$annotations$lc_solvent_b)
					} else {
						oo$annotations$lc_solvent_b <- NULL
					}
					
					if(tclvalue(currentProjectEnv$annotations$lc_column) != ""){
						oo$annotations$lc_column <- tclvalue(currentProjectEnv$annotations$lc_column)
					} else {
						oo$annotations$lc_column <- NULL
					}
				
					##Make those the current settings
					options("RMassBank" = oo)

					
					##Save the project
					saveCurrentProject()
					
					##Destroy the "create project" window 
					tkdestroy(newProj)
			} else {
				##Error message for missing specifications
				tkmessageBox(message = "There are some mandatory specifications (marked in red) missing", icon= "warning")
			}
		} else {
			##Error for entry prefix not having 2 letters
			tkmessageBox(message = "The entry prefix must be made up of exactly 2 letters", icon= "warning")
		}
	})
	
	#tkgrid(name.label, name.entry, sticky="ew")
	tkgrid(authors.label, authors.entry, sticky="ew")
	tkgrid(copyright.label, copyright.entry, sticky="ew")
	tkgrid(publication.label, publication.entry, sticky="ew")	
	tkgrid(license.label, license.entry, sticky="ew")	
	tkgrid(instrument.label, instrument.entry, sticky="ew")	
	tkgrid(instrument_type.label, instrument_type.entry, sticky="ew")
	tkgrid(confidence_comment.label, confidence_comment.entry, sticky="ew")
	tkgrid(compound_class.label, compound_class.entry, sticky="ew")	
	tkgrid(internal_id_fieldname.label, internal_id_fieldname.entry, sticky="ew")	
	tkgrid(entry_prefix.label, entry_prefix.entry, sticky="ew")	
	tkgrid(ms_type.label, ms_type.entry, sticky="ew")
	tkgrid(ionization.label, ionization.entry, sticky="ew")
	tkgrid(lc_gradient.label, lc_gradient.entry, sticky="ew")
	tkgrid(lc_flow.label, lc_flow.entry, sticky="ew")
	tkgrid(lc_solvent_a.label, lc_solvent_a.entry, sticky="ew")
	tkgrid(lc_solvent_b.label, lc_solvent_b.entry, sticky="ew")
	tkgrid(lc_column.label, lc_column.entry, sticky="ew")
	
	SEP <- ttkseparator(parent=newProj, orient="horizontal")
	tkgrid(SEP, columnspan=2, pady=c(5,5), sticky = "ew")
	tkgrid(import.but,submit.but)
	
	#tkgrid.configure(name.label, sticky="e")
	tkgrid.configure(authors.label, sticky="e")
	tkgrid.configure(copyright.label, sticky="e")
	tkgrid.configure(publication.label, sticky="e")
	tkgrid.configure(license.label, sticky="e")
	tkgrid.configure(instrument.label, sticky="e")
	tkgrid.configure(instrument_type.label, sticky="e")
	tkgrid.configure(confidence_comment.label, sticky="e")
	tkgrid.configure(compound_class.label, sticky="e")
	tkgrid.configure(internal_id_fieldname.label, sticky="e")
	tkgrid.configure(entry_prefix.label, sticky="e")
	tkgrid.configure(ms_type.label, sticky="e")
	tkgrid.configure(ionization.label, sticky="e")
	tkgrid.configure(lc_gradient.label, sticky="e")
	tkgrid.configure(lc_flow.label, sticky="e")
	tkgrid.configure(lc_solvent_a.label, sticky="e")
	tkgrid.configure(lc_solvent_b.label, sticky="e")
	tkgrid.configure(lc_column.label, sticky="e")
	
	tkgrid.columnconfigure(newProj, 0, weight = 1)
	tkgrid.columnconfigure(newProj, 1, weight = 3)
	
    #currentProjectEnv$annotations$ms_dataprocessing <- list()
	Sys.sleep(0.1)
	.Tcl(paste("wm resizable", .Tk.ID(newProj), 0, 0)) ##Block resizing
}

readCurrentProject <- function(){
	
	##RESET BEFORE READING THE PROJECT
	resetMain()
	resetList()
	
	##LIST THE FILES IN PROJECT FOLDER
	filesInProject <- list.files(file.path(currentProjectEnv$fileDir,currentProjectEnv$currentProject))
	if(length(filesInProject) > 0){
		csvs <- grep("csv", filesInProject)
		if(length(csvs) > 0){
			csv1 <- file.path(currentProjectEnv$fileDir,currentProjectEnv$currentProject,filesInProject[csvs[1]])
			FILE1 <- as.matrix(read.csv(csv1))
			if("SMILES" %in% colnames(FILE1)){
				tclvalue(WorkflowEnv$compoundList) <- file.path(currentProjectEnv$fileDir,currentProjectEnv$currentProject,filesInProject[csvs[1]])
				loadList(tclvalue(WorkflowEnv$compoundList))
			} else {
				for(i in 1:nrow(FILE1)){
					tkinsert(ObjectEnv$lboxfiles, "end", FILE1[i,"Files"])
					tkinsert(ObjectEnv$lboxcpdID, "end", FILE1[i,"ID"])
					WorkflowEnv$guifiles[i] <- FILE1[i,"Files"]
					WorkflowEnv$cpdID[i] <- FILE1[i,"ID"]
				}
			}
			
			if(length(csvs) == 2){
				csv2 <- file.path(currentProjectEnv$fileDir,currentProjectEnv$currentProject,filesInProject[csvs[2]])
				FILE2 <- as.matrix(read.csv(csv2))
				if("SMILES" %in% colnames(FILE2)){
					tclvalue(WorkflowEnv$compoundList) <- file.path(currentProjectEnv$fileDir,currentProjectEnv$currentProject,filesInProject[csvs[2]])
					loadList(tclvalue(WorkflowEnv$compoundList))
				} else {
					for(i in 1:nrow(FILE2)){
						tkinsert(ObjectEnv$lboxfiles, "end", FILE2[i,"Files"])
						tkinsert(ObjectEnv$lboxcpdID, "end", FILE2[i,"ID"])
						WorkflowEnv$guifiles[i] <- FILE2[i,"Files"]
						WorkflowEnv$cpdID[i] <- FILE2[i,"ID"]
					}
				}
			}
		}
		##Annotation
		ini <- grep("ini", filesInProject)
		if(length(ini) == 1){
			projectPath <- file.path(currentProjectEnv$fileDir,currentProjectEnv$currentProject,filesInProject[ini])
			importRmbSettings(projectPath)
		} else {
			tkmessageBox(message="Missing settings-file in project folder. Either use RMassBankGUI to create it or put it manually into the folder", icon="warning")
		}
	}
}

saveCurrentProject <- function(){
		
		##Find out directory path of project
		currentProjectDir <- file.path(currentProjectEnv$fileDir,currentProjectEnv$currentProject)
		
		##Save files and cpdIDs in filetable
		if(length(WorkflowEnv$guifiles) > 0){
			m <- matrix(c(sapply(WorkflowEnv$guifiles,normalizePath),WorkflowEnv$cpdID),length(WorkflowEnv$guifiles),2,
				dimnames=list(rep("",length(WorkflowEnv$guifiles)),c("Files","ID")))
			write.csv(m, file=file.path(currentProjectDir, "ftable.csv"), row.names = FALSE)
		}
		
		##Save settings
		fileConn <- file(file.path(currentProjectDir,"settings.ini"), open="w")
		writeLines(as.yaml(getOption("RMassBank")), fileConn)
		close(fileConn)
		
		if(tclvalue(WorkflowEnv$compoundList) != ""){
			if(dirname(tclvalue(WorkflowEnv$compoundList)) != currentProjectDir){
				file.copy(from=tclvalue(WorkflowEnv$compoundList), to=currentProjectDir, overwrite = TRUE)
			}
		}
}