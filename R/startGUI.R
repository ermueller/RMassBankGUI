startEnv <- new.env()
		
startGUI <- function(){

		##Starting prompt: Check if a filedir is currently used.
		if(is.na(currentProjectEnv$fileDir)){
			configfile <- system.file(file = "config/config.txt",package="RMassBankGUI")
			fileConn <- file(configfile)
			projectDir <- readLines(fileConn)
			if(length(projectDir) > 0){
				currentProjectEnv$fileDir <- projectDir
				close(fileConn)
			} else {
				tkmessageBox(message = "Welcome to RMassBankGUI!\n Please choose a folder that you want to use to save your projects.")
				currentProjectEnv$fileDir <- tk_choose.dir()
				if(is.na(currentProjectEnv$fileDir)){
					close(fileConn)
					return(0)
				}
				#writeLines(currentProjectEnv$fileDir,fileConn)
				close(fileConn)
			}
		}
		
		##NAMES OF ALREADY EXISTANT PROJECTS
		startEnv$projects <- list.files(currentProjectEnv$fileDir)
		
		##WINDOW
		startEnv$startWindow <- tktoplevel()  ##Create window
		tkwm.title(startEnv$startWindow,"RMassBank GUI Start Window") ##Window title
		tkgrid.propagate(startEnv$startWindow,1)
		
		##LISTBOX WITH SCROLLBAR
		startEnv$ybar <- ttkscrollbar(startEnv$startWindow, command = function(...)tkyview(startEnv$lboxProjects,...))
		startEnv$lboxProjects <- tklistbox(startEnv$startWindow, width = 50, borderwidth = 0 , height = 10, selectmode = "single", 
						yscrollcommand = function(...) tkset(startEnv$ybar, ...), 
						background = "white")
		
		##LIST THE PROJECTS IN THE PROJECTDIR
		for(i in startEnv$projects){
			tkinsert(startEnv$lboxProjects,"end",i)
		}
		
		##CREATE PROJECT BUTTON
		startEnv$createProject.but <- ttkbutton(parent=startEnv$startWindow, text="Create new project", command = function(){
			createNewProjectGUI()
		})
		
		##CHOOSE PROJECT BUTTON
		startEnv$chooseProject.but <- ttkbutton(parent=startEnv$startWindow, text="Choose selected project", command = function(){
			
			number <- tclvalue(tkcurselection(startEnv$lboxProjects))
			if(number != ""){
				number <- as.numeric(number) + 1
				currentProjectEnv$currentProject <- startEnv$projects[number]
				WorkflowGUI()
				readCurrentProject()
				tkdestroy(startEnv$startWindow)
			} else {
				tkmessageBox(message = "No project is currently selected")
			}
			
		})
		
		SEP <- ttkseparator(parent=startEnv$startWindow, orient="horizontal")
		
		##GRID LAYOUT
		tkgrid(startEnv$lboxProjects, columnspan = 3, sticky="ew")
		tkgrid(SEP, columnspan=4, pady=c(5,5), sticky = "ew")
		tkgrid(startEnv$createProject.but, startEnv$chooseProject.but, pady=c(5,0))
		tkgrid(startEnv$ybar, sticky = "nws", column = 3, row = 0)
		
		##Configuring grid
		tkgrid.configure(startEnv$lboxProjects, sticky="e")
		tkgrid.columnconfigure(startEnv$startWindow, 0, weight = 1)
		tkgrid.columnconfigure(startEnv$startWindow, 1, weight = 1)
		tkgrid.columnconfigure(startEnv$startWindow, 2, weight = 1)
		tkgrid.columnconfigure(startEnv$startWindow, 3, weight = 1)
		Sys.sleep(0.1)
		.Tcl(paste("wm resizable", .Tk.ID(startEnv$startWindow), 0, 0))
}