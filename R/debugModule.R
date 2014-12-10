debugEnv <- new.env()

initDebugEnv <- function(){
	debugEnv$workflowVar <-  tclVar("w")
	debugEnv$on <- tclVar("0")
	debugEnv$oldVar <- tclVar("")
}

debugGUI <- function(tlwindow){
	##Window
	tt5 <- tktoplevel(tlwindow)
	tkwm.title(tt5,"Debug options")
	
	##
	#tkmenu()
	
	debugEnv$varEntry <- ttkentry(parent=tt5, textvariable=debugEnv$workflowVar, width = 6)
	debugEnv$varLabel <- ttklabel(parent=tt5, text="Name of the workspace-variable in the global environment")
	
	debugEnv$debugButton <- ttkcheckbutton(parent = tt5, variable = debugEnv$on, onvalue = 1, offvalue = 0, 
	command=function(x, ...){
		if(tclvalue(debugEnv$on) == 0){
			message("Debug mode off")
			message(paste("Debug variable", tclvalue(debugEnv$oldVar), "has been removed"))
			rm(list = tclvalue(debugEnv$oldVar), envir=.GlobalEnv)
		} else {
			message("Debug mode on")
			message(paste("Debug variable is", tclvalue(debugEnv$workflowVar)))
			tclvalue(debugEnv$oldVar) <- tclvalue(debugEnv$workflowVar)
		}
	})
	debugEnv$buttonLabel <- ttklabel(parent=tt5, text="Enable debug mode")
	debugEnv$readButton <- ttkbutton(parent=tt5, text="Read variable", command=function(){
		yesno <- tkmessageBox(icon = "warning" , message = "Do you want to read the changed Workspace? This will overwrite the internal workspace and can have unforeseen effects!", title = "Warning", type = "yesno")
		
		if(tclvalue(yesnocancel) == "yes"){
			eval(parse(text=paste0("WorkflowEnv$wSpace <- ",".GlobalEnv$",tclvalue(debugEnv$oldVar))))
		}
		
	})
	tkgrid(debugEnv$varLabel, debugEnv$varEntry)
	tkgrid(debugEnv$buttonLabel, debugEnv$debugButton)
	tkgrid(debugEnv$readButton)
}