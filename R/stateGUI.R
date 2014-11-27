stateEnv <- new.env()

##################
##Canvas functions
##################

tkcanvas <- function(parent, ...) tkwidget(parent, "canvas", ...)

tkcreateRect <- function(canvas, x1, x2, y1, y2, outline="000", fill="000") .Tcl(paste(.Tk.ID(canvas), "create rect", 
x1, x2, y1, y2, "-outline", paste0("#",outline), "-fill", paste0("#",fill)))

tkGreenRect <- function(canvas){
	tkcreateRect(canvas, 0, 0, 80, 20, "000", "0F3")
}

tkRedRect <- function(canvas){
	tkcreateRect(canvas, 0, 0, 80, 20, "000", "F00")
}

##############
##GUI function
##############
stateGUI <- function(tlwindow) {
	
	##Window
	stateEnv$stateWindow <- tktoplevel(tlwindow)
	tkwm.title(stateEnv$stateWindow,"Status")
	tkgrid.propagate(stateEnv$stateWindow,1)
	
	##Create canvas
	stateEnv$canvasList <- lapply(1:8, function(i) tkcanvas(stateEnv$stateWindow, height=20, width=80, selectborderwidth=5))
	
	##Check completed workspace steps
	tkWorkSpaceCheck(WorkflowEnv$wSpace)
	
	##Create labels
	stepLabels <- lapply(1:8, function(i) ttklabel(stateEnv$stateWindow, text=paste("Step",i)))
	
	for(i in 1:8) tkgrid(stepLabels[[i]], stateEnv$canvasList[[i]], padx=c(0,0))
	
	a.but <- ttkbutton(stateEnv$stateWindow, text = "Show spectra", command = showspecGUI, width=15)
	
	tkgrid(a.but,row=0,column=3)
	
	.Tcl(paste("wm resizable", .Tk.ID(stateEnv$stateWindow), 0, 0))
}



tkWorkSpaceCheck <- function(wSpace){
	
	##Create Canvas
	
	if(length(wSpace@specs)){
		tkGreenRect(stateEnv$canvasList[[1]])
	} else{ tkRedRect(stateEnv$canvasList[[1]])}
	
	if(length(wSpace@analyzedSpecs)){
		tkGreenRect(stateEnv$canvasList[[2]])
	} else{ tkRedRect(stateEnv$canvasList[[2]])}
	
	if(length(wSpace@aggregatedSpecs)){
		tkGreenRect(stateEnv$canvasList[[3]])
	} else{ tkRedRect(stateEnv$canvasList[[3]])}
	
	if(length(wSpace@recalibratedSpecs)){
		tkGreenRect(stateEnv$canvasList[[4]])
	} else{ tkRedRect(stateEnv$canvasList[[4]])}
	
	if(length(wSpace@analyzedRcSpecs)){
		tkGreenRect(stateEnv$canvasList[[5]])
	} else{ tkRedRect(stateEnv$canvasList[[5]])}
	
	if(length(wSpace@aggregatedSpecs)){
		tkGreenRect(stateEnv$canvasList[[6]])
	} else{ tkRedRect(stateEnv$canvasList[[6]])}
	
	if(length(wSpace@reanalyzedRcSpecs)){
		tkGreenRect(stateEnv$canvasList[[7]])
	} else{ tkRedRect(stateEnv$canvasList[[7]])}
	
	if(length(wSpace@refilteredRcSpecs)){
		tkGreenRect(stateEnv$canvasList[[8]])
	} else{ tkRedRect(stateEnv$canvasList[[8]])}
}

###############
###Progressbars
###############

progressBarRMBGUI <- function(object = NULL, value = 0, min = 0, max = 100, close = FALSE)
{		
		if(is.null(object))
		{
			object <- splitnum(80,max)
		} else {
			if(value != 0){
				if(WorkflowEnv$currentStep != 7){
					tkcreateRect(stateEnv$canvasList[[WorkflowEnv$currentStep]],  object[value], 0, object[value+1], 20, "000", "0F3")
				} else{
					tkcreateRect(stateEnv$canvasList[[WorkflowEnv$currentStep]],  object[value], 0, object[value+1], 20, "0F3", "0F3")
				}
				#.Tcl(paste("tkwait visibility",.Tk.ID(stateEnv$stateWindow)))
			}
		}	
	return(object)
}

splitnum <- function(num, div){
	res <- vector()
	res <- rep((num+1) %/% div, div)
	
	modres <- (num+1) %% div
	if(modres){
		for(i in 1:modres){
			res[i] <- res[i] + 1
		}
	}
	
	#res[1] <- 0
	for(i in div:2){
		res[i] <- sum(res[1:(i)])
	}
	
	
	return(c(0,res-1))
}