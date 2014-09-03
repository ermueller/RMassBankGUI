RTGUI <- function(tlwindow){
	
	##Windows
	tt3 <- tktoplevel(tlwindow)
	tkwm.title(tt3,"Edit RT Margin/Shift (mzR) ")
	oo <- getOption("RMassBank")
	
	##Retention time
	RTEnv <- new.env()
	RTEnv$margin <- tclVar(oo$rtMargin)
	RTEnv$shift <- tclVar(oo$rtShift)
	
	RTDUMMYEnv <- new.env()
	RTDUMMYEnv$margin <- tclVar(as.numeric(oo$rtMargin)*10)
	RTDUMMYEnv$shift <- tclVar(as.numeric(oo$rtShift)*10)
	
	##Scales to set the values
	scaleRTM <- tkwidget(parent=tt3, type="ttk::scale", from = 0, to = 10, variable = RTDUMMYEnv$margin, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(RTEnv$margin) <- (floor(as.numeric(...)) / 10)
			oo <- getOption("RMassBank")
			oo$rtMargin <- (floor(as.numeric(...)) / 10)
			options("RMassBank" = oo)
		})
	scaleRTS <- tkwidget(parent=tt3, type="ttk::scale", from = -100, to = 100, variable = RTDUMMYEnv$shift, orient = "horizontal", 
		command=function(x, ...){
			tclvalue(RTEnv$shift) <- (floor(as.numeric(...)) / 10)
			oo <- getOption("RMassBank")
			oo$rtShift <- (floor(as.numeric(...)) / 10)
			options("RMassBank" = oo)
		})
	
	##Entries
	marentry <- ttkentry(parent=tt3, textvariable=RTEnv$margin, width=6)
	shientry <- ttkentry(parent=tt3, textvariable=RTEnv$shift, width=6)
	
	##Labels
	marlabel <- ttklabel(parent = tt3, text="Deviation allowed for retention time (m)")
	shilabel <- ttklabel(parent = tt3, text="Systematic retention time shift (m)")
	
	##Close Button
	savebut <- ttkbutton(tt3, text = "Save and close", command = function(){
		tkdestroy(tt3)
	})
	
	##Grid
	tkgrid(marlabel, scaleRTM)
	tkgrid(marentry, row=0, column=2, sticky = "w")
	tkgrid(shilabel, scaleRTS)
	tkgrid(shientry, row=1, column=2, sticky = "w")
	tkgrid(savebut, row=2, column = 2)
	Sys.sleep(0.1)
	.Tcl(paste("wm resizable", .Tk.ID(tt3), 0, 0))
}