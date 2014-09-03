xcmsGUI <- function(){
	
	initXCMS()
	#scaleppm <- tkwidget(parent=tt2, type="ttk::scale", label= "Dinkidongdungbam", showvalue=TRUE, from = 0, to = 100, orient = "horizontal", variable = XCMSEnv$ppm)
	
	##Button to save and quit
	savebut <- ttkbutton(ObjectEnv$tt2, text = "Save and close", command = function(){
		tkdestroy(ObjectEnv$tt2)
	})
	
	
	tkgrid(ObjectEnv$ppmlabel, ObjectEnv$scaleppm)
	tkgrid(ObjectEnv$ppmentry, row=0, column=2, sticky = "w")
	tkgrid(ObjectEnv$pwslabel, ObjectEnv$scalepwstart)
	tkgrid(ObjectEnv$peakwidthstartentry, row=1, column=2, sticky = "w")
	tkgrid(ObjectEnv$pwelabel, ObjectEnv$scalepwend)
	tkgrid(ObjectEnv$peakwidthendentry, row=2, column=2, sticky = "w")
	tkgrid(ObjectEnv$sntlabel, ObjectEnv$scalesnthresh)
	tkgrid(ObjectEnv$snthreshentry, row=3, column=2, sticky = "w")
	tkgrid(ObjectEnv$pfplabel, ObjectEnv$scalepfpeaks)
	tkgrid(ObjectEnv$pfpeaksentry, row=4, column=2, sticky = "w")
	tkgrid(ObjectEnv$pfilabel, ObjectEnv$scalepfint)
	tkgrid(ObjectEnv$pfintentry, row=5, column=2, sticky = "w")
	tkgrid(ObjectEnv$mzdlabel, ObjectEnv$scalemzdiff)
	tkgrid(ObjectEnv$mzdiffentry, row=6, column=2, sticky = "w")
	tkgrid(ObjectEnv$itglabel, ObjectEnv$integratebutton)
	tkgrid(savebut, row=7, column=2, sticky = "w")
	Sys.sleep(0.1)
	.Tcl(paste("wm resizable", .Tk.ID(ObjectEnv$tt2), 0, 0))
}





# addSpecGUI <- function(tlwind){
	# tt5 <- tktoplevel(tlwind)
	# tkwm.title(tt5, "Add new spectrum")
	
	
	
	# mlabel <- ttklabel(parent = tt5, text="Mode")
	# CElabel <- ttklabel(parent = tt5, text="CE")
	# CESlabel <- ttklabel(parent = tt5, text="CES")
	# RESlabel <- ttklabel(parent = tt5, text="Res")
	
	# addb <- ttkbutton(parent=tt5, text="Add spectrum", command = function(...){
		
		# tkdestroy(tt5)
	# })
	
	# tkgrid(mlabel, CElabel, CESlabel, RESlabel, padx=c(0,0))
	
	# tkgrid(addb, row=2, column = 1, columnspan=2)
# }

