addFilesGUI <- function(newfiles){
	ObjectEnv$tt6 <- tktoplevel(ObjectEnv$tt)
	tkwm.title(ObjectEnv$tt6, "Specify cpdIDs for the files")
	ObjectEnv$labellist <- list()
	ObjectEnv$entrylist <- list()
	ObjectEnv$cpdIDlist <- list()
	for(i in 1:length(newfiles)){
		ObjectEnv$labellist[[i]] <- ttklabel(parent = ObjectEnv$tt6, text=basename(newfiles[i]))
		ObjectEnv$cpdIDlist[[i]] <- tclVar("")
		ObjectEnv$entrylist[[i]] <- ttkentry(parent = ObjectEnv$tt6, textvariable=ObjectEnv$cpdIDlist[[i]])
	} 
	
	tkgrid(ttklabel(parent = ObjectEnv$tt6, text="Name of the file"),ttklabel(parent = ObjectEnv$tt6, text="Assigned cpdID"))
	for(j in 1:length(newfiles)){
		tkgrid(ObjectEnv$labellist[[j]], ObjectEnv$entrylist[[j]])
	}
	
	lfiles <- length(WorkflowEnv$guifiles)
	
	addb <- ttkbutton(parent=ObjectEnv$tt6, text="Add these Files", command = function(...){
		if(all(sapply(ObjectEnv$cpdIDlist, function(x) (tclvalue(x) != "")))){
			if(all(sapply(ObjectEnv$cpdIDlist, function(x) !is.na(as.numeric(tclvalue(x)))))){
				for(i in 1:length(newfiles)){
					
					WorkflowEnv$cpdID[lfiles + i] <- as.numeric(tclvalue(ObjectEnv$cpdIDlist[[i]]))
					tkinsert(ObjectEnv$lboxcpdID,"end",as.numeric(tclvalue(ObjectEnv$cpdIDlist[[i]])))
					WorkflowEnv$guifiles[lfiles + i] <- newfiles[i]
					tkinsert(ObjectEnv$lboxfiles,"end",basename(newfiles[i]))
				}
				tkdestroy(ObjectEnv$tt6)
			} else {
				tkmessageBox(message="cpdIDs can't contain characters.",icon="warning")
			}
		} else {
			tkmessageBox(message="Please specify all cpdIDs.",icon="warning")
		}
	})
	
	inferb <- ttkbutton(parent=ObjectEnv$tt6, text="Infer cpdIDs automatically...", command = function(...){
		inferredIDs <- sapply(newfiles,function(x){
			strsplit(x,"_")[[1]][2]
		})
		for(i in 1:length(newfiles)){
			tclvalue(ObjectEnv$cpdIDlist[[i]]) <- inferredIDs[i]
		}
	})
	

	tkgrid(addb,inferb)
}