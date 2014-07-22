bindDel <- function(){
	tkbind(ObjectEnv$lboxfiles, "<Delete>", function(...) {
		index <- tclvalue(tkcurselection(ObjectEnv$lboxfiles))
		if(nchar(index) > 1){
			index <- unlist(strsplit(index, " "))
		}
		index <- as.numeric(index)
		if(!is.na(index[1])){
			sapply(rev(index), function(x){tkdelete(ObjectEnv$lboxfiles,x)})
			sapply(rev(index), function(x){tkdelete(ObjectEnv$lboxcpdID,x)})
			for(i in rev(index)){
				WorkflowEnv$guifiles <- WorkflowEnv$guifiles[-(i+1)]
				WorkflowEnv$cpdID <- WorkflowEnv$cpdID[-(i+1)]
			}
		}
	})
	
	tkbind(ObjectEnv$lboxcpdID, "<Delete>", function(...) {
		index <- tclvalue(tkcurselection(ObjectEnv$lboxcpdID))
		if(nchar(index) > 1){
			index <- unlist(strsplit(index, " "))
		}
		index <- as.numeric(index)
		if(!is.na(index[1])){
			sapply(rev(index), function(x){tkdelete(ObjectEnv$lboxfiles,x)})
			sapply(rev(index), function(x){tkdelete(ObjectEnv$lboxcpdID,x)})
			for(i in rev(index)){
				WorkflowEnv$guifiles <- WorkflowEnv$guifiles[-(i+1)]
				WorkflowEnv$cpdID <- WorkflowEnv$cpdID[-(i+1)]
			}
		}
	})
}