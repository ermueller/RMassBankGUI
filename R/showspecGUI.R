specGUIEnv <- new.env()

showspecGUI <- function(){
	tclRequire("Tktable")
	
	##Number
	tNumber <- 1
	
	##Window
	specWindow <- tktoplevel(ObjectEnv$tt)
	tkwm.title(specWindow,"Status")
	tkgrid.propagate(specWindow,1)
	
	##Index of the spectra that were found OK
	OKindex <- which(sapply(WorkflowEnv$wSpace@specs, function(x) return(x$foundOK)))
	
	##Lists of variables
	A <- list() ##Tables
	specGUIEnv$SBars <- list() ##Scrollbars
	Names <- list() ##Headers of the tables
	
	j <- 1
	opt <- getOption("RMassBank", NULL)
	
	for(i in OKindex){
		A[[j]] <- list()
		specGUIEnv$SBars[[j]] <- list()
		Names[[j]] <- list()
		nameStart <- findName(WorkflowEnv$wSpace@specs[[i]]$id)
		for(k in 1:length(WorkflowEnv$wSpace@specs[[i]]$peaks)){
			Names[[j]][[k]] <- paste0(nameStart,"_",opt$spectraList[[k]]$mode,"_",opt$spectraList[[k]]$ce)
			specGUIEnv$SBars[[j]][[k]] <- ttkscrollbar(specWindow, orient = "vertical", command = function(...) tkyview(A[[j]][[k]],...))
			A[[j]][[k]] <- myTList(specWindow, WorkflowEnv$wSpace@specs[[i]]$peaks[[k]], j, k)
		}
		j <- j + 1
	}
	
	j <- 1
	k <- 1
	tkgrid(A[[j]][[k]], columnspan = 2)
	tkgrid(specGUIEnv$SBars[[j]][[k]], column = 2, row = 0, sticky="ns")
	tkwm.title(specWindow,Names[[j]][[k]])
	
	nfiles <- length(WorkflowEnv$wSpace@specs)
	nspec <- length(WorkflowEnv$wSpace@specs[[1]]$peaks)
	
	##Buttons for looking at different specs
	left.but <- ttkbutton(parent = specWindow, text="<", width="3", command = function(){
		tkgrid.remove(A[[j]][[k]])
		tkgrid.remove(specGUIEnv$SBars[[j]][[k]])

		k <<- (k - 2) %% nspec + 1
		if(k == nspec){
			j <<- (j - 2) %% nfiles + 1
		}

		tkgrid(A[[j]][[k]], row = 0, column = 0, columnspan = 2)
		tkgrid(specGUIEnv$SBars[[j]][[k]], column = 2, row = 0, sticky="ns")
		tkwm.title(specWindow,Names[[j]][[k]])
	})
	
	right.but <- ttkbutton(parent = specWindow, text=">", width="3", command = function(){
		tkgrid.remove(A[[j]][[k]])
		tkgrid.remove(specGUIEnv$SBars[[j]][[k]])
		
		k <<- k %% nspec + 1
		if(k == 1){
			j <<- j %% nfiles + 1
		}
		
		tkgrid(A[[j]][[k]], row = 0, column = 0, columnspan = 2)
		tkgrid(specGUIEnv$SBars[[j]][[k]], column = 2, row = 0, sticky="ns")
		tkwm.title(specWindow,Names[[j]][[k]])
	})
	
	
	##Cosmetic stuff

	Separator <- ttkseparator(parent=specWindow, orient="horizontal")

	tkgrid(Separator, columnspan=3, pady=c(5,5), sticky = "ew")
	tkgrid(left.but, sticky="w")
	tkgrid(right.but, sticky="e", row=2, columnspan = 3)
	
}

myTList <- function(parent, ptable, j, k){
		w <- tkwidget(parent, "tablelist::tablelist", columntitles = c("m/z", "int"),background="white",  
						stretch="all", expand=1, width=40,
						yscrollcommand = function(...)tkset(specGUIEnv$SBars[[j]][[k]], ...))
		ptable[1:nrow(ptable),1] <- round(as.numeric(ptable[1:nrow(ptable),1]),5)
		ptable[1:nrow(ptable),2] <- round(as.numeric(ptable[1:nrow(ptable),2]),1)
		for(currRow in 1:nrow(ptable)){
			tkinsert(w, "end", c(ptable[currRow,1],ptable[currRow,2]))
		}
		return(w)
}