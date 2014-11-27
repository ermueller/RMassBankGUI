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
	
	for(i in OKindex){
		A[[j]] <- list()
		specGUIEnv$SBars[[j]] <- list()
		Names[[j]] <- list()
		nameStart <- findName(WorkflowEnv$wSpace@specs[[i]]$id)
		for(k in 1:length(WorkflowEnv$wSpace@specs[[i]]$peaks)){
			Names[[j]][[k]] <- paste0(nameStart,"_",k)
			specGUIEnv$SBars[[j]][[k]] <- ttkscrollbar(specWindow, orient = "vertical", command = function(...) tkyview(A[[j]][[k]],...))
			A[[j]][[k]] <- specToTkTable(specWindow, WorkflowEnv$wSpace@specs[[i]]$peaks[[k]], Names[[j]][[k]], j, k)
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
		print(k)
		print(k %% nspec + 1)
		k <<- k %% nspec + 1
		print(k)
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

specToTkTable <- function(tt, ptable, name, j, k){
	
	newptable <- rbind(colnames(ptable),ptable)
	newptable[2:nrow(newptable),1] <- round(as.numeric(newptable[2:nrow(newptable),1]),5)
	newptable[2:nrow(newptable),2] <- round(as.numeric(newptable[2:nrow(newptable),2]),1)
	
	for(i in (0:(nrow(newptable) - 1)))
		for(m in (0:(ncol(newptable) - 1)))
			.Tcl(paste0("set ", name, "(",i,",",m,") ", newptable[i + 1, m + 1]))
	
	rettable1 <- tkwidget(tt,"table",variable=name, rows=nrow(newptable),cols=ncol(newptable),titlerows="1",selectmode="browse",colwidth="25",state="disabled", background="white",
							yscrollcommand = function(...)tkset(specGUIEnv$SBars[[j]][[k]], ...))
	return(rettable1)
}