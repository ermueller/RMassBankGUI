pseudoSetScroll <- function(scroll, ...){
	.Tcl(paste("setScroll", .Tk.ID(scroll), ...))
}
	
pseudoSynchScroll <- function(widgets, ...){
	.Tcl(paste0("synchScroll {",do.call(paste,lapply(widgets, .Tk.ID)),"} yview ", do.call(paste,list(...))))
}
	
