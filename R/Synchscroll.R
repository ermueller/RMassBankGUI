pseudoSetScroll <- function(scroll, ...){
	.Tcl(paste("setScroll", .Tk.ID(scroll), ...))
}
	
pseudoSynchScroll <- function(widgets, ...){
	.Tcl(paste0("synchScroll {",do.call(paste,lapply(widgets, .Tk.ID)),"} yview ", do.call(paste,list(...))))
}
	
Scrolls <- function(){
	.Tcl('proc setScroll {s args} {
		$s set {*}$args
		{*}[$s cget -command] moveto [lindex [$s get] 0]
	}')
	.Tcl('proc synchScroll {widgets args} {
		foreach w $widgets {$w {*}$args}
	}')
	.Tcl('proc resolution {res t val var} {
		set factor [expr 1 / $res]
		set val [expr int($val * $factor) / $factor]
		set $var $val
		return $val
	}')
}

checkRMBsettingsGUI <- function(){
	o <- getOption("RMassBank", NULL)
	if(is.null(o)){
		tkmessageBox(message="No options were specified. Please supply a file or edit the options (Settings -> Edit RMassBank Settings")
		return(FALSE)
	} else{return(TRUE)}
		
}

changeSettings <- function(){
	if(tclvalue(tkmessageBox(type="okcancel", message="Loading these settings will overwrite the current settings! Do you want to load new settings?", icon = "warning")) == "ok"){
		return(TRUE)
	} else{return(FALSE)}
}