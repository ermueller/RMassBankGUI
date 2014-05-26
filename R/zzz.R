.onLoad <- function(libname, pkgname) {
	.Tcl('proc setScroll {s args} {
		$s set {*}$args
		{*}[$s cget -command] moveto [lindex [$s get] 0]
	}')
	.Tcl('proc synchScroll {widgets args} {
		foreach w $widgets {$w {*}$args}
	}')
	WorkflowGUI()
}