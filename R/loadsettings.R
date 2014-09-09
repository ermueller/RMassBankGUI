importRmbSettings <- function(fpath){
	
	oo <- yaml.load_file("RMassBankProjects/QEX/settings.ini")
	if(is.null(oo$deprofile)){
		oo$deprofile <- NA
	}
	if(is.null(o$babeldir)){
		oo$babeldir <- NA
	}
	options("RMassBank" = oo)
	
	ox <- oo$xcms
	tclvalue(XCMSEnv$ppm) <- ox$ppm
	tclvalue(XCMSEnv$peakwidthstart) <- ox$peakwidth[1]
	tclvalue(XCMSEnv$peakwidthend) <- ox$peakwidth[2]
	tclvalue(XCMSEnv$snthresh) <- ox$snthresh
	tclvalue(XCMSEnv$prefilterpeaks) <- ox$prefilter[1]
	tclvalue(XCMSEnv$prefilterintensity) <- ox$prefilter[2]
	tclvalue(XCMSEnv$method) <- ox$method
	tclvalue(XCMSEnv$mzCenterFun) <- ox$mzCenterFun
	tclvalue(XCMSEnv$integrate) <- ox$integrate
	tclvalue(XCMSEnv$mzdiff) <- ox$mzdiff
	tclvalue(XCMSEnv$fitGauss) <- ox$fitGauss
	if(is.na(oo$deprofile)){
		tclvalue(WorkflowEnv$dummyradio) <- "NA"
	} else{
		tclvalue(WorkflowEnv$dummyradio) <- oo$deprofile
	}
}

defaultSettings <- function(){
	
	##Default settings
		RmbDefaultSettings()
	
	##Load these settings
		oo <- getOption("RMassBank")
		oo$spectraList <- NULL
		oo$annotations$authors <- tclvalue(currentProjectEnv$annotations$authors)
		oo$annotations$license <- tclvalue(currentProjectEnv$annotations$license)
			
		if(tclvalue(currentProjectEnv$annotations$publication) != ""){
			oo$annotations$publication <- tclvalue(currentProjectEnv$annotations$publication)
		} else {
			oo$annotations$publication <- NULL
		}

		if(tclvalue(currentProjectEnv$annotations$copyright) != ""){
			oo$annotations$copyright <- tclvalue(currentProjectEnv$annotations$copyright)
		} else {
			oo$annotations$copyright <- NULL
		}

		oo$annotations$instrument <- tclvalue(currentProjectEnv$annotations$instrument)
		oo$annotations$instrument_type <- tclvalue(currentProjectEnv$annotations$instrument_type)
		oo$annotations$confidence_comment <- tclvalue(currentProjectEnv$annotations$confidence_comment)
		oo$annotations$compound_class <- tclvalue(currentProjectEnv$annotations$compound_class)
		oo$annotations$internal_id_fieldname <- tclvalue(currentProjectEnv$annotations$internal_id_fieldname)
		oo$annotations$entry_prefix <- tclvalue(currentProjectEnv$annotations$entry_prefix)
		oo$annotations$ms_type <- tclvalue(currentProjectEnv$annotations$ms_type)
		oo$annotations$ionization <- tclvalue(currentProjectEnv$annotations$ionization)
		
		
		if(tclvalue(currentProjectEnv$annotations$lc_gradient) != ""){
			oo$annotations$lc_gradient <- tclvalue(currentProjectEnv$annotations$lc_gradient)
		} else {
			oo$annotations$lc_gradient <- NULL
		}
		
		if(tclvalue(currentProjectEnv$annotations$lc_flow) != ""){
			oo$annotations$lc_flow <- tclvalue(currentProjectEnv$annotations$lc_flow)	
		} else {
			oo$annotations$lc_flow <- NULL
		}
		
		if(tclvalue(currentProjectEnv$annotations$lc_solvent_a) != ""){
			oo$annotations$lc_solvent_a <- tclvalue(currentProjectEnv$annotations$lc_solvent_a)
		} else {
			oo$annotations$lc_solvent_a <- NULL
		}
		
		if(tclvalue(currentProjectEnv$annotations$lc_solvent_b) != ""){
			oo$annotations$lc_solvent_b <- tclvalue(currentProjectEnv$annotations$lc_solvent_b)
		} else {
			oo$annotations$lc_solvent_b <- NULL
		}
		
		if(tclvalue(currentProjectEnv$annotations$lc_column) != ""){
			oo$annotations$lc_column <- tclvalue(currentProjectEnv$annotations$lc_column)
		} else {
			oo$annotations$lc_column <- NULL
		}
	
	##Make those the current settings
		options("RMassBank" = oo)
}