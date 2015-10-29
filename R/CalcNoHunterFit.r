#' Calculate the fit to the number of hunters per farm
#'
#' This function is only used to preform the test on scenarios already run.
#' The batchr.r scripts runs the test on the fly. Added to ralmass just to 
#' keep track of things. Will probably be removed further down.
#'
#' @param respath character Path to the result files
#' @param method character Method to calculate fit. Either 'overlap' or 
#' 'leastsq'.
#' @return numeric The overlap
#' @export
CalcNoHunterFit = function(respath = NULL, method = NULL) {
	if(is.null(method)){
		stop('Method not specified')
	}
	if(is.null(respath)){
		stop('respath not specified')
	}
	survey = data.table::fread('e:/almass/WorkDirectories/Hunter/HunterSurveyResultsFarm.csv')
	files = dir(respath)[grep('Farm', dir(respath))]
	res = data.table::fread(paste0(respath, 'ParameterFittingResults.txt'))
	res[, NoHunterFit:=0]
	numberofparams = nrow(unique(res[, 1, with = FALSE])) # The number of paramters being modified per run 
	runs = nrow(res)/numberofparams
	lineno = seq(1, runs*numberofparams, numberofparams)
	# Do the stuff:
	for (j in seq_along(files)) 
	{
		farms = data.table::fread(paste0(respath, files[j]))
		if(method == 'overlap'){
			farms[, Type:='Simulated']
			simulated = farms[NoHunters > 0, .(NoHunters, Type)]
			setnames(simulated, old = 'NoHunters', new = 'Numbers')
			no.hunters = rbind(survey, simulated)
			no.huntersFit = round(CalcOverlab(no.hunters, species = 'Hunter'), 3)
		}
		if(method == 'leastsq'){
			# Commented out as it threw an error. Values hardcoded in further down
			# survey[, N:=.N, by = 'Numbers']
			# survey = unique(survey)
			# setnames(survey, old = 'Numbers', new = 'Bin')
			# bins = data.table('N' = c(0,0), 'Type' = rep('Fieldobs',2), 'Bin' = c(8, 12))
			# survey = rbind(survey, bins)
			# setkey(survey, Bin)
			# # The simulation results needs to be binned manually and added to the survey data:
			survey = data.table('Bin' = 1:12, 'N' = c(16,12,12,9,15,10,7,7,0,1,3,0))
			for (i in 1:11) 
			{
				survey[Bin == i, ModelRes:= length(farms[NoHunters == i, NoHunters])]
			}
			survey[Bin == 12, ModelRes:= length(farms[NoHunters >= 12, NoHunters])]
			# Calculate the proportions
			survey[, propSim:=ModelRes/sum(ModelRes)]
			survey[, propSur:=N/sum(N)]
			# Calculate fit
			no.huntersFit = with(survey, 1-sum((propSim-propSur)^2))
		}
		res[lineno[j], NoHunterFit:=no.huntersFit]
		res[lineno[j], OverallFit:=OverallFit + no.huntersFit]
	}
	write.table(res, file = paste0(respath, 'ParameterFittingResultsC.txt'), row.names = FALSE, sep = '\t')
}

