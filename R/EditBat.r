#' Edit bat file
#'
#' Edit a bat file to reflect the correct number of runs in a scenario
#'
#' @param WorkDir character Path to the work directory with the ALMaSS exe file
#' @export
EditBat = function(WorkDir = NULL) {
	if(is.null(WorkDir)) 
	{
		stop('Input parameter WorkDir missing')
	}
	# Get the number of runs:
	paramvals = fread(paste0(WorkDir, '/','ParameterValues.txt'))
	numberofparams = nrow(unique(paramvals[, 1, with = FALSE]))
	runs = nrow(paramvals)/numberofparams
	# Get the right file in the workdirectory
	WorkDirContent = dir(WorkDir)
	BatIndex = grep('_01_', WorkDirContent)
	TheBat = paste0(WorkDir, '/', WorkDirContent[BatIndex])
	Bat = readLines(TheBat)
	# Edit the line with the for loop
	TheForLine = paste0('FOR /L %%A IN (1,1,', runs, ') DO call _02')
	Bat[grep('FOR /L %%A IN', Bat)] = TheForLine
	filecon = file(TheBat, open = 'wt')
	write(Bat, file = filecon)
	close(filecon)
}
