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
	index = grep('ParameterValues.txt', dir(WorkDir))
	if(length(index) == 0) {
		stop('ParameterValues.txt missing from work directory')
	}
	paramvals = data.table::fread(paste0(WorkDir, '/','ParameterValues.txt'))
	numberofparams = nrow(unique(paramvals[, 1, with = FALSE]))
	runs = nrow(paramvals)/numberofparams
	# Get the right file in the workdirectory
	WorkDirContent = dir(WorkDir)
	BatIndex = grep('_01_', WorkDirContent)
	if(length(BatIndex) == 0) {
		stop('Bat file missing from work directory')
	}
	TheBat = paste0(WorkDir, '/', WorkDirContent[BatIndex])
	Bat = readLines(TheBat)
	if(length(Bat) == 0) {
		stop('Error reading bat file')
	}
	# Edit the line with the for loop
	TheForLine = paste0('FOR /L %%A IN (1,1,', runs, ') DO call _02')
	index = grep('FOR /L %%A IN', Bat)
	if(length(index) == 0) {
		stop('Cannot find the for loop in the bat file')
	}
	Bat[index] = TheForLine
	filecon = file(TheBat, open = 'wt')
	write(Bat, file = filecon)
	close(filecon)
}
