#' Append work directory paths
#'
#' Append work directory paths to already excisting scripts
#'
#' @param WorkDir character Path to the work directory with the ALMaSS exe file
#' @param InScript character Path to the base script into which the WD is appended
#' @param OutScript character File name for the modified base script. This will
#' usually be the same name as the base script. 
#' @export

AppendWorkDir = function(WorkDir = NULL, InScript = NULL, OutScript = NULL) {
	if(any(is.null(WorkDir), is.null(InScript), is.null(OutScript)))
	{
		stop('Input parameter missing')
	}
	Script = readLines(InScript)
	OutputScript = paste0(WorkDir, '/', OutScript)	
	TheWd = paste0('setwd(','\'', WorkDir, '\'', ')')
	Script = append(Script, TheWd, grep('# Setup work directory', Script))
	filecon = file(OutputScript, open = 'wt')
	write(Script, file = filecon)
	close(filecon)
}