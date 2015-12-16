#' Edit ini file
#'
#' Edit the TI_inifile to reflect the species and output probes of choise
#'
#' @param WorkDir character Path to the work directory with the TI_inifile.
#' @param Model character The model to run.
#' @param StandardProbes logical Run all the standard output probes?
#' @param Type character Edit BatchALMaSS.ini file (cmd, default) or
#' the TI_inifile.ini (gui).
#' @param NYear numeric The number of years to run.
#' @export
EditIni = function(WorkDir = NULL, Model = NULL, StandardProbes = TRUE, 
	Type = 'cmd', NYear = 1) {
	if(all(is.null(WorkDir), is.null(Model))) 
	{
		stop('Input parameter WorkDir or Model missing')
	}
	if(StandardProbes)
	{
		cat(paste0('Running all standard probes for the ', Model, ' model', '\n',
			'Number of years:', NYear, '\n'))
	}
	model = ModelNumber(Model)
	if(model < 0){
		stop('Invalid input to argument Model')
	}
	if(tolower(Type) == 'gui'){
		file = paste0(WorkDir, '/','TI_inifile.ini')
	}
	file = paste0(WorkDir, '/','BatchALMaSS.ini')
	if(model == 6 & StandardProbes)
	{
		content = c('6',
		'Probe_ob0.prb',
		'Probe_ob1.prb',
		'Probe_ob2.prb',
		'Probe_ob3.prb',
		'Probe_ob4.prb',
		'Probe_ob5.prb',
		'.\\',
		 paste(NYear),
		paste(model))
	}	
	filecon = file(file, open = 'wt')
	write(content, file = filecon)
	close(filecon)
}
# Helper function ---------------------
ModelNumber = function(name = NULL) {
	if(is.null(name)) {stop('Input argument missing \n')}
	name = stringr::str_trim(name, side = "both")
	name = tolower(name)
	switch(EXPR = name,
  	# Names of the paramters:
		'skylark' = 0,
		'vole' = 1,
		'spider' = 2,
		'beetle' = 3,
		'bembidion' = 3,
		'hare' = 4,
		'partridge' = 5,
		'goose' = 6,
		'marshfritillary' = 7,
		'dormouse' = 8,
		'roedeer' = 9,
		'rabbit' = 10,
		-1
		)
}
