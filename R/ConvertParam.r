#' Convert ALMaSS config parameter to a meaningful character string
#'
#' Convert ALMaSS config parameter to a meaningful character string. This
#' can be useful when doing plots and generating labels and titles 
#' automatically.
#' 
#' @param param character The input parameter
#' @return character String with more meaningful explanation
#' @export
ConvertParam <- function(param = NULL) {
	if(is.null(param)) {stop('Input argument missing \n')}
    param = stringr::str_trim(param, side = "both")
    param = stringr::word(param)
    switch(EXPR = param,
  	# Names of the paramters:
  		'GOOSE_MINFORAGEOPENNESS' = 'Minimum openness',
  		'GOOSE_AFTERDARKTIME' = 'Past sunset active time',
        'BGOOSE_FOLLOWINGLIKELYHOOD' = 'Following likelyhood',
		'PFGOOSE_FOLLOWINGLIKELYHOOD' = 'Following likelyhood',
		'GLGOOSE_FOLLOWINGLIKELYHOOD' = 'Following likelyhood',
		'GOOSE_MAXAPPETITESCALER' = 'Max appetite scaler',
		'GOOSE_MAXENERGYRESERVEPROPORTION' = 'Max energy reserve proportion',
		'GOOSE_LEAVINGTHRESHOLD' = 'Leaving threshold',
		'GOOSE_FORAGEDIST_BN' = 'Max foraging distance',
		'GOOSE_FORAGEDIST_PF' = 'Max foraging distance',
		'GOOSE_FORAGEDIST_GL' = 'Max foraging distance',
		'GOOSE_MINFORAGEDECAYRATE' = 'Min accepted forage rate decay',
		'GOOSE_FEEDINGTIME' = 'Feeding time',
		'GOOSE_ROOSTLEAVINGLIKELYHOOD' = 'Roost leaving likelyhood',
		'GOOSE_MEM_MINMEMVALUE' = 'Memory duration',
		'GOOSE_GRAINDECAYRATE' = 'Grain decay rate',
		'GOOSE_ROOSTLEAVEDISTSD' = 'SD of roost leave time distribution',
        'GOOSE_MEM_EXPECTEDFORAGINGTIME' = 'Expected foraging time'
        )
}
