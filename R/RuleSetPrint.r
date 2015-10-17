#' Print rule set number and description
#'
#' Print rule set number and description
#' 
#' @param ruleset numeric The rule set number
#' @return character The rule set number and description
#' @export

RuleSetPrint = function(ruleset = NULL){
	if(is.null(ruleset))
	{
		stop('Ruleset paramter missing')
	}
	if(ruleset == 0)
	{
		theset = 'Ruleset 0 - Random'
	}
	if(ruleset == 1)
	{
		theset = 'Ruleset 1 - Closest'
	}
	if(ruleset == 2)
	{
		theset = 'Ruleset 2 - RandomOpen'
	}
	if(ruleset == 3)
	{
		theset = 'Ruleset 3 - RandomMaxDensity'
	}
	if(ruleset == 4)
	{
		theset = 'Ruleset 4 - ClostestOpen'
	}
	if(ruleset == 5)
	{
		theset = 'Ruleset 5 - ClosestMaxDensity'
	}
	if(ruleset == 6)
	{
		theset = 'Ruleset 6 - RandomOpenMaxDensity'
	}
	if(ruleset == 7)
	{
		theset = 'Ruleset 7 - ClostestOpenMaxDensity'
	}
	if(ruleset == 8)
	{
		theset = 'Ruleset 8 - ClosestProb'
	}
	if(ruleset == 9)
	{
		theset = 'Ruleset 9 - ClosestProbOpen'
	}
	if(ruleset == 10)
	{
		theset = 'Ruleset 10 - ClosestProbOpenMaxDensity'
	}
	return(theset)
}


