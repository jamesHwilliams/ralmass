#' Convert numeric vegphase to its character name
#'
#' Spell out the full name of the vegphase from the numeric code
#' 
#' @param VegPhase integer A vegphase number
#' @return The vegphase name
#' @export
ConvertVegPhase = function(VegPhase) {
	# Since the numeric version of switch starts at 1, I convert to character.
	x = as.character(VegPhase)
	switch(x,
		'0' = 'janfirst',
		'1' = 'sow',
		'2' = 'marchfirst',
		'3' = 'harvest1',
		'4' = 'harvest2',
		'foobar'
		)
}
