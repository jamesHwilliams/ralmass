#' Convert numeric game type to character
#'
#' Spell out the full name of the species shot from the numeric code
#' 
#' @param GameType integer A game type
#' @return The species name
#' @export
ConvertGameType = function(GameType) {
	# Since the numeric version of switch starts at 1, I convert to character.
	x = as.character(GameType)
	switch(x,
		'0' = 'Pinkfoot',
		'1' = 'Pinkfoot',
		'2' = 'Barnacle',
		'3' = 'Barnacle',
		'4' = 'Greylag',
		'5' = 'Greylag',
		'foobar'
		)
}
