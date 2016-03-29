#' Generate a range combinations of paramter values
#'
#' Generate all possible combinations of values in the input vectors and 
#' (optionally) write them to a file. Typically used when making scenarios for 
#' fitting parameters.
#' 
#' This function is a modification of expand.grid from base.
#' 
#' @param ... numeric One or more vectors (usually made with seq and the
#' length.out argument)
#' @param write logical Should a file with the table be written to disk?
#' @param splits numeric The number of files to split the scenarios into. 
#' (Optional). If not NULL the files gets written to disk.
#' @return data.frame One column data.frame where each line is a text string
#' with the config variable and its value
#' @export
#' @examples
#' val = seq(1, 5, length.out = 5)
#' val2 = seq(10, 50, length.out = 5)
#' GenerateParams('GOOSE_MINFORAGEOPENNESS' = val,
#' 'HUNTERS_MAXDENSITY' = val2, write = FALSE)
GenerateParams = function (..., write = FALSE, splits = NULL) 
{
	# Code from the base expand.grid with the KEEP.ATTR.OUT and StringAsFactor
	# bits removed.
	nargs <- length(args <- list(...))
	if (!nargs) 
		return(as.data.frame(list()))
	if (nargs == 1L && is.list(a1 <- args[[1L]])) 
		nargs <- length(args <- a1)
	if (nargs == 0L) 
		return(as.data.frame(list()))
	cargs <- vector("list", nargs)
	iArgs <- seq_len(nargs)
	nmc <- paste0("Var", iArgs)
	nm <- names(args)
	# ralmass modification
	# Modify the names to also specify parameter type:
	nm = sapply(nm, FUN = ParamType)
	if (is.null(nm)) 
		nm <- nmc
	else if (any(ng0 <- nzchar(nm))) 
		nmc[ng0] <- nm[ng0]
	names(cargs) <- nmc
	rep.fac <- 1L
	d <- lengths(args)

	orep <- prod(d)
	if (orep == 0L) {
		for (i in iArgs) cargs[[i]] <- args[[i]][FALSE]
	}
	else {
		for (i in iArgs) {
			x <- args[[i]]
			nx <- length(x)
			orep <- orep/nx
			x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac, nx)), orep)]
			cargs[[i]] <- x
			rep.fac <- rep.fac * nx
		}
	}

	rn <- .set_row_names(as.integer(prod(d)))
    # The ralmass modification
	exgrid = structure(cargs, class = "data.frame", row.names = rn)
	values = as.vector(t(exgrid))
	confignames = rep(names(exgrid), nrow(exgrid))
	df = data.frame('Params' = paste(confignames, values, sep = ' = '))
	if(!is.null(splits)) 
	{
		starts = seq(1, nrow(df), by = nrow(df)/splits)
		ends = seq(nrow(df)/splits, nrow(df), by = nrow(df)/splits)
		for (i in 1:splits) 
		{
			temp = df[starts[i]:ends[i],]
			fname = paste0('ParameterValues', i, '.txt')
			write.table(temp, file = fname, sep = '\t', quote = FALSE,
			row.names = FALSE, col.names = FALSE)
		}
		cat('Wrote', splits, 'versions of ParameterValues.txt to',
			getwd(), '\n')
		cat('The full list of scenarios is printed here:\n')
	}
	if(write) 
	{
		write.table(df, file = 'ParameterValues.txt', sep = '\t', quote = FALSE,
			row.names = FALSE, col.names = FALSE)
		cat('Printed following to ParameterValues.txt:\n')
	}
	return(df)
}

# Helper function ---------------------
ParamType <- function(name = NULL) {
	if(is.null(name)) {stop('Input argument missing \n')}
    name = str_trim(name, side = "both")
    switch(EXPR = name,
  	# Names of the paramters:
  		'GOOSE_MINFORAGEOPENNESS' = 'GOOSE_MINFORAGEOPENNESS (int)',
        'BGOOSE_FOLLOWINGLIKELYHOOD' = 'BGOOSE_FOLLOWINGLIKELYHOOD (int)',
		'PFGOOSE_FOLLOWINGLIKELYHOOD' = 'PFGOOSE_FOLLOWINGLIKELYHOOD (int)',
		'GLGOOSE_FOLLOWINGLIKELYHOOD' = 'GLGOOSE_FOLLOWINGLIKELYHOOD (int)',
		'GOOSE_MAXAPPETITESCALER' = 'GOOSE_MAXAPPETITESCALER (float)',
		'GOOSE_MAXENERGYRESERVEPROPORTION' = 'GOOSE_MAXENERGYRESERVEPROPORTION (float)',
		'GOOSE_LEAVINGTHRESHOLD' = 'GOOSE_LEAVINGTHRESHOLD (float)',
		'GOOSE_FORAGEDIST_BN' = 'GOOSE_FORAGEDIST_BN (float)',
		'GOOSE_FORAGEDIST_PF' = 'GOOSE_FORAGEDIST_PF (float)',
		'GOOSE_FORAGEDIST_GL' = 'GOOSE_FORAGEDIST_GL (float)',
		'GOOSE_MINFORAGEDECAYRATE' = 'GOOSE_MINFORAGEDECAYRATE (float)',
		'GOOSE_ENERGYCALIBRATION' = 'GOOSE_ENERGYCALIBRATION (float)',
		'GOOSE_FEEDINGTIME' = 'GOOSE_FEEDINGTIME (float)',
		'GOOSE_ROOSTLEAVINGLIKELYHOOD' = 'GOOSE_ROOSTLEAVINGLIKELYHOOD (int)',
		'GOOSE_MEM_DISTPENALTY' = 'GOOSE_MEM_DISTPENALTY (float)',
		'GOOSE_MEM_MINMEMVALUE' = 'GOOSE_MEM_MINMEMVALUE (int)',
		'GOOSE_GRAINDECAYRATE' = 'GOOSE_GRAINDECAYRATE (float)',
		'HUNTERS_MAXDENSITY' = 'HUNTERS_MAXDENSITY (float)',
        'CLOSESTFARMPROBPARAMONE' = 'CLOSESTFARMPROBPARAMONE (float)'
        )
}
