#' Generate a range of paramter values
#'
#' Generate a range of paramter values from specified start 
#' and end values. A simple wrapper around seq
#' 
#' Currently just a slight modification of expand.grid
#' 
#' 
#' @param start numeric Starting value.
#' @param end numeric End value.
#' @param N Number of required values.
#' @param Config The character string with the config variable (including (int))
#' @param start2 numeric Starting value for the (optional) second variable
#' @param end2 numeric End value for the (optional) second variable
#' @param N2 Number of required values for the (optional) second variable
#' @param Config2 The character string with the second config variable (including (int))
#' @return data.frame One column each value a text string with the config and 
#' and its value
#' @export

GenerateParams = function (..., KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE) 
{
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
    if (is.null(nm)) 
        nm <- nmc
    else if (any(ng0 <- nzchar(nm))) 
        nmc[ng0] <- nm[ng0]
    names(cargs) <- nmc
    rep.fac <- 1L
    d <- lengths(args)
    if (KEEP.OUT.ATTRS) {
        dn <- vector("list", nargs)
        names(dn) <- nmc
    }
    orep <- prod(d)
    if (orep == 0L) {
        for (i in iArgs) cargs[[i]] <- args[[i]][FALSE]
    }
    else {
        for (i in iArgs) {
            x <- args[[i]]
            if (KEEP.OUT.ATTRS) 
                dn[[i]] <- paste0(nmc[i], "=", if (is.numeric(x)) 
                  format(x)
                else x)
            nx <- length(x)
            orep <- orep/nx
            x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac, 
                nx)), orep)]
            if (stringsAsFactors && !is.factor(x) && is.character(x)) 
                x <- factor(x, levels = unique(x))
            cargs[[i]] <- x
            rep.fac <- rep.fac * nx
        }
    }
    if (KEEP.OUT.ATTRS) 
        attr(cargs, "out.attrs") <- list(dim = d, dimnames = dn)
    rn <- .set_row_names(as.integer(prod(d)))
    exgrid = structure(cargs, class = "data.frame", row.names = rn)
    values = as.vector(t(exgrid))
    confignames = rep(names(exgrid), nrow(exgrid))
    data.frame('Params' = paste(confignames, values, sep = ' = '))
}


