#' Calculate the fit of habitat use frequencies
#'
#' Calculate the fit between habitat use scored in the field 
#' and habitat use based on the simulations
#'
#' @param FieldData data.table The table with the field data
#' @param SimData data.table The field forage output file from ALMaSS
#' @return data.table The fit per species and month
#' @export
CalcHabitatUseFit = function(FieldData, SimData) {
# Field data:
	h = unique(FieldData[, HabitatUse])
	sp = unique(FieldData[, Species])
	mths = unique(FieldData[, Month])
	fieldcombs = expand.grid(N = 0, HabitatUse = h, Month = mths, Species =  sp,
		stringsAsFactors = FALSE)
	fieldcombs = data.table::as.data.table(fieldcombs)
	FieldData = rbind(FieldData, fieldcombs)
	cols = c('Species', 'HabitatUse', 'Month')
	FieldData = FieldData[, lapply(.SD, sum, na.rm = TRUE), by = cols]
	FieldData[, NMTotal:=sum(N), by=.(Month, Species)]
	FieldData[, PropField:=N/NMTotal]
	FieldData[, c('N', 'NMTotal'):=NULL]
	data.table::setkeyv(FieldData, cols)
# field data end
	
# Sim data:
	SimData = SimData[Month %in% mths,]
# Pinkfoot
	hbpf = SimData[, .(Pinkfoot, HabitatUsePF, Month)]
	hbpf[, Species:='Pinkfoot']
	newnames = c('HabitatUse', 'N')
	data.table::setnames(hbpf, old = c('HabitatUsePF', 'Pinkfoot'), new = newnames)
# Greylag
	hbgl = SimData[, .(Greylag, HabitatUseGL, Month)]
	hbgl[, Species:='Greylag']
	data.table::setnames(hbgl, old = c('HabitatUseGL', 'Greylag'), new = newnames)
# Barnacle
	hbbn = SimData[, .(Barnacle, HabitatUseBN, Month)]
	hbbn[, Species:='Barnacle']
	data.table::setnames(hbbn, old = c('HabitatUseBN', 'Barnacle'), new = newnames)
# Full data
	hb = rbind(hbpf, hbbn, hbgl)
	hb = hb[complete.cases(hb),]

	h = unique(c(hb[,HabitatUse], h))
	combs = expand.grid(N = 0, HabitatUse = h, Month = mths,
		Species = sp, stringsAsFactors = FALSE) 
	combs = data.table::as.data.table(combs)

	temp = rbind(hb, combs)
	temp[,NMTotal:=sum(N), by=.(Month, Species)]
	temp[, PropSim:=N/NMTotal]
	temp = temp[, lapply(.SD, sum, na.rm = TRUE), by = cols]
	temp = temp[, .(HabitatUse, Month, Species, PropSim)][is.na(PropSim), PropSim:=0] %>% unique
	data.table::setkeyv(temp, cols)

	HabFit = merge(temp, habuse)
	cols = c('Species', 'Month')
	HabFit = HabFit[, Fit:=1-sum((PropSim-PropField)^2), by = cols]
	HabFit = HabFit[, .(Species, Month, Fit)] %>% unique
}
