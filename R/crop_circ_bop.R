#' Crops circumference at bottom outer point
#'
#' @importFrom stats smooth.spline
#' @importFrom reshape2 melt
#'
#' @param outer.line a numeric 1D vector. The outer profile line.
#' @param inner.line a numeric 1D vector. The inner profile line.
#' @param marSc a numeric scalar. The amount of pixels vertically after the outer wall of the base that will be included in the result.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item mati_scaled - a dataframe with numeric attributes. The x and y position of the points belonging to the circumference.
#'   \item thrP - a numeric scalar. The x index value of the custom cut of the profile base.
#'   \item indx - a numeric 1D vector. The y indexes with a value in the custom cut of the profile base.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' crop_circ_bop(outer.line = m19$outer.line, inner.line = m19$inner.line, marSc = 5)
crop_circ_bop = function(outer.line, inner.line, marSc) {

  thrP = (outer.line[1] + marSc)
  test = inner.line > thrP
  indx_all = which(test)
  if(length(which(diff(indx_all) > 1)) > 0) {indx = indx_all[1:which(diff(indx_all) > 1)]} else {indx = indx_all}
  inner.line[indx] = thrP
  m54 = get_circumference(outer.line, inner.line) # <--- calls function of package
  mati_scaled = as.matrix(m54$data_circumference)
  colnames(mati_scaled) = NULL
  rownames(mati_scaled) = NULL

  returns = list("mati_scaled" = mati_scaled, "thrP" = thrP, "indx" = indx)

  return(returns)

}

