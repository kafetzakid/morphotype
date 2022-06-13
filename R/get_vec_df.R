#' @title Return a list of dataframes with the profile planar curves
#'
#' @param m19 a list. Output of get_wall_lines.
#' @param m32o a list. Output of get_curvature for the outer rim profile line.
#' @param m32i a list. Output of get_curvature for the inner rim profile line.
#' @param m33 a list. Output of get_smoothed.lines for the profile binary matrix.
#' @param m34o a list. Output of get_curvature for the outer rim profile splines model.
#' @param m34i a list. Output of get_curvature for the inner rim profile splines model.
#' @param m5 a numeric scalar. The width in pixels for the flat top of the rim.
#' @param m9 a numeric scalar. The height of the profile.
#' @param mY a numeric scalar. The height of the profile minus the height of the rim profile.
#' @param fl a character vector. Name of the image file including extension. The supported extensions are jpeg, png and tiff.
#'
#' @return A list with the following dataframes:
#' \itemize{
#'   \item perip.original - The original periphery data.
#'   \item perip.smoothed - The smoothed periphery data.
#'   \item slop.original - The original slope data.
#'   \item slop.smoothed - The smoothed slope data.
#'   \item curvf.original - The original curvature data.
#'   \item curvf.smoothed - The smoothed curvature data.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#'  get_vec_df(m19 = m19, m32o = m32o, m32i = m32i, m33 = m33,
#'             m34o = m34o, m34i = m34i, m5 = m5, m9 = m9,
#'             mY = mlypos, fl = filename)
get_vec_df = function(m19, m32o, m32i, m33, m34o, m34i, m5, m9, mY, fl) {

  perip.original = as.data.frame(c(m19$outer.line[mY:m9], rep(m19$outer.line[length(m19$outer.line)], m5), m19$inner.line[length(m19$inner.line):mY]))
  perip.smoothed = as.data.frame(c(m33$fit_splines$y[mY:m9], rep(m33$fit_splines$y[length(m33$fit_splines$y)], m5), m33$fit_splines.inn$y[length(m33$fit_splines.inn$y):mY]))
  slop.original = as.data.frame(c(m32o$slop, rep(0, m5), m32i$slop))
  slop.smoothed = as.data.frame(c(m34o$slop, rep(0, m5), m34i$slop))
  curvf.original = as.data.frame(c(m32o$curvf[!is.na(m32o$curvf)], rep(0, m5), m32i$curvf[!is.na(m32i$curvf)]))
  curvf.smoothed = as.data.frame(c(m34o$curvf[!is.na(m34o$curvf)], rep(0, m5), m34i$curvf[!is.na(m34i$curvf)]))

  colnames(perip.original) = paste0("Prh_O_",fl)
  colnames(perip.smoothed) = paste0("Prh_S_",fl)
  colnames(slop.original) = paste0("Slp_O_",fl)
  colnames(slop.smoothed) = paste0("Slp_S_",fl)
  colnames(curvf.original) = paste0("Crv_O_",fl)
  colnames(curvf.smoothed) = paste0("Crv_S_",fl)

  returns = list("perip.original" = perip.original, "perip.smoothed" = perip.smoothed,
                 "slop.original" = slop.original, "slop.smoothed" = slop.smoothed,
                 "curvf.original" = curvf.original, "curvf.smoothed" = curvf.smoothed)
  return(returns)
}
