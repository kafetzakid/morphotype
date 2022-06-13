#' @title Define high and low fuzzy sets
#'
#' @param indMin a numeric scalar. The global minimum for the universe of discourse.
#' @param indMax a numeric scalar. The global maximum for the universe of discourse.
#' @param seqstep a numeric 1D vector. Increment of the sequence in the universe of discourse.
#' @param kval a numeric scalar. The logistic growth rate or steepness of the curve.
#' @param x0val a numeric scalar. The x value of the sigmoid midpoint.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item xvals_mf - a numeric 1D vector. The values defining the universe of discourse.
#'   \item yvals_mf_A - a numeric 1D vector. The values defining the fuzzy set high.
#'   \item yvals_mf_B - a numeric 1D vector. The values defining the fuzzy set low.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' mf_logistic(indMin = 0,indMax = 1, seqstep = 0.001, kval = 15, x0val = 0.5)
mf_logistic = function(indMin, indMax, seqstep, kval, x0val) {

  xvals_mf = seq(indMin, indMax, seqstep)
  xvals_mf = round(xvals_mf, 3)
  yvals_mf_A = 1/(1+exp(-kval*(xvals_mf - x0val)))
  kval = -kval
  yvals_mf_B = 1/(1+exp(-kval*(xvals_mf - x0val)))

  returns = list("xvals_mf" = xvals_mf, "yvals_mf_A" = yvals_mf_A, "yvals_mf_B" = yvals_mf_B)
  return(returns)

}
