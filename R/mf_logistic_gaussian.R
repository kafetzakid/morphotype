#' @title Define positive zero and negative fuzzy sets
#'
#' @param indMin a numeric scalar. The global minimum for the universe of discourse.
#' @param indMax a numeric scalar. The global maximum for the universe of discourse.
#' @param seqstep a numeric 1D vector. Increment of the sequence in the universe of discourse.
#' @param kval a numeric scalar. The logistic growth rate or steepness of the curve.
#' @param x0val a numeric scalar. The x value of the sigmoid midpoint.
#' @param mean0 a numeric scalar.
#' @param sd0 a numeric scalar.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item xvals_mf - a numeric 1D vector. The values defining the universe of discourse.
#'   \item yvals_mf_A - a numeric 1D vector. The values defining the fuzzy set positive.
#'   \item yvals_mf_B - a numeric 1D vector. The values defining the fuzzy set zero.
#'   \item yvals_mf_C - a numeric 1D vector. The values defining the fuzzy set negative.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' mf_logistic_gaussian(indMin=-50,indMax=50,seqstep=0.1,kval=0.3,x0val=0,mean0=0,sd0=5)
mf_logistic_gaussian = function(indMin, indMax, seqstep, kval, x0val, mean0, sd0) {

  xvals_mf = seq(indMin, indMax, seqstep)
  xvals_mf = round(xvals_mf, 1)
  yvals_mf_A = 1/(1+exp(-kval*(xvals_mf - x0val)))
  kval = -kval
  x0val = -x0val
  yvals_mf_C = 1/(1+exp(-kval*(xvals_mf - x0val)))
  yvals_mf_B = exp(-(xvals_mf-mean0)^2/(2*sd0^2))

  returns = list("xvals_mf" = xvals_mf, "yvals_mf_A" = yvals_mf_A, "yvals_mf_B" = yvals_mf_B, "yvals_mf_C" = yvals_mf_C)
  return(returns)
}
