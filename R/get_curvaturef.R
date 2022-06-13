#' @title Compute the curvature between two specific points of the profile.
#'
#' @param slop1 a numeric scalar. The slope value at the first point.
#' @param slop2 a numeric scalar. The slope value at the second point.
#'
#' @return a numeric scalar. The value of curvature between two specific points.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_curvaturef(slop1 = -1, slop2 = 1)
get_curvaturef = function(slop1, slop2) {
  tanTh = (slop2 - slop1)/(1 + slop2*slop1) # abs()
  theta = atan(tanTh)
  return(theta)
}
