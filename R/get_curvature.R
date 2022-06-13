#' @title Compute the curvature and the slope for a given outline.
#'
#' @param outline a numeric 1D vector. The outline of the profile.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item curvf - a numeric 1D vector. The curvature representation of the given profile.
#'   \item slop - a numeric 1D vector. The tangent representation of the given profile.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_curvature(outline = m19$outer.line)
get_curvature = function(outline) {
  slop = NULL
  slop[1] = 0                            # needed?

  for (jj in 1:(length(outline)-1)) {
    x0 = jj
    y0 = outline[jj]
    x1 = jj + 1
    y1 = outline[jj+1]
    slop[jj+1] = (y1 - y0)/(x1 - x0)
  }

  curvf = NULL
  curvf[1] = 0                            # needed?
  for (jj in 2:(length(outline))) {               # changed from 2:(length(m19$outline)) to 2:(length(outline))
    curvf[jj] = get_curvaturef(slop[jj], slop[jj+1])
  }

  returns = list("curvf" = curvf, "slop" = slop)

  return(returns)

}
