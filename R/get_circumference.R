#' @title Compute the circumference of the profile
#'
#' @param outer.line a numeric 1D vector. The outer profile line.
#' @param inner.line a numeric 1D vector. The inner profile line.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item bottom_width - a numeric scalar. The width of the bottom in the rim profile.
#'   \item lip_width - a numeric scalar. The width of the top in the rim profile.
#'   \item data_circumference - a dataframe with numeric attributes. The x and y position of the points belonging to the circumference.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_circumference(outer.line = m53$outer.rim.line.scaled,
#'                   inner.line = m53$inner.rim.line.scaled)
get_circumference = function(outer.line, inner.line) {

  bottom_width = 9999
  lip_width = 9999
  data_circum = NULL

  # remake the circumference from inner and outer line

  if (length(outer.line) == length(inner.line)) {
    lip_width = length((inner.line[length(inner.line)]-1):(outer.line[length(outer.line)]+1))
    bottom_width = length((outer.line[1]+1):(inner.line[1]-1)) # this may be a footbase width if we have an image of a full profile and there is a foot.
    df_circum = rbind.data.frame(
      cbind.data.frame("x" = (outer.line[1]+1):(inner.line[1]-1), "y" = rep(1, bottom_width)),
      cbind.data.frame("x" = inner.line, "y" = 1:length(inner.line)),
      cbind.data.frame("x" = (inner.line[length(inner.line)]-1):(outer.line[length(outer.line)]+1), "y" = rep(length(inner.line), lip_width)),
      cbind.data.frame("x" = outer.line[length(outer.line):1], "y" = length(outer.line):1)
    )
  }

  returns = list("bottom_width" = bottom_width, "lip_width" = lip_width, "data_circumference" = df_circum)
  return(returns)

}
