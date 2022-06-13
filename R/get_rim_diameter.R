#' @title Compute the rim diameter of the profile
#'
#' @param xx a dataframe. Melted profile matrix as a result of get_transforms.
#' @param sherd_height a numeric scalar. The height of the profile.
#' @param margin a numeric scalar. The number of pixels checked for correction in the calculation.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item sherd_rimDiam - a numeric scalar. The rim diameter of the profile in pixels.
#'   \item sherd_rimDiam.certain - a character vector. Specifies whether the calculated rim diameter is 'known' or 'unknown'.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_rim_diameter(xx = m10, sherd_height = m9, margin = 10)
get_rim_diameter = function(xx, sherd_height, margin){

  indexX = unique(xx$Var1[(xx$value == 1) & (xx$Var2 == sherd_height)])
  if(sum(diff(indexX)>margin)>0) {sherd_rimDiam.certain = "unknown"} else {sherd_rimDiam.certain = "known"}

  ind_X1 = indexX[which.min(indexX)]
  ind_X2 = indexX[which.max(indexX)]
  sherd_rimDiam = ind_X2 - ind_X1 + 1

  returns = list("sherd_rimDiam" = sherd_rimDiam, "sherd_rimDiam.certain" = sherd_rimDiam.certain)

  return(returns)

}
