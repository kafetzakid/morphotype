#' Compute the diameter at the base of the profile
#'
#' @param xx a dataframe. Melted profile matrix as a result of get_transforms.
#'
#' @return a numeric scalar. The diameter at the bottom of the profile. In case the profile is recognized as full profile, the value is the diameter at the profile base.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_bottom_diameter(xx = m10)
get_bottom_diameter = function(xx){

  indexX = unique(xx$Var1[(xx$value == 1) & (xx$Var2 == 1)])
  ind_X1 = indexX[which.min(indexX)]
  ind_X2 = indexX[which.max(indexX)]
  sherd_botDiam = ind_X2 - ind_X1 + 1

  return(sherd_botDiam)

}
