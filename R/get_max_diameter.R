#' @title Compute the maximum diameter of the profile and its location
#'
#' @param image_profile a numeric 2D matrix. The binary matrix with the drawing cropped at its bounding box, as a result of get_vessel.bbox.
#' @param xx a dataframe. Melted profile matrix as a result of get_transforms.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item max.diameter - a numeric scalar. The maximum diameter of the profile in pixels.
#'   \item max.diameter.loc - a numeric scalar. The location of the maximum diameter from the top of the profile in pixels.
#'   \item YposXmin - a numeric 1D vector. The y position in the image_profile of the maximum diameter on the left of the profile.
#'   \item YposXmax - a numeric 1D vector. The y position in the image_profile of the maximum diameter on the right of the profile.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_max_diameter(image_profile = m7$image_profile, xx = m10)
get_max_diameter = function(image_profile, xx){

  indexX = unique(xx[(xx$value == 1), "Var1"])
  ind_X1 = indexX[which.min(indexX)]
  ind_X2 = indexX[which.max(indexX)]

  sherd_maxDiam = ind_X2 - ind_X1 + 1

  YposXmin = xx$Var2[(xx$Var1 == ind_X1) & (xx$value == 1)]
  YposXmax = xx$Var2[(xx$Var1 == ind_X2) & (xx$value == 1)]

  sherd_maxDiam_loc = dim(image_profile)[2] - YposXmin[length(YposXmin)]

  returns = list("max.diameter" = sherd_maxDiam, "max.diameter.loc" = sherd_maxDiam_loc, "YposXmin" = YposXmin, "YposXmax" = YposXmax)
  # are YposXmin and YposXmax needed? There are 6 int in this example - test of sequential ?! - These 6 here are the tip thickness.

  return(returns)

}

