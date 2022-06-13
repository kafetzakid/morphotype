#' @title Compute the wall thickness at a specific location
#'
#' @param xx a dataframe. Melted profile matrix as a result of get_transforms.
#' @param loc a numeric scalar. The location in pixels where the wall thickness should be computed. It takes minimum value 1 and maximum equal to the profile height.
#'
#' @return a numeric scalar. The wall thickness in pixels at a specific location.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_wall_thickness(xx = m10, loc = round(2*m9/3))
get_wall_thickness = function(xx, loc){

  # loc = round(2*sherd_height/3)  # at 2/3
  # loc = YposXmin[length(YposXmin)]  # at maxDiam
  WThick_P_X1 = xx[(xx$value == 1 & xx$Var2 == loc), "Var1"][1]
  WThick_P_X2 = xx[(xx$value == 0 & xx$Var2 == loc & xx$Var1 > WThick_P_X1), "Var1"][1]
  sherd_WThick1 = WThick_P_X2 - WThick_P_X1

  return(sherd_WThick1)

}
