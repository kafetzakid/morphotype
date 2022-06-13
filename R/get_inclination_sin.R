#' @title Compute the inclination of the profile
#'
#' @param xx a dataframe. Melted profile matrix as a result of get_transforms.
#' @param m5 a numeric scalar. The flatness of the rim in pixels as a result of get_flat.rim.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item sherd_incl_distance - a numeric scalar. Actual length in pixels of the conceivable hypotenuse for the triangle measuring sherd_incl_sin.
#'   \item sherd_incl_sin - a numeric scalar. Sine value for the inclination measured at the profile top most inner point and the bottom most outer point.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_inclination_sin(xx = m10, m5 = m5)
get_inclination_sin = function(xx, m5){

  incl_X_top = xx[(xx$value == 1 & xx$Var2 == max(xx$Var2)), "Var1"][1] + m5 # top first inner point ==> in SEMA II it is the outer point
  incl_X_bot = xx[(xx$value == 1 & xx$Var2 == 1), "Var1"][1]  # bottom of the sherd, first (outer) point.

  sherd_incl_distance = round(sqrt((incl_X_top - incl_X_bot)^2 + (max(xx$Var2) - 1)^2),0)
  sherd_incl_sin = max(xx$Var2)/sherd_incl_distance

  # make one more variable for the angle of the wall, similar approach to the protuberance length calculation

  returns = list("sherd_incl_distance" = sherd_incl_distance, "sherd_incl_sin" = sherd_incl_sin)

  return(returns)
  # return degrees: https://stackoverflow.com/questions/1897704/angle-between-two-vectors-in-r

}
