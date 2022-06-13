#' @title Compute the percentage the rim profile resembles a trapezoid
#'
#' @param rim.height a numeric scalar. The height of the rim profile.
#' @param sherd.height a numeric scalar. The height of the profile.
#' @param rim.flattness a numeric scalar. The rim flatness in pixels as results from get_circumference.
#' @param rim.bottom.width a numeric scalar. The rim bottom width in pixels as results from get_circumference.
#' @param all_WT a numeric 1D vector. The wall thickness in pixels across the height of the profile wall.
#'
#' @return A list with the following item:
#' \itemize{
#'   \item trapez.perC - a numeric scalar. The ratio of resemblance to a trapezoid.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_rim_trapezoid(rim.height = 67, sherd.height = m9, rim.flattness = m5,
#'                   rim.bottom.width = m38$bottom_width, all_WT = m21$all_WT)
get_rim_trapezoid = function(rim.height, sherd.height, rim.flattness, rim.bottom.width, all_WT) {

  trapez.perC = 9999

  Rim.trapezoid.area = (rim.height*(rim.flattness + rim.bottom.width))/2
  Rim.area = sum(all_WT[(sherd.height - rim.height):sherd.height])
  trapez.perC  = round(1 - (Rim.trapezoid.area - Rim.area)/Rim.trapezoid.area, 4)*100

  returns = list("trapez.perC" = trapez.perC)

  return(returns)

}
