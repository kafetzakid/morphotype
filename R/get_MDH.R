#' @title Compute the ratio between height and maximum diameter
#'
#' @param sherd_maxDiam_loc a numeric scalar. The maximum diameter of the profile.
#' @param sherd_height a numeric scalar. The height of the profile.
#'
#' @return a numeric scalar. The ratio of the height over the maximum diameter.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_MDH(sherd_maxDiam_loc = m11$max.diameter.loc, sherd_height = m9)
get_MDH = function(sherd_maxDiam_loc, sherd_height){

  MDLHratio = sherd_maxDiam_loc/sherd_height
  return(MDLHratio)

}
