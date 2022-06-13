#' @title Compute the ratio between height and rim diameter
#'
#' @param sherd_height a numeric scalar. The height of the profile.
#' @param sherd_rimDiam a numeric scalar. The rim diameter of the profile.
#'
#' @return a numeric scalar. The ratio of the height over the rim diameter.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_HW(sherd_height = m9, sherd_rimDiam = m13$sherd_rimDiam)
get_HW = function(sherd_height, sherd_rimDiam) {

  sherd_HWratio = sherd_height/sherd_rimDiam
  return(sherd_HWratio)

}

