#' @title Compute the height of the profile
#'
#' @param Y_rim_pos a named list of 1. The y position for the highest profile point.
#' @param Y_bot_pos a named list of 1. The y position for the lowest profile point.
#'
#' @return a numeric scalar. The height of the profile in pixels.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_height(Y_rim_pos = m7$bbox_TLBR[1], Y_bot_pos = m7$bbox_TLBR[3])
get_height = function(Y_rim_pos, Y_bot_pos){

  sherd_height = as.numeric(Y_rim_pos) - as.numeric(Y_bot_pos) + 1
  return(sherd_height)

}
