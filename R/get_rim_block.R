#' @title Compute the percentage the rim profile resembles a block
#'
#' @param rim.height a numeric scalar. The height of the rim profile.
#' @param sherd.height a numeric scalar. The height of the profile.
#' @param outL a numeric 1D vector. The outer profile line.
#' @param all_WT a numeric 1D vector. The wall thickness in pixels across the height of the profile wall.
#'
#' @return A list with the following item:
#' \itemize{
#'   \item block.perC - a numeric scalar. The ratio of resemblance to a block.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_rim_block(rim.height=67, sherd.height=m9, outL=m19$outer.line, all_WT=m21$all_WT)
get_rim_block = function(rim.height, sherd.height, outL, all_WT) {

  block.perC = 9999

  if (length(rim.height) > 0 && rim.height != 9999 && rim.height > 0) {            # rim.end.p > 1 ?
    rim.max.width = max(all_WT[(sherd.height - rim.height):sherd.height])
    perfect.block = rim.max.width*rim.height
    actual.block = sum(all_WT[(sherd.height - rim.height):sherd.height])
    block.perC = round(1 - (perfect.block - actual.block)/perfect.block, 4)*100
  }

  returns = list("block.perC" = block.perC)

  return(returns)

}
