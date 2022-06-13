#' @title Compute the bounding box of the profile
#'
#' @param img a numeric 2D matrix. The binary matrix of the input image as a result of get_input.
#' @param ym a numeric vector. The index for each point on the y axis containing data.
#' @param yt a numeric scalar. The y axis distance in pixels from the first data point to the rim line.
#' @param yb a numeric scalar. The y coordinate of the most bottom point of the profile.
#' @param m1 a list of 3. Output of get_projection_x.
#'
#' @return A list with the follwoing items:
#' \itemize{
#'   \item image_profile - a numeric 2D matrix. The binary matrix with the drawing cropped at its bounding box, as a result of get_vessel.bbox.
#'   \item bbox_TLBR - a list of four numeric scalars. The bounding box of the profile drawing.
#' }
#'
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_vessel_bbox(img=image_denoised, ym=m2$Y_min_i, yt=m4$topline_Yi, yb=m6, m1=m1)
get_vessel_bbox = function(img, ym, yt, yb, m1){

  Y_rim.pos = ym[length(ym)-yt+1] # added the +1 to retain the line in the rim diameter
  Y_bot.pos = yb

  image_profile = img[m1$X_min:m1$X_max, Y_bot.pos:Y_rim.pos]

  returns = list("image_profile" = image_profile, "bbox_TLBR" = list("Y_rim_pos" = Y_rim.pos, "X_max" = m1$X_max, "Y_bot_pos" = Y_bot.pos, "X_min" = m1$X_min))

  return(returns)

}
