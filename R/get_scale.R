#' @title Compute the pixels included in the scale
#'
#' @importFrom stats var
#'
#' @param image_denoised a numeric 2D matrix. The binary image matrix as a result of get_input.
#' @param Y_min_i a numeric vector. The index for each point on the y axis containing data as a result of get_projection_y.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item scale - a numeric scalar. The number of pixels included in the scale.
#'   \item scale_alt - a numeric scalar. The alternative number of pixels included in the scale.
#'   \item scaleCheck - a numeric scalar. Variance of scale.
#'   \item Scale_Bot - a numeric scalar. The y index the scale starts.
#'   \item scale_End - a numeric scalar. The y index the scale stops.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_scale(image_denoised = image_denoised, Y_min_i = m2$Y_min_i)
get_scale = function(image_denoised, Y_min_i){

  firstD = diff(Y_min_i)
  which.max(firstD)

  Scale_End = Y_min_i[which.max(firstD)]
  Scale_Bot = Y_min_i[1]

  scale1 = max(which(image_denoised[,Scale_Bot]>0)) - min(which(image_denoised[,Scale_Bot]>0)) + 1
  scale2 = max(which(image_denoised[,Scale_Bot+1]>0)) - min(which(image_denoised[,Scale_Bot+1]>0)) + 1
  scale3 = max(which(image_denoised[,Scale_Bot+2]>0)) - min(which(image_denoised[,Scale_Bot+2]>0)) + 1
  scale = max(scale1,scale2,scale3)
  scale_alt = min(scale1,scale2,scale3)
  scaleCheck = var(c(scale1,scale2,scale3))

  returns = list("scale" = scale, "scale_alt" = scale_alt, "scaleCheck" = scaleCheck, "Scale_Bot" = Scale_Bot, "Scale_End" = Scale_End)
  return(returns)
}
