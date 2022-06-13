#' @title Compute position of the profile rim line
#'
#' @param image_denoised Output of get_input.
#' @param Y_min_i Output of get_projection_y.
#' @param margin Numeric value. The range in pixels the topline is checked.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item topline_Yi - a numeric scalar. The y axis distance in pixels from the first data point to the rim line.
#'   \item X_Rim_min_updated - a numeric scalar. The updated y axis index for the rim line.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_topline(image_denoised = image_denoised, Y_min_i = m2$Y_min_i, margin = 12)
get_topline = function(image_denoised, Y_min_i, margin){

  Yi = NULL
  Yi_list = NULL

  for (m in 0:margin) {
    Yi = length(which(image_denoised[,Y_min_i[length(Y_min_i)-m]]>0))
    Yi_list = c(Yi_list, Yi)
  }

  topline_Yi = which.max(Yi_list)

  X_Rim_min = min(which((image_denoised[,Y_min_i[length(Y_min_i) + 1 - topline_Yi]]>0) == TRUE))

  returns = list("topline_Yi" = topline_Yi, "X_Rim_min_updated" = X_Rim_min)

  return(returns)

}
