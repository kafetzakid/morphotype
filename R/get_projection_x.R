#' @title One dimensional projection of the profile on the x-axis
#'
#' @param image_denoised a numeric 2D matrix. The binary matrix with the drawing cropped at its bounding box, as a result of get_vessel.bbox.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item xx_range - a numeric 1D vector. Frequency of data per x axis index.
#'   \item X_min - a numeric scalar. The first x axis index which contains data.
#'   \item X_max - a numeric scalar. The last x axis index which contains data.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples get_projection_x(image_denoised = image_denoised)
get_projection_x = function(image_denoised){

  xx_range = NULL

  for (dx in 1:(dim(image_denoised)[1])) {
    xx_range[dx] = sum(image_denoised[dx, 1:dim(image_denoised)[2]])
  }
  X_min = min(which((xx_range>0) == TRUE))
  X_max = max(which((xx_range>0) == TRUE))

  returns = list("xx_range" = xx_range, "X_min" = X_min, "X_max" = X_max)
  return(returns)
}
