#' @title One dimensional projection of the profile on the y-axis
#'
#' @param image_denoised a numeric 2D matrix. The binary matrix with the drawing cropped at its bounding box, as a result of get_vessel.bbox.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item yy_range - a numeric 1D vector. Frequency of data per y axis index.
#'   \item Y_min_i - a numeric vector. The index for each point on the y axis containing data.
#'   \item X_Rim_min - a numeric scalar.The x axis index of the top outer profile point.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples get_projection_y(image_denoised = image_denoised)
get_projection_y = function(image_denoised){

  yy_range = NULL

  for (dy in 1:(dim(image_denoised)[2])) {
    yy_range[dy] = sum(image_denoised[1:dim(image_denoised)[1], dy])
  }
  Y_min_i = which((yy_range>0) == TRUE)

  X_Rim_min = min(which((image_denoised[,Y_min_i[length(Y_min_i)]]>0) == TRUE))

  returns = list("yy_range" = yy_range, "Y_min_i" = Y_min_i, "X_Rim_min" = X_Rim_min)
  return(returns)
}
