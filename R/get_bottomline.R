#' @title Compute the coordinate of the profile's bottom line
#'
#' @param image_denoised a numeric 2D matrix. The 2D binary matrix of the input image.
#' @param Y_min_i a numeric 1D vector. The y index for each y including data across the image height.
#' @param margin a numeric scalar. The number of pixels checked for correction in the calculation.
#'
#' @return a numeric scalar. The y coordinate of the most bottom point of the profile.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_bottomline(image_denoised = image_denoised, Y_min_i = m2$Y_min_i, margin = 6)
get_bottomline = function(image_denoised, Y_min_i, margin){

  Yi_list = NULL
  Yi_list_sum = NULL

  firstD = diff(Y_min_i)

  for (m in 1:margin) {
    Yi = Y_min_i[which.max(firstD)+m]
    Yi_list = c(Yi_list, Yi)
    Yi_sum = sum(image_denoised[,Y_min_i[which.max(firstD)+m]])
    Yi_list_sum = c(Yi_list_sum, Yi_sum)
  }

  bottomline_Yi = Yi_list[which.max(Yi_list_sum)]

  return(bottomline_Yi)

}
