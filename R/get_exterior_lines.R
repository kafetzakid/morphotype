#' @title Compute the y positions of each potential groove line
#'
#' @param image_profile a numeric 2D matrix. The binary matrix with the drawing cropped at its bounding box, as a result of get_vessel.bbox.
#' @param margin a numeric scalar. The margin as the number of pixels on which the potential grooves will be checked.
#' @param sherd_rimDiam a numeric scalar. The rim diameter of the profile in pixels.
#'
#' @return A list with the following item:
#' \itemize{
#'   \item list_potGroov_ext - a numeric 1D vector. The y position of each potential groove line.
#' }
#'
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_exterior_lines(image_profile = m7$image_profile, margin = 10, sherd_rimDiam = m13$sherd_rimDiam)
get_exterior_lines = function(image_profile, margin, sherd_rimDiam){

  xx_range = NULL
  for (ii in 1:(dim(image_profile)[1])) {
    xx_range[ii] = sum(image_profile[ii,1:dim(image_profile)[2]])
  }
  X_max = max(which((xx_range>0) == TRUE))

  xx_pot_cut = which(xx_range == max(xx_range))
  xx_cut = max(xx_pot_cut[which.min(abs(xx_pot_cut - sherd_rimDiam/2))]) # m5 & check diff with middle line
  data_corrected = image_profile[(xx_cut+1):X_max,]

  list_potGroov = which(colSums(data_corrected[1:margin,]) > 2)

  returns = list("list_potGroov_ext" = list_potGroov)

  return(returns)

}
