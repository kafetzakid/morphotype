#' @title Compute the thickness at the rim tip
#'
#' @param image_denoised a numeric 2D vector. The profile binary matrix.
#' @param X_min a numeric scalar. The first x axis index which contains data.
#'
#' @return a numeric scalar. The width in pixels for the flat top of the rim.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_tip_thickness(image_denoised = image_denoised, X_min = m1$X_min)
get_tip_thickness = function(image_denoised, X_min){

  RimWT_margin_i = image_denoised[X_min,1:dim(image_denoised)[2]]
  RimWT_margin = which.min(diff(RimWT_margin_i)) - which.max(diff(RimWT_margin_i))

  return(RimWT_margin)
}
