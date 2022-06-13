#' @title Compute how flat the top of the profile is
#'
#' @param img a numeric 2D matrix. The binary matrix of the input image as a result of get_input.
#' @param ym a numeric vector. The index for each point on the y axis containing data.
#' @param ty a numeric scalar. The y position of the top line as a result of get_topline.
#' @param xrm a numeric scalar. The x position of the highest rim point as a result of get_topline.
#'
#' @return a numeric scalar. The width in pixels for the flat top of the rim.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_flat_rim(img = image_denoised, ym = m2$Y_min_i, ty = m4$topline_Yi, xrm = m4$X_Rim_min_updated)
get_flat_rim = function(img, ym, ty, xrm){

  Prof_Thic_X2_beg = min(which((img[,ym[length(ym)-ty-1]]>0) == TRUE))
  Prof_Thic_X2_Ind = which.max(diff(which((img[,ym[length(ym)-ty-1]]>0) == TRUE))>2)
  Prof_Thic_X2_end = which((img[,ym[length(ym)-ty-1]]>0) == TRUE)[Prof_Thic_X2_Ind]+1
  Rim_Diam_extra_half = Prof_Thic_X2_end - xrm

  return(Rim_Diam_extra_half)

}
