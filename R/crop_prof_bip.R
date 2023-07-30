#' Crops profile at bottom inner point
#'
#' @importFrom EBImage as.Image
#'
#' @param profile.wall a numeric 2D matrix. The binary matrix of the profile wall.
#' @param thrP a numeric scalar. The x index value of the custom cut of the profile base.
#' @param indx a numeric 1D vector. The y indexes with a value in the custom cut of the profile base.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item df_img_check - Image object, as a result of EBImage formal class Image.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' crop_prof_bip(profile.wall = m19$profile.wall, thrP = 124, indx = c(1:12))
crop_prof_bip = function(profile.wall, thrP, indx){

  # crop the profile at the bottom inner point + 1
  profile.wall[thrP:(dim(profile.wall)[1]),indx] = 0
  df_img_check = EBImage::as.Image(profile.wall)

  returns = list("df_img_check" = df_img_check)

  return(returns)


}



