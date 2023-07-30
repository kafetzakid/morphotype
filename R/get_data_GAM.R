#' Creates the data needed for computing the GAM representation
#'
#' @importFrom EBImage resize
#'
#' @param df_img_check Image object, as a result of EBImage formal class Image.
#' @param h_sc a numeric scalar. The height for re-scaling the profile (in pixels). This should result in a global height for all profiles in the dataset.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item df_img_i_check - a numeric 2D matrix. The data values for the rescaled profile, ranging from zero to one.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' load(file = "~/myRpacks/morphotype/data/df_img_check.RData")
#' get_data_GAM(df_img_check = df_img_check, h_sc = 300)
get_data_GAM = function(df_img_check, h_sc) {

  # Resize the image wrt the scalar for global height
  img_rescaled_check = EBImage::resize(df_img_check, h = h_sc)
  # Get the data
  df_img_i_check = img_rescaled_check@.Data

  returns = list("df_img_i_check" = df_img_i_check)

  return(returns)

}

